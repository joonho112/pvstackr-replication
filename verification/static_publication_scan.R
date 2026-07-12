#!/usr/bin/env Rscript
# =============================================================================
# static_publication_scan.R: Scan public files for unsafe publication traces
# =============================================================================
#
# Purpose : Enumerates the public tree via tools/public_files.R (excluding this
#           scanner) and inspects text, binary, and serialized files for
#           content that must not ship: disallowed or archive extensions,
#           author-machine absolute paths, credential assignments, private
#           keys, non-project authoring traces, and control characters. Writes
#           verification/reports/publication-scan.csv; fails closed on any hit.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   - Extensions: reject unknown or archive types outside the allowlist.
#   - Text files: per-line scan for absolute paths, credential assignments,
#     private-key headers, non-project authoring traces, control characters.
#   - Binary files: scan printable fragments for credentials, private keys,
#     and authoring traces.
#   - Serialized (RDS) objects: fail-closed recursive walk flagging unreadable
#     or unsupported objects and embedded credential fields, paths, or keys.
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), ".."), mustWork = TRUE)
source(file.path(root, "tools", "public_files.R"), local = FALSE)
files <- file.path(root, pv_public_files(root))
files <- files[basename(files) != "static_publication_scan.R"]

allowed_extensions <- c(
  "r", "csv", "tsv", "md", "qmd", "json", "pdf", "tex", "yml", "yaml",
  "rds", "stan", "cff", "lock", "rproj", "txt", "sh", "png", "jpg",
  "jpeg", "svg", "gitignore", "gitattributes", "bib"
)
extension <- tolower(tools::file_ext(files))
no_extension_allowed <- basename(files) %in% c("LICENSE", ".gitignore",
                                                ".gitattributes")
unknown <- files[!no_extension_allowed & !extension %in% allowed_extensions]
text_ext <- c("R", "r", "md", "qmd", "yml", "yaml", "json", "csv", "txt",
              "stan", "cff", "Rproj", "gitignore", "gitattributes", "lock",
              "bib")
is_text <- tools::file_ext(files) %in% text_ext |
  basename(files) %in% c("LICENSE", ".gitignore", ".gitattributes")

nonproject_products <- c(
  paste0("chat", "gpt"),
  paste0("clau", "de", "([[:space:]_-]+code)?"),
  paste0("cod", "ex"),
  paste0("open", "ai")
)
patterns <- c(
  author_absolute_path = "(/Users/|/home/[A-Za-z0-9._-]+/|[A-Za-z]:\\\\Users\\\\)",
  credential_assignment = "(?i)(api[_-]?key|access[_-]?token|secret[_-]?key|password)[[:space:]]*[:=][[:space:]]*(?:['\"][^'\"]+['\"]|[^[:space:]#,'\"}]+)",
  private_key = "-----BEGIN (RSA |OPENSSH |EC )?PRIVATE KEY-----",
  nonproject_authoring_trace = paste0(
    "(?i)(", paste(nonproject_products, collapse = "|"), ")"
  ),
  control_character = "[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]"
)
credential_field <- "(?i)^(api[_-]?key|access[_-]?token|secret[_-]?key|password)$"
findings <- list()

add_finding <- function(file, rule, line = NA_integer_) {
  findings[[length(findings) + 1L]] <<- data.frame(
    path = substring(file, nchar(root) + 2L), line = line, rule = rule,
    stringsAsFactors = FALSE
  )
}

if (length(unknown)) {
  for (file in unknown) add_finding(file, "forbidden_archive_or_unknown_extension")
}

for (file in files[is_text]) {
  x <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  for (name in names(patterns)) {
    if (name == "author_absolute_path" &&
        basename(file) %in% c("00_config.R", "check_scaffold.R",
                              "check_guide.R")) next
    hit <- grep(patterns[[name]], x, perl = TRUE)
    if (length(hit)) add_finding(file, name, hit)
  }
}

# Inspect printable fragments in every allowlisted non-text payload except RDS,
# which receives a structured recursive scan below.
binary_files <- files[!is_text & tolower(tools::file_ext(files)) != "rds"]
for (file in binary_files) {
  size <- file.info(file)$size
  raw <- tryCatch(readBin(file, what = "raw", n = size),
                  error = function(e) raw())
  if (!length(raw)) next
  bytes <- as.integer(raw)
  bytes[bytes < 32L | bytes > 126L] <- 32L
  printable <- rawToChar(as.raw(bytes))
  for (name in c("credential_assignment", "private_key",
                 "nonproject_authoring_trace")) {
    if (grepl(patterns[[name]], printable, perl = TRUE)) {
      add_finding(file, paste0("binary_", name))
    }
  }
}

# Serialized objects are fail-closed: unreadable objects are findings, and
# character values plus field names are inspected recursively.
walk_rds <- function(x, object_path = "root") {
  out <- list()
  record <- function(rule, where) {
    out[[length(out) + 1L]] <<- data.frame(
      object_path = where, rule = rule, stringsAsFactors = FALSE
    )
  }
  unsupported_type <- typeof(x) %in% c(
    "environment", "externalptr", "weakref", "closure", "builtin",
    "special", "language"
  )
  if (unsupported_type) {
    record(paste0("unsupported_serialized_type_", typeof(x)), object_path)
    return(out)
  }
  if (is.character(x)) {
    for (name in c("author_absolute_path", "credential_assignment",
                   "private_key", "nonproject_authoring_trace")) {
      if (any(grepl(patterns[[name]], x, perl = TRUE))) {
        record(paste0("embedded_", name), object_path)
      }
    }
  }
  attrs <- attributes(x)
  if (length(attrs)) {
    attr_hits <- which(grepl(credential_field, names(attrs), perl = TRUE))
    for (i in attr_hits) {
      record("embedded_credential_attribute",
             paste0(object_path, "@", names(attrs)[[i]]))
    }
    for (i in seq_along(attrs)) {
      child <- walk_rds(attrs[[i]],
                        paste0(object_path, "@", names(attrs)[[i]]))
      if (length(child)) out <- c(out, child)
    }
  }
  if (isS4(x)) {
    slots <- methods::slotNames(x)
    slot_hits <- which(grepl(credential_field, slots, perl = TRUE))
    for (i in slot_hits) {
      record("embedded_credential_slot",
             paste0(object_path, "@", slots[[i]]))
    }
    for (slot in slots) {
      child <- walk_rds(methods::slot(x, slot),
                        paste0(object_path, "@", slot))
      if (length(child)) out <- c(out, child)
    }
  }
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      field_hits <- which(grepl(credential_field, nm, perl = TRUE))
      for (i in field_hits) record("embedded_credential_field", paste0(object_path, "$", nm[[i]]))
    }
    labels <- if (is.null(nm)) as.character(seq_along(x)) else nm
    labels[!nzchar(labels)] <- as.character(which(!nzchar(labels)))
    for (i in seq_along(x)) {
      child <- walk_rds(x[[i]], paste0(object_path, "$", labels[[i]]))
      if (length(child)) out <- c(out, child)
    }
  }
  out
}

rds <- files[tolower(tools::file_ext(files)) == "rds"]
for (file in rds) {
  read_error <- NULL
  obj <- tryCatch(readRDS(file), error = function(e) {
    read_error <<- conditionMessage(e)
    NULL
  })
  if (!is.null(read_error)) {
    add_finding(file, "unreadable_serialized_object")
    next
  }
  hits <- walk_rds(obj)
  if (length(hits)) {
    for (hit in hits) {
      add_finding(file, paste0(hit$rule[[1L]], ":", hit$object_path[[1L]]))
    }
  }
}

result <- if (length(findings)) do.call(rbind, findings) else
  data.frame(path = character(), line = integer(), rule = character())
dir.create(file.path(root, "verification", "reports"), recursive = TRUE,
           showWarnings = FALSE)
utils::write.csv(result,
                 file.path(root, "verification", "reports",
                           "publication-scan.csv"), row.names = FALSE)
if (nrow(result)) {
  stop("Publication scan found ", nrow(result),
       " unsafe candidate(s). See verification/reports/publication-scan.csv",
       call. = FALSE)
}
cat(sprintf("Publication scan PASS: %d files checked; no unsafe findings.\n",
            length(files)))
