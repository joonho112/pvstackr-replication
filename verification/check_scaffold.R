#!/usr/bin/env Rscript
# =============================================================================
# check_scaffold.R: Check the directory, config, and runner scaffold
# =============================================================================
#
# Purpose : Sources code/helpers/paths.R and confirms the required files and
#           directories exist, nested-root discovery resolves to the package
#           root, and relative-path assertions reject escapes. Exercises
#           run_all.R (help, dry-run, unknown track, invalid config), checks the
#           five track contracts are unique with relative entrypoints, and scans
#           authored scaffold files for author-machine absolute paths.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

check_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) return(normalizePath("verification/check_scaffold.R", mustWork = TRUE))
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/", mustWork = TRUE)
}

root <- normalizePath(file.path(dirname(check_file()), ".."), winslash = "/",
                      mustWork = TRUE)
source(file.path(root, "code", "helpers", "paths.R"), local = FALSE)

checks <- list()
add <- function(name, pass, detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    pass = isTRUE(pass),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

required_files <- c(
  ".gitignore", ".gitattributes", "pvstackr-replication.Rproj",
  "config/defaults.yml", "code/helpers/paths.R", "code/00_setup.R",
  "docs/reproduction-tracks.md", "docs/system-requirements.md",
  "manifest/README.md", "manifest/schemas/artifact-fields.csv",
  "manifest/schemas/status-values.csv", "run_all.R",
  "verification/check_scaffold.R"
)
for (path in required_files) {
  add(paste0("file:", path), file.exists(file.path(root, path)), path)
}

required_dirs <- c(
  "code/01_core", "code/02_simulation", "code/03_pisa", "code/04_exhibits",
  "data/pisa", "data/precomputed",
  "output/figures", "output/tables", "output/results",
  "tests", "book", ".github/workflows"
)
for (path in required_dirs) {
  add(paste0("dir:", path), dir.exists(file.path(root, path)), path)
}

found_root <- tryCatch(pv_find_root(file.path(root, "code", "helpers")),
                       error = identity)
add("nested root discovery", is.character(found_root) && identical(found_root, root),
    if (is.character(found_root)) found_root else conditionMessage(found_root))

path_escape <- tryCatch({
  pv_assert_relative_path("../outside")
  FALSE
}, error = function(e) TRUE)
add("path escape rejected", path_escape, "../outside")

contracts <- pv_track_contracts()
add("five unique tracks",
    nrow(contracts) == 5L && !anyDuplicated(contracts$track),
    paste(contracts$track, collapse = ","))
add("entrypoints are relative",
    all(vapply(contracts$entrypoint, function(x) {
      tryCatch({ pv_assert_relative_path(x); TRUE }, error = function(e) FALSE)
    }, logical(1L))),
    paste(contracts$entrypoint, collapse = ";"))

rscript <- file.path(R.home("bin"), "Rscript")
run <- function(args) {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2(rscript, c("--vanilla", "run_all.R", args),
                                  stdout = TRUE, stderr = TRUE))
  list(output = out, status = attr(out, "status") %||% 0L)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

help <- run("--help")
add("runner help", identical(as.integer(help$status), 0L) &&
      any(grepl("Reproduction|replication runner", help$output)),
    paste(help$output, collapse = " | "))

dry <- run(c("--track", "quick", "--dry-run"))
add("runner dry-run", identical(as.integer(dry$status), 0L) &&
      any(grepl("Entrypoint: code/04_exhibits/run_quick.R", dry$output, fixed = TRUE)),
    paste(dry$output, collapse = " | "))

bad <- run(c("--track", "not-a-track", "--dry-run"))
add("unknown track fails", as.integer(bad$status) != 0L,
    paste(bad$output, collapse = " | "))

bad_config_path <- file.path(root, "config", ".negative-invalid.yml")
writeLines(c("schema_version: wrong", "tracks: {}"), bad_config_path)
bad_config <- run(c("--track", "quick", "--config",
                    "config/.negative-invalid.yml", "--dry-run"))
unlink(bad_config_path)
add("invalid config fails", as.integer(bad_config$status) != 0L,
    paste(bad_config$output, collapse = " | "))

authored <- file.path(root, setdiff(required_files, "verification/check_scaffold.R"))
lines <- unlist(lapply(authored[file.exists(authored)], readLines, warn = FALSE,
                       encoding = "UTF-8"), use.names = FALSE)
absolute_marker <- grepl("/Users/", lines, fixed = TRUE)
add("no author-machine absolute paths", !any(absolute_marker),
    paste0("matches=", sum(absolute_marker)))

result <- do.call(rbind, checks)
print(result, row.names = FALSE)
if (any(!result$pass)) {
  cat("\nScaffold check FAILED: ", sum(!result$pass), " check(s).\n", sep = "")
  quit(save = "no", status = 1L, runLast = FALSE)
}
cat("\nScaffold check PASS: ", nrow(result), " checks.\n", sep = "")
