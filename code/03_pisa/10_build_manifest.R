# =============================================================================
# 10_build_manifest.R: build the deterministic public artifact manifest
# =============================================================================
#
# Purpose : Stage 10. Enumerates every curated file under the public
#           precomputed directory and records each one's repository-relative
#           path, SHA-256, byte size, format, row/column shape, and object
#           schema, writing the manifest as CSV and JSON. Asserts that every
#           listed path is portable (no absolute or user paths). Provides the
#           integrity index that the public-safety check verifies against.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

manifest_targets <- c(
  sort(list.files("data/precomputed/pisa", full.names = TRUE))
)
manifest_targets <- manifest_targets[file.exists(manifest_targets)]
manifest_targets <- manifest_targets[!grepl("public_manifest\\.(csv|json)$", manifest_targets)]

describe_file <- function(path) {
  ext <- tools::file_ext(path)
  rows <- columns <- NA_integer_
  object_schema <- ""
  if (ext == "csv") {
    x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    rows <- nrow(x); columns <- ncol(x)
  } else if (ext == "rds") {
    x <- readRDS(path)
    if (is.data.frame(x)) { rows <- nrow(x); columns <- ncol(x) }
    if (is.list(x) && !is.null(x$schema_version)) object_schema <- x$schema_version
    if (is.list(x) && is.data.frame(x$data)) {
      rows <- nrow(x$data); columns <- ncol(x$data)
    }
  }
  data.frame(
    logical_id = sub("[^A-Za-z0-9]+", "_", sub("\\.[^.]+$", "", path)),
    path = path,
    sha256 = pisa_sha256_file(path),
    bytes = unname(file.info(path)$size),
    format = ext,
    rows = rows,
    columns = columns,
    object_schema = object_schema,
    status = "public_curated",
    stringsAsFactors = FALSE
  )
}

manifest <- do.call(rbind, lapply(manifest_targets, describe_file))
pisa_assert(all(vapply(manifest$path, pisa_safe_relative_path, logical(1))),
            "Manifest contains an unsafe path.")
utils::write.csv(manifest, file.path(PISA_PRECOMPUTED_DIR, "public_manifest.csv"),
                 row.names = FALSE, na = "")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.", call. = FALSE)
jsonlite::write_json(
  list(schema_version = "pisa_public_manifest_v1",
       source_sha256 = PISA_SOURCE_SHA256,
       files = unname(split(manifest, seq_len(nrow(manifest))))),
  file.path(PISA_PRECOMPUTED_DIR, "public_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null", digits = NA
)
cat(sprintf("PISA public manifest: %d files\n", nrow(manifest)))
