#!/usr/bin/env Rscript
# =============================================================================
# build_manifest.R: Regenerate the public artifact manifest
# =============================================================================
#
# Purpose : Rebuilds manifest/artifacts.csv, the tamper-evidence manifest, for
#           the files returned by pv_public_files() in tools/public_files.R.
#           Each boundary file gets a logical id, path, artifact type, schema
#           version, producer script, source id, SHA-256 digest, byte size,
#           row/column dimensions, status, and notes. Input: package root and
#           its public files; output: artifacts.csv and a stdout count.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), ".."), mustWork = TRUE)
if (!requireNamespace("digest", quietly = TRUE)) stop("digest is required", call. = FALSE)
source(file.path(root, "tools", "public_files.R"), local = FALSE)
files <- pv_public_files(root)
info <- file.info(file.path(root, files))
artifact_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("r", "stan", "sh")) return("code")
  if (ext %in% c("rds", "rdata")) return("data")
  if (ext %in% c("csv", "tsv")) return(if (startsWith(path, "output/tables/")) "table" else "data")
  if (ext %in% c("pdf", "png", "jpg", "jpeg", "svg")) return("figure")
  if (grepl("manifest", path, ignore.case = TRUE)) return("manifest")
  if (startsWith(path, "tests/") || startsWith(path, "verification/")) return("test")
  "document"
}
producer_for <- function(path) {
  if (startsWith(path, "output/")) return("code/04_exhibits/build_all.R")
  if (startsWith(path, "data/precomputed/simulation/")) return("code/02_simulation/build_precomputed_bundle.R")
  if (startsWith(path, "data/precomputed/pisa/")) return("code/03_pisa/08_build_public_evidence.R")
  "manual-curation"
}
source_for <- function(path) {
  if (grepl("simulation", path)) return("SIM-JULY-PARITY")
  if (grepl("pisa", path, ignore.case = TRUE)) return("PISA-PRECOMPUTED-V3")
  if (startsWith(path, "code/01_core/") || startsWith(path, "stan/")) return("ENGINE-V3")
  if (startsWith(path, "output/")) return("EXHIBIT-V5")
  "RELEASE-METADATA"
}
dimensions <- function(path) {
  ext <- tolower(tools::file_ext(path))
  x <- tryCatch({
    if (ext == "csv") utils::read.csv(path, check.names = FALSE) else
      if (ext == "tsv") utils::read.delim(path, check.names = FALSE) else
        if (ext == "rds") readRDS(path) else NULL
  }, error = function(e) NULL)
  if (is.list(x) && !is.data.frame(x) && is.data.frame(x$data)) x <- x$data
  if (is.data.frame(x) || is.matrix(x)) c(nrow(x), ncol(x)) else c(NA_integer_, NA_integer_)
}
dims <- t(vapply(file.path(root, files), dimensions, integer(2L)))
manifest <- data.frame(
  logical_id = sprintf("artifact-%04d", seq_along(files)),
  path = files,
  artifact_type = vapply(files, artifact_type, character(1L)),
  schema_version = "pvstackr-artifact-manifest-v1",
  producer = vapply(files, producer_for, character(1L)),
  source_ids = vapply(files, source_for, character(1L)),
  sha256 = vapply(file.path(root, files), digest::digest, character(1L), algo = "sha256", file = TRUE),
  size_bytes = as.numeric(info$size),
  rows = dims[, 1L],
  columns = dims[, 2L],
  status = ifelse(startsWith(files, "output/"), "derived",
                  ifelse(startsWith(files, "data/precomputed/"), "verified", "source")),
  notes = ifelse(grepl("[.]pdf$", files), "PDF byte metadata normalized after rendering", ""),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(root, "manifest", "artifacts.csv"), row.names = FALSE)
cat(sprintf("Wrote manifest/artifacts.csv with %d files.\n", nrow(manifest)))
