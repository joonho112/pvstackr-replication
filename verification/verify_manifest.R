#!/usr/bin/env Rscript
# =============================================================================
# verify_manifest.R: Verify the public file set against the manifest
# =============================================================================
#
# Purpose : Confirms the shipped public tree exactly equals manifest/
#           artifacts.csv. Reads the manifest and the allowed status list, then
#           enforces the column schema, unique logical_id and path keys, valid
#           status values, and 64-hex SHA-256 form. Computes the expected public
#           set via tools/public_files.R and fails closed on any difference in
#           either direction, then checks each file's byte size and SHA-256.
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
path <- file.path(root, "manifest", "artifacts.csv")
if (!file.exists(path)) stop("manifest/artifacts.csv is missing", call. = FALSE)
m <- utils::read.csv(path, stringsAsFactors = FALSE)
required <- c("logical_id", "path", "artifact_type", "schema_version", "producer",
              "source_ids", "sha256", "size_bytes", "rows", "columns", "status", "notes")
if (!identical(names(m), required) || anyDuplicated(m$logical_id) || anyDuplicated(m$path)) stop("Invalid manifest schema or duplicate key", call. = FALSE)
allowed_status <- utils::read.csv(file.path(root, "manifest", "schemas", "status-values.csv"), stringsAsFactors = FALSE)$status
if (any(!m$status %in% allowed_status) || any(!grepl("^[a-f0-9]{64}$", m$sha256))) stop("Invalid manifest status or hash", call. = FALSE)
expected_paths <- pv_public_files(root)
unlisted <- setdiff(expected_paths, m$path)
out_of_boundary <- setdiff(m$path, expected_paths)
if (length(unlisted) || length(out_of_boundary)) {
  stop(sprintf("Manifest boundary mismatch: %d unlisted and %d out-of-boundary files. Unlisted: %s; out-of-boundary: %s",
               length(unlisted), length(out_of_boundary),
               paste(unlisted, collapse = ", "),
               paste(out_of_boundary, collapse = ", ")), call. = FALSE)
}
missing <- m$path[!file.exists(file.path(root, m$path))]
if (length(missing)) stop("Missing manifest files: ", paste(missing, collapse = ", "), call. = FALSE)
size <- file.info(file.path(root, m$path))$size
hash <- vapply(file.path(root, m$path), digest::digest, character(1L), algo = "sha256", file = TRUE)
bad <- m$path[size != m$size_bytes | hash != m$sha256]
if (length(bad)) stop("Manifest mismatch: ", paste(bad, collapse = ", "), call. = FALSE)
cat(sprintf("Manifest verification PASS: %d files.\n", nrow(m)))
