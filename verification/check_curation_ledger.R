#!/usr/bin/env Rscript
# =============================================================================
# check_curation_ledger.R: Check curation decisions and public locations
# =============================================================================
#
# Purpose : Reads manifest/curation-ledger.csv and requires unique logical ids
#           with a disposition of include, adapt, reference, or exclude.
#           Confirms every named public location exists and that no row exposes
#           restricted PISA unit records. Fails closed otherwise.
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
x <- utils::read.csv(file.path(root, "manifest", "curation-ledger.csv"), stringsAsFactors = FALSE)
if (anyDuplicated(x$logical_id) || any(!x$disposition %in% c("include", "adapt", "reference", "exclude"))) stop("Invalid curation ledger", call. = FALSE)
public <- x$public_location[nzchar(x$public_location)]
missing <- public[!file.exists(file.path(root, public))]
if (length(missing)) stop("Curation ledger points to missing public locations: ", paste(missing, collapse = ", "), call. = FALSE)
if (any(grepl("data/pisa/local|data/pisa/source|data/pisa/derived", public))) stop("Curation ledger exposes PISA unit records", call. = FALSE)
cat(sprintf("Curation ledger PASS: %d decisions; public locations exist.\n", nrow(x)))
