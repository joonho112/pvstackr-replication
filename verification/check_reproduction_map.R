#!/usr/bin/env Rscript
# =============================================================================
# check_reproduction_map.R: Check every output has a producer and rule
# =============================================================================
#
# Purpose : Reads verification/reproduction-map.csv and requires unique target
#           ids with non-empty producer and public-output columns. Confirms
#           every listed producer script and public output exists on disk and
#           that no target is still marked planned. Fails closed otherwise.
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
x <- utils::read.csv(file.path(root, "verification", "reproduction-map.csv"), stringsAsFactors = FALSE)
if (anyDuplicated(x$target_id) || any(!nzchar(x$producer)) || any(!nzchar(x$public_output))) stop("Invalid reproduction map", call. = FALSE)
missing_producer <- x$producer[!file.exists(file.path(root, x$producer))]
missing_output <- x$public_output[!file.exists(file.path(root, x$public_output))]
if (length(missing_producer)) stop("Missing producers: ", paste(unique(missing_producer), collapse = ", "), call. = FALSE)
if (length(missing_output)) stop("Missing outputs: ", paste(unique(missing_output), collapse = ", "), call. = FALSE)
if (any(x$status == "planned")) stop("Reproduction map still contains planned targets", call. = FALSE)
cat(sprintf("Reproduction map PASS: %d targets, no unmapped output.\n", nrow(x)))
