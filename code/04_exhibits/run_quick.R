#!/usr/bin/env Rscript
# =============================================================================
# run_quick.R: entrypoint for the "quick" reproduction track
# =============================================================================
#
# Purpose : Track entrypoint for the quick reproduction. Rebuilds every paper
#           and supplement exhibit from the shipped precomputed evidence by
#           launching code/04_exhibits/build_all.R in a fresh vanilla R
#           subprocess from the package root, and propagates its exit status.
#           Inputs : data/precomputed/** (via build_all.R).
#           Outputs: CSV/TeX tables in output/tables; PDF figures in
#           output/figures.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), "..", ".."), mustWork = TRUE)
old <- setwd(root)
on.exit(setwd(old), add = TRUE)
status <- system2(file.path(R.home("bin"), "Rscript"),
                  c("--vanilla", "code/04_exhibits/build_all.R"))
if (!identical(as.integer(status), 0L)) quit(save = "no", status = status)
cat("Quick track complete. Outputs are in output/tables and output/figures.\n")
