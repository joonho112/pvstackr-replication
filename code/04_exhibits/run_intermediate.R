#!/usr/bin/env Rscript
# =============================================================================
# run_intermediate.R: entrypoint for the "intermediate" reproduction track
# =============================================================================
#
# Purpose : Track entrypoint for the intermediate reproduction. Recomputes
#           pooled/calibrated summaries from shipped objects and then rebuilds
#           the exhibits. Runs, each in a fresh vanilla R subprocess from the
#           package root: the core tests (tests/test_core.R), the simulation
#           aggregation (code/02_simulation/aggregate.R), the optional PISA
#           public-evidence verification, and code/04_exhibits/build_all.R.
#           Inputs : shipped precomputed objects and test fixtures.
#           Outputs: refreshed output/tables and output/figures.
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
run <- function(path, args = character()) {
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", path, args))
  if (!identical(as.integer(status), 0L)) {
    stop("Intermediate component failed: ", path, " (status ", status, ")", call. = FALSE)
  }
}
run("tests/test_core.R")
run("code/02_simulation/aggregate.R")
if (file.exists(file.path(root, "code", "03_pisa", "10_verify_public_evidence.R"))) {
  run("code/03_pisa/10_verify_public_evidence.R")
}
run("code/04_exhibits/build_all.R")
cat("Intermediate track complete.\n")
