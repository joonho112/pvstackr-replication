#!/usr/bin/env Rscript
# =============================================================================
# run_verification.R: Run the full public verification suite
# =============================================================================
#
# Purpose : Orchestrates every public audit component in a fixed order, each in
#           a fresh vanilla Rscript process from the package root, stopping at
#           the first non-zero exit (fail closed). Runs the manifest, scaffold,
#           lockfile, unit and negative tests, simulation and PISA rebuilds,
#           exhibit build, artifact and claim checks, reproduction map, curation
#           ledger, and publication scan, then re-verifies the manifest.
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
old <- setwd(root); on.exit(setwd(old), add = TRUE)
run <- function(path, args = character()) {
  cat("\n== ", path, " ==\n", sep = "")
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", path, args))
  if (!identical(as.integer(status), 0L)) stop("Verification component failed: ", path, call. = FALSE)
}

run("verification/verify_manifest.R")
run("verification/check_scaffold.R")
run("verification/check_lockfile.R")
run("tests/test_core.R")
run("tests/test_verifier_negative.R")
run("code/02_simulation/verify_simulation.R")
run("code/02_simulation/tests/test_simulation.R")
run("code/03_pisa/run_pisa_checks.R")
run("code/04_exhibits/build_all.R")
run("verification/verify_artifacts.R")
run("verification/verify_claims.R")
run("verification/check_reproduction_map.R")
run("verification/check_curation_ledger.R")
run("verification/static_publication_scan.R")
run("verification/verify_manifest.R")
cat("\nALL PUBLIC VERIFICATION CHECKS PASSED.\n")
