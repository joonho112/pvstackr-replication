# =============================================================================
# run_pisa_checks.R: run the focused PISA verify_* checks in sequence
# =============================================================================
#
# Purpose : Orchestrates the focused checks - the estimand contract and, when
#           the authorized local slice is present, data/weights, fit/target
#           anchors, the PSIS gate and sensitivity, and public-artifact
#           safety - each in a separate Rscript process, skipping the
#           unit-record checks when the local OECD data is absent. Stops on
#           the first failing check.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))
checks <- c("02_contract.R", if (file.exists(PISA_SOURCE_FILE)) "verify_data_weights.R",
            "verify_fit_targets.R", "verify_gate_sensitivity.R", "verify_public_safety.R")
if (!file.exists(PISA_SOURCE_FILE)) {
  cat("PISA unit-record data checks: SKIPPED (local OECD-authorized data not present)\n")
}
for (check in checks) {
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    file.path("code", "03_pisa", check))
  if (!identical(status, 0L)) stop("PISA check failed: ", check, call. = FALSE)
}
cat("All focused PISA checks: PASS\n")
