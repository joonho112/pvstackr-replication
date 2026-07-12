# =============================================================================
# seed_protocol.R: Separated DGM and MCMC seed contract
# =============================================================================
#
# Purpose : Define and enforce the two separated random-seed roots used by the
#           evidence run: the preregistered data-generating root and the fixed
#           MCMC root. Exposes SIM_DGM_SEED_ROOT and SIM_MCMC_SEED_ROOT, a
#           machine-readable seed contract (DGM roles and MCMC policy), and an
#           assertion that the two roots stay distinct so sampler choices can
#           never change the generated population, sample, or plausible values.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
# Seed contract used by the July 2026 evidence run.
#
# The data-generating substrate uses the preregistered seed family rooted at
# 20260514.  The MCMC calls in the July parity workers use the separate fixed
# root 20260601.  Keeping these roots distinct prevents sampler choices from
# changing the generated population, sample, or plausible values.

SIM_DGM_SEED_ROOT <- 20260514L
SIM_MCMC_SEED_ROOT <- 20260601L

simulation_seed_contract <- function() {
  list(
    dgm_seed_root = SIM_DGM_SEED_ROOT,
    mcmc_seed_root = SIM_MCMC_SEED_ROOT,
    dgm_roles = c("population", "school_sample", "student_sample", "pv",
                  "nonresponse", "weight_trim", "rep_weights", "brr"),
    mcmc_policy = paste(
      "Per-PV fits use 20260601 + PV index; the calibrated-stack fit uses",
      "20260601. Each independent worker fits chains serially."
    ),
    shared_substrate_across_estimators = TRUE
  )
}

assert_simulation_seed_contract <- function() {
  contract <- simulation_seed_contract()
  if (!identical(contract$dgm_seed_root, 20260514L) ||
      !identical(contract$mcmc_seed_root, 20260601L) ||
      identical(contract$dgm_seed_root, contract$mcmc_seed_root)) {
    stop("Simulation DGM and MCMC seed roots are not correctly separated.",
         call. = FALSE)
  }
  invisible(TRUE)
}
