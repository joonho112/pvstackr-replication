# =============================================================================
# 05_cached_calibrated.R: validate the cached Pipeline C calibrated stack
# =============================================================================
#
# Purpose : Cached Pipeline C. Reads the curated public cache and verifies,
#           per country, that the covariance-calibrating change of coordinates
#           (CCC) succeeded and that the calibrated fixed-effect draws
#           reproduce the Pipeline A target covariance to tolerance with
#           matching target hashes. Pipeline C is the one-MCMC calibrated
#           primary method. Input: data/precomputed/pisa/pipeline_c_cached.rds.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_load_pipeline_c <- function(tolerance = 1e-10) {
  path <- file.path(PISA_PRECOMPUTED_DIR, "pipeline_c_cached.rds")
  pisa_assert(file.exists(path), "Pipeline C cache is missing.")
  x <- readRDS(path)
  pisa_assert(identical(x$schema_version, "pisa_pipeline_c_public_cache_v1"),
              "Unexpected Pipeline C cache schema.")
  for (country in PISA_COUNTRIES) {
    z <- x$countries[[country]]
    pisa_assert(identical(z$ccc_status, "ok"), paste("CCC failed for", country))
    fe <- c("b_Intercept", "b_ESCS_within", "b_ESCS_between")
    empirical <- stats::cov(z$calibrated_draws[, fe, drop = FALSE])
    delta <- max(abs(empirical - z$Sigma_target))
    pisa_assert(delta <= tolerance,
                sprintf("Calibrated covariance differs from target for %s: %.3e", country, delta))
    pisa_assert(identical(z$target$target_hash,
                          unique(z$rows$target_hash)[[1L]]),
                paste("Target hash is misaligned for", country))
  }
  x
}

if (sys.nframe() == 0L) {
  x <- pisa_load_pipeline_c()
  print(x$rows[, c("country", "term", "estimate", "std_error", "rhat_max")])
}
