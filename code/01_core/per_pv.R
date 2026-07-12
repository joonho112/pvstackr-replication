# =============================================================================
# per_pv.R: Per-plausible-value pooling interface
# =============================================================================
#
# Purpose : Shared entry point for the per-PV "analyze then pool" path used by
#           both the simulation and PISA workflows. pool_per_pv takes an
#           M-by-p matrix of per-PV point estimates and their covariances and
#           applies Rubin's rules via rubin_pool_matrix (rubin_pool.R).
#           per_pv_task_grid enumerates the country-by-PV task grid (with a
#           task_id) that drives the per-PV fits. Output: a pooled-result
#           list, or a task-grid data frame.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

pool_per_pv <- function(estimates, covariances, df_method = "classic",
                        df_complete = NULL, conf_level = 0.95) {
  if (!is.matrix(estimates) || nrow(estimates) < 2L) {
    stop("estimates must be an M by p numeric matrix with M >= 2", call. = FALSE)
  }
  rubin_pool_matrix(
    beta = estimates,
    U = covariances,
    orientation = "rows_pv",
    conf_level = conf_level,
    df_method = df_method,
    df_complete = df_complete
  )
}

per_pv_task_grid <- function(countries, pv_names) {
  out <- expand.grid(country = countries, pv = pv_names,
                     stringsAsFactors = FALSE)
  out$task_id <- sprintf("%s-%s", out$country, out$pv)
  out[, c("task_id", "country", "pv")]
}
