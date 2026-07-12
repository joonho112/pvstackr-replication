# =============================================================================
# backends.R: model-fitting backend selection for full-replay tracks
# =============================================================================
#
# Purpose : Probe and select the Stan backend used by the simulation and PISA
#           full-replay tracks. Reports whether cmdstanr and a built CmdStan
#           installation are available (optionally erroring when a full replay
#           requires them) and resolves a per-model-hash Stan cache directory
#           under output/cache/cmdstan.
#           Inputs : a model hash; a require-install flag.
#           Outputs: a backend-availability list; a created cache directory.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
check_cmdstan_backend <- function(require_install = FALSE) {
  available <- requireNamespace("cmdstanr", quietly = TRUE)
  path <- if (available) tryCatch(cmdstanr::cmdstan_path(), error = function(e) "") else ""
  installed <- nzchar(path) && dir.exists(path)
  if (require_install && !installed) {
    stop("CmdStan is required for this full replay track. See docs/getting-started.md.", call. = FALSE)
  }
  list(cmdstanr_available = available, cmdstan_installed = installed,
       cmdstan_path = if (installed) path else NA_character_)
}

stan_cache_dir <- function(model_hash) {
  dir <- project_path("output", "cache", "cmdstan", model_hash)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
