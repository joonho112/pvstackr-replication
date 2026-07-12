# =============================================================================
# 06_full_calibrated_runner.R: full Pipeline C stacked fit plus CCC
# =============================================================================
#
# Purpose : Optional full Pipeline C. Re-runs the calibrated stack from
#           scratch: one weighted random-intercept Stan fit per country on the
#           plausible-values-stacked long data (each PV down-weighted by 1/10),
#           then applies the covariance-calibrating change of coordinates so
#           the draws match the Pipeline A pooled target's mean and covariance.
#           Uses the full-run target if present, else the cached target.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   pisa_calibrate_draws() - affine CCC map of draws to the target moments
#   pisa_calibrated_contract() - stacked long data, target, hashes, signature
#   pisa_run_calibrated_country() - sample the stacked fit and calibrate
#   pisa_run_calibrated_all() - run/validate both countries, then publish
# =============================================================================
source(file.path("code", "03_pisa", "04_full_reference_runner.R"))
source(file.path("code", "03_pisa", "05_cached_calibrated.R"))

pisa_calibrate_draws <- function(raw_draws, target_beta, Sigma_target) {
  fe <- c("b_Intercept", "b_ESCS_within", "b_ESCS_between")
  X <- raw_draws[, fe, drop = FALSE]
  target_beta <- as.numeric(target_beta[fe])
  pisa_assert(!anyNA(target_beta), "Target beta is not aligned to fixed effects.")
  R_raw <- chol(stats::cov(X))
  R_target <- chol(Sigma_target)
  A <- solve(R_raw, R_target)
  calibrated_fe <- sweep(sweep(X, 2L, colMeans(X), "-") %*% A,
                         2L, target_beta, "+")
  colnames(calibrated_fe) <- fe
  out <- raw_draws
  out[, fe] <- calibrated_fe
  list(draws = out, A = A, Sigma_raw = stats::cov(X),
       Sigma_target = Sigma_target,
       Sigma_calibrated_empirical = stats::cov(calibrated_fe),
       max_covariance_error = max(abs(stats::cov(calibrated_fe) - Sigma_target)))
}

pisa_calibrated_contract <- function(country, runtime = pisa_full_runtime(),
                                     full_target_file = file.path(PISA_ROOT, "output", "pisa_full", "pipeline_a", "pooled_targets.rds")) {
  analytic <- readRDS(PISA_ANALYTIC_FILE)$data
  d <- analytic[analytic$CNT == country, , drop = FALSE]
  long <- do.call(rbind, lapply(PISA_PV_VARS, function(pv) {
    z <- d; z$.outcome <- z[[pv]]; z$.fractional_weight <- z$w_norm_sample / length(PISA_PV_VARS); z
  }))
  rownames(long) <- NULL
  target_cache <- pisa_load_pipeline_c()$countries[[country]]
  target_source <- "released_pipeline_a_target"
  if (file.exists(full_target_file)) {
    full_target <- readRDS(full_target_file)$countries[[country]]$pooled
    target_cache$target$beta <- full_target$beta
    target_cache$target$T_MI <- full_target$T_MI
    target_source <- "full_pipeline_a_target"
  }
  stan_data <- pisa_prepare_stan_data(long, ".outcome", long$.fractional_weight)
  target_hash <- pisa_hash_object(list(beta = target_cache$target$beta,
                                       T_MI = target_cache$target$T_MI))
  model_hash <- pisa_sha256_file(file.path(PISA_CODE_DIR, "pisa_random_intercept.stan"))
  model_data_hash <- pisa_hash_object(stan_data)
  signature <- pisa_hash_object(list(country = country, seed = target_cache$seed,
                                     runtime = runtime, model_hash = model_hash,
                                     model_data_hash = model_data_hash,
                                     target_hash = target_hash))
  list(data = d, long = long, target = target_cache, stan_data = stan_data,
       target_source = target_source, target_hash = target_hash,
       model_hash = model_hash, model_data_hash = model_data_hash,
       run_signature = signature)
}

pisa_run_calibrated_country <- function(country, model = pisa_cmdstan_model(),
                                        runtime = pisa_full_runtime(),
                                        full_target_file = file.path(PISA_ROOT, "output", "pisa_full", "pipeline_a", "pooled_targets.rds")) {
  contract <- pisa_calibrated_contract(country, runtime, full_target_file)
  d <- contract$data; long <- contract$long; target_cache <- contract$target
  stan_data <- contract$stan_data
  init <- function() list(alpha = weighted.mean(stan_data$y, stan_data$w),
                          beta = c(0, 0), sigma = max(stats::sd(stan_data$y), 1),
                          tau_school = 30, z_school = rep(0, stan_data$J))
  fit <- model$sample(
    data = stan_data, seed = target_cache$seed, chains = runtime$chains,
    parallel_chains = runtime$parallel_chains,
    iter_warmup = runtime$iter_warmup, iter_sampling = runtime$iter_sampling,
    adapt_delta = runtime$adapt_delta, max_treedepth = runtime$max_treedepth,
    init = init, refresh = 100
  )
  raw <- pisa_stan_draws(fit)
  ccc <- pisa_calibrate_draws(raw, target_cache$target$beta,
                              target_cache$target$T_MI)
  list(schema_version = "pisa_pipeline_c_full_country_v1", country = country,
       seed = target_cache$seed, n_rows = nrow(d), n_long = nrow(long),
       n_schools = length(unique(d$CNTSCHID)), raw_draws = raw,
       calibrated_draws = ccc$draws, calibration_matrix = ccc$A,
       Sigma_raw = ccc$Sigma_raw, Sigma_target = ccc$Sigma_target,
       Sigma_calibrated_empirical = ccc$Sigma_calibrated_empirical,
       max_covariance_error = ccc$max_covariance_error,
       diagnostics = pisa_stan_diagnostics(fit, runtime$max_treedepth),
       target_hash = contract$target_hash, target_source = contract$target_source,
       model_source_hash = contract$model_hash,
       model_data_hash = contract$model_data_hash,
       run_signature = contract$run_signature, runtime = runtime)
}

pisa_run_calibrated_all <- function(output_dir = file.path(PISA_ROOT, "output", "pisa_full", "pipeline_c")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  model <- pisa_cmdstan_model()
  runtime <- pisa_full_runtime()
  schedule <- data.frame(country = PISA_COUNTRIES,
                         seed = vapply(pisa_load_pipeline_c()$countries, function(x) x$seed, integer(1)),
                         output_file = paste0(PISA_COUNTRIES, "_stacked_ccc.rds"))
  utils::write.csv(schedule, file.path(output_dir, "task_manifest.csv"), row.names = FALSE)
  for (country in PISA_COUNTRIES) {
    path <- file.path(output_dir, paste0(country, "_stacked_ccc.rds"))
    if (file.exists(path)) {
      old <- tryCatch(readRDS(path), error = function(e) NULL)
      expected <- pisa_calibrated_contract(country, runtime)
      required_draws <- c("b_Intercept", "b_ESCS_within", "b_ESCS_between",
                          "sd_school_id_model__Intercept", "sigma")
      valid <- is.list(old) && identical(old$schema_version, "pisa_pipeline_c_full_country_v1") &&
        identical(old$country, country) && identical(old$runtime, runtime) &&
        identical(old$run_signature, expected$run_signature) &&
        identical(old$target_hash, expected$target_hash) &&
        is.matrix(old$calibrated_draws) && nrow(old$calibrated_draws) > 0L &&
        all(required_draws %in% colnames(old$calibrated_draws)) &&
        all(is.finite(old$calibrated_draws[, required_draws])) &&
        is.list(old$diagnostics) && all(is.finite(unlist(old$diagnostics))) &&
        old$diagnostics$divergences == 0L && old$diagnostics$max_treedepth_hits == 0L
      if (!valid) stop("Existing Pipeline C task is invalid: ", path, call. = FALSE)
      next
    }
    result <- pisa_run_calibrated_country(country, model, runtime)
    staged <- tempfile("pisa-c-", tmpdir = output_dir, fileext = ".rds")
    saveRDS(result, staged, compress = "xz", version = 3)
    if (!file.rename(staged, path)) stop("Could not atomically publish ", path, call. = FALSE)
  }
  utils::write.csv(data.frame(country = PISA_COUNTRIES, status = "complete"),
                   file.path(output_dir, "completion_manifest.csv"), row.names = FALSE)
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  seeds <- vapply(pisa_load_pipeline_c()$countries, function(x) x$seed, integer(1))
  print(data.frame(country = names(seeds), seed = unname(seeds), fits = 1L))
  if ("--run" %in% commandArgs(trailingOnly = TRUE)) pisa_run_calibrated_all()
  else cat("Schedule only. Add --run to execute the two stacked fits and CCC.\n")
}
