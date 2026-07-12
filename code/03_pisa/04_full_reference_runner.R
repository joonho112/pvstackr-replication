# =============================================================================
# 04_full_reference_runner.R: full Pipeline A, 20 weighted Stan fits
# =============================================================================
#
# Purpose : Optional full Pipeline A. Re-runs the reference from scratch:
#           20 weighted random-intercept Stan fits (10 plausible values per
#           country) via cmdstanr, then Rubin-pools each country's per-PV
#           fixed effects into the reference target. Tasks run under a locked
#           seed/runtime/model-hash signature, published atomically; the
#           cached track never invokes --run. Needs local data + toolchain.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   pisa_full_runtime() - sampler settings resolved from env variables
#   pisa_prepare_stan_data() - build the Stan data list for a country slice
#   pisa_stan_draws() - extract fixed effects and variance components
#   pisa_stan_diagnostics() - R-hat, ESS, divergences, treedepth hits
#   pisa_hash_object() - SHA-256 of an R object for run signatures
#   pisa_reference_contract() - per-task data, hashes, run signature
#   pisa_cmdstan_model() - compile the random-intercept Stan model
#   pisa_run_reference_task() - sample one country x plausible-value fit
#   pisa_run_reference_all() - run/validate all 20 tasks, then pool
#   pisa_pool_reference_all() - Rubin-pool per-PV draws to pooled_targets.rds
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))
source(file.path("code", "03_pisa", "03_cached_reference.R"))
source(file.path("code", "01_core", "rubin_pool.R"))

pisa_full_runtime <- function() list(
  iter_sampling = as.integer(Sys.getenv("PISA_ITER_SAMPLING", "500")),
  iter_warmup = as.integer(Sys.getenv("PISA_ITER_WARMUP", "500")),
  chains = as.integer(Sys.getenv("PISA_CHAINS", "2")),
  parallel_chains = as.integer(Sys.getenv("PISA_PARALLEL_CHAINS", "2")),
  adapt_delta = as.numeric(Sys.getenv("PISA_ADAPT_DELTA", "0.95")),
  max_treedepth = as.integer(Sys.getenv("PISA_MAX_TREEDEPTH", "12"))
)

pisa_prepare_stan_data <- function(data, outcome, weight) {
  school <- as.integer(factor(data$school_id_model))
  list(N = nrow(data), J = length(unique(school)), school = school,
       y = as.numeric(data[[outcome]]),
       escs_within = as.numeric(data$ESCS_within),
       escs_between = as.numeric(data$ESCS_between),
       w = as.numeric(weight))
}

pisa_stan_draws <- function(fit) {
  raw <- posterior::as_draws_matrix(fit$draws(
    variables = c("alpha", "beta", "sigma", "tau_school")))
  out <- cbind(
    b_Intercept = raw[, "alpha"],
    b_ESCS_within = raw[, "beta[1]"],
    b_ESCS_between = raw[, "beta[2]"],
    sd_school_id_model__Intercept = raw[, "tau_school"],
    sigma = raw[, "sigma"]
  )
  storage.mode(out) <- "double"
  out
}

pisa_stan_diagnostics <- function(fit, max_treedepth) {
  s <- fit$summary(variables = c("alpha", "beta[1]", "beta[2]", "sigma", "tau_school"))
  d <- as.data.frame(fit$sampler_diagnostics(format = "df"))
  list(rhat_max = max(s$rhat), ess_bulk_min = min(s$ess_bulk),
       ess_tail_min = min(s$ess_tail),
       divergences = sum(d$divergent__ > 0),
       max_treedepth_hits = sum(d$treedepth__ >= max_treedepth))
}

pisa_hash_object <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required.", call. = FALSE)
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

pisa_reference_contract <- function(country, pv_index, runtime = pisa_full_runtime()) {
  analytic <- readRDS(PISA_ANALYTIC_FILE)$data
  d <- analytic[analytic$CNT == country, , drop = FALSE]
  schedule <- pisa_load_pipeline_a()$task_schedule
  task <- schedule[schedule$country == country & schedule$pv_index == pv_index, ]
  pisa_assert(nrow(task) == 1L, "Requested task is absent from the locked schedule.")
  stan_data <- pisa_prepare_stan_data(d, PISA_PV_VARS[[pv_index]], d$w_norm_sample)
  model_hash <- pisa_sha256_file(file.path(PISA_CODE_DIR, "pisa_random_intercept.stan"))
  model_data_hash <- pisa_hash_object(stan_data)
  signature <- pisa_hash_object(list(country = country, pv_index = as.integer(pv_index),
                                     seed = as.integer(task$seed), runtime = runtime,
                                     model_hash = model_hash, model_data_hash = model_data_hash))
  list(data = d, stan_data = stan_data, task = task, model_hash = model_hash,
       model_data_hash = model_data_hash, run_signature = signature)
}

pisa_cmdstan_model <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE)) {
    stop("The full path requires cmdstanr and posterior.", call. = FALSE)
  }
  cmdstanr::cmdstan_model(file.path(PISA_CODE_DIR, "pisa_random_intercept.stan"),
                          force_recompile = FALSE)
}

pisa_run_reference_task <- function(country, pv_index, model = pisa_cmdstan_model(),
                                    runtime = pisa_full_runtime()) {
  contract <- pisa_reference_contract(country, pv_index, runtime)
  d <- contract$data; task <- contract$task; stan_data <- contract$stan_data
  init <- function() list(alpha = weighted.mean(stan_data$y, stan_data$w),
                          beta = c(0, 0), sigma = max(stats::sd(stan_data$y), 1),
                          tau_school = 30, z_school = rep(0, stan_data$J))
  fit <- model$sample(
    data = stan_data, seed = task$seed, chains = runtime$chains,
    parallel_chains = runtime$parallel_chains,
    iter_warmup = runtime$iter_warmup, iter_sampling = runtime$iter_sampling,
    adapt_delta = runtime$adapt_delta, max_treedepth = runtime$max_treedepth,
    init = init, refresh = 100
  )
  list(schema_version = "pisa_pipeline_a_full_task_v1", country = country,
       pv_index = as.integer(pv_index), pv_col = PISA_PV_VARS[[pv_index]],
       seed = as.integer(task$seed), n_rows = nrow(d),
       n_schools = length(unique(d$CNTSCHID)), draws = pisa_stan_draws(fit),
       diagnostics = pisa_stan_diagnostics(fit, runtime$max_treedepth),
       runtime = runtime, model_source_hash = contract$model_hash,
       model_data_hash = contract$model_data_hash,
       run_signature = contract$run_signature)
}

pisa_run_reference_all <- function(output_dir = file.path(PISA_ROOT, "output", "pisa_full", "pipeline_a")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  model <- pisa_cmdstan_model()
  schedule <- pisa_load_pipeline_a()$task_schedule
  runtime <- pisa_full_runtime()
  schedule$output_file <- sprintf("%s_pv%02d.rds", schedule$country, schedule$pv_index)
  utils::write.csv(schedule[, c("country", "pv_index", "pv_col", "seed", "output_file")],
                   file.path(output_dir, "task_manifest.csv"), row.names = FALSE)
  for (i in seq_len(nrow(schedule))) {
    task_path <- file.path(output_dir, schedule$output_file[[i]])
    if (file.exists(task_path)) {
      old <- tryCatch(readRDS(task_path), error = function(e) NULL)
      expected <- pisa_reference_contract(schedule$country[[i]], schedule$pv_index[[i]], runtime)
      required_draws <- c("b_Intercept", "b_ESCS_within", "b_ESCS_between",
                          "sd_school_id_model__Intercept", "sigma")
      valid <- is.list(old) && identical(old$schema_version, "pisa_pipeline_a_full_task_v1") &&
        identical(old$country, schedule$country[[i]]) &&
        identical(as.integer(old$pv_index), as.integer(schedule$pv_index[[i]])) &&
        identical(as.integer(old$seed), as.integer(schedule$seed[[i]])) &&
        identical(old$runtime, runtime) && identical(old$run_signature, expected$run_signature) &&
        is.matrix(old$draws) && nrow(old$draws) > 0L &&
        all(required_draws %in% colnames(old$draws)) && all(is.finite(old$draws[, required_draws])) &&
        is.list(old$diagnostics) && all(is.finite(unlist(old$diagnostics))) &&
        old$diagnostics$divergences == 0L && old$diagnostics$max_treedepth_hits == 0L
      if (!valid) stop("Existing Pipeline A task is invalid: ", task_path, call. = FALSE)
      next
    }
    task <- pisa_run_reference_task(schedule$country[[i]], schedule$pv_index[[i]], model, runtime)
    staged <- tempfile("pisa-a-", tmpdir = output_dir, fileext = ".rds")
    saveRDS(task, staged, compress = "xz", version = 3)
    if (!file.rename(staged, task_path)) stop("Could not atomically publish ", task_path, call. = FALSE)
  }
  pooled <- pisa_pool_reference_all(output_dir)
  invisible(list(schedule = schedule, pooled = pooled))
}

pisa_pool_reference_all <- function(output_dir = file.path(PISA_ROOT, "output", "pisa_full", "pipeline_a")) {
  fe <- c("b_Intercept", "b_ESCS_within", "b_ESCS_between")
  out <- lapply(PISA_COUNTRIES, function(country) {
    paths <- file.path(output_dir, sprintf("%s_pv%02d.rds", country, seq_len(10L)))
    pisa_assert(all(file.exists(paths)), paste("Incomplete full Pipeline A tasks for", country))
    tasks <- lapply(paths, readRDS)
    estimates <- do.call(rbind, lapply(tasks, function(x) colMeans(x$draws[, fe, drop = FALSE])))
    covariances <- lapply(tasks, function(x) stats::cov(x$draws[, fe, drop = FALSE]))
    pooled <- rubin_pool_matrix(estimates, covariances, orientation = "rows_pv",
                                df_method = "classic")
    list(country = country, df_method = "classic", pooled = pooled,
         task_files = basename(paths),
         diagnostics = do.call(rbind, lapply(tasks, function(x) as.data.frame(x$diagnostics))))
  })
  names(out) <- PISA_COUNTRIES
  target_file <- file.path(output_dir, "pooled_targets.rds")
  saveRDS(list(schema_version = "pisa_pipeline_a_full_pooled_v1",
               countries = out), target_file, compress = "xz", version = 3)
  completion <- data.frame(country = PISA_COUNTRIES, n_tasks = 10L,
                           pooled_target = basename(target_file), status = "complete")
  utils::write.csv(completion, file.path(output_dir, "completion_manifest.csv"), row.names = FALSE)
  out
}

if (sys.nframe() == 0L) {
  schedule <- pisa_load_pipeline_a()$task_schedule[, c("country", "pv_index", "pv_col", "seed")]
  print(schedule)
  if ("--run" %in% commandArgs(trailingOnly = TRUE)) pisa_run_reference_all()
  else cat("Schedule only. Add --run to execute all 20 fits.\n")
}
