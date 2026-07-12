# =============================================================================
# calibrated_stack.R: Stacked fractional-posterior engine (single long fit)
# =============================================================================
#
# Purpose : Fit ONE Bayesian multilevel model to all plausible values stacked
#           into a single long data frame, each row weighted by
#           (base weight / mean) / M, so the M plausible values are absorbed
#           in one MCMC run rather than in M separate fits. Returns the
#           posterior-mean-centred fixed-effect draws that the CCC map
#           (ccc.R) later calibrates to the design-based target. Inputs: a
#           wide PV + predictor data frame, an OUTCOME-placeholder formula,
#           and PV / weight column names. Outputs: stacked_draws, log_lik,
#           psi_hat_fe, a fixed / variance-component param_map, diagnostics,
#           and provenance meta.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   stack_fractional_posterior: single long-data fractional fit; returns
#     CCC-ready draws, log_lik, psi_hat_fe, param_map, diagnostics, meta.
#   swl_prepare_fractional_long_data: reshape wide PVs to long and build the
#     internal fractional weights(.swl_w) column.
#   swl_build_brms_fit_args: assemble the protected brms::brm() argument list.
#   swl_default_brms_diagnostics: R-hat, n_eff ratio, divergence counts.
#   swl_reportable_fixed_effect_draws: slice reportable fixed-effect draws.
#   swl_stack_vc_policy / swl_assert_vc_policy_allows: variance-component
#     pass-through policy; VC draws stay diagnostic-only, never reported.
#   swl_hash_long_data: deterministic SHA-256 hash of the long design.
# =============================================================================

# Stacked fractional posterior engine for SWL Paper 2 codebase-v2.
#
# DG-1 implemented topology candidate: one long-data fractional fit. This
# replaces the v1 production loop over PV-specific fits, which conflicted with
# "fit once" wording. DG-1 is not frozen until explicit user signoff is recorded.

`%||%` <- function(x, y) if (is.null(x)) y else x

.swl_ensure_engine_facade <- function() {
  env <- environment(.swl_ensure_engine_facade)
  if (exists("engine_moments", mode = "function", envir = env,
             inherits = TRUE) &&
      exists("engine_loglik", mode = "function", envir = env,
             inherits = TRUE)) {
    return(invisible(TRUE))
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- unique(c(
    if (!is.null(ofile)) file.path(dirname(ofile), "engine_facade.R"),
    file.path(getwd(), "R", "engine_facade.R"),
    file.path(dirname(getwd()), "R", "engine_facade.R")
  ))
  for (path in candidates) {
    if (file.exists(path)) {
      sys.source(path, envir = env)
      break
    }
  }
  if (!exists("engine_moments", mode = "function", envir = env,
              inherits = TRUE) ||
      !exists("engine_loglik", mode = "function", envir = env,
              inherits = TRUE)) {
    stop("[stack_fractional_posterior] source R/engine_facade.R before using the stacked posterior engine",
         call. = FALSE)
  }
  invisible(TRUE)
}

swl_stack_fractional_api_contract <- function() {
  list(
    topology = "single_long_fit",
    dg1_status = "autonomous_recommendation_pending_explicit_user_signoff",
    dg1_freeze_status = "not_frozen_pending_explicit_user_signoff",
    dg1_user_signoff_recorded = FALSE,
    vc_policy = swl_stack_vc_policy(),
    documented_params = c(
      "data", "formula", "pv_cols", "weight_col", "iter", "chains",
      "warmup", "cores", "seed", "prior", "backend", "family",
      "extract_log_lik", "cache_dir", "cache_stem", "fit_function",
      "draws_function", "diagnose_function", "log_lik_function",
      "additional_brms_args", "return_fit", "verbose"
    ),
    documented_return = c(
      "stacked_draws", "diagnostics", "log_lik", "psi_hat_fe",
      "param_map", "formula", "weight_summary", "meta", "fit"
    )
  )
}

swl_stack_vc_policy <- function() {
  list(
    policy_id = "VC-STACK-01",
    status = "nuisance_pass_through_not_calibrated_not_reported",
    calibration_status = "not_calibrated",
    validation_status = "not_validated",
    reporting_status = "not_reported_pending_validation",
    confirmatory_reporting_allowed = FALSE,
    pipeline_b_consumption_allowed = FALSE,
    allowed_current_use = "diagnostic_pass_through_only",
    reportable_parameter_scope = "fixed_effects_only",
    reportable_parameter_regex = "^b_",
    vc_required_before_reporting = "dedicated_vc_validation_or_explicit_user_policy_update",
    required_before_reporting = "dedicated_vc_validation_or_explicit_user_policy_update",
    note = paste(
      "Variance-component posterior draws may be retained in stacked_draws",
      "for diagnostics/pass-through compatibility, but they are not calibrated,",
      "not reportable confirmatory quantities, and not valid Pipeline B inputs."
    )
  )
}

swl_assert_vc_policy_allows <- function(use,
                                        policy = swl_stack_vc_policy()) {
  use <- match.arg(use, c("diagnostic_pass_through",
                         "confirmatory_reporting",
                         "pipeline_b_consumption"))
  if (identical(use, "diagnostic_pass_through")) {
    return(invisible(TRUE))
  }
  if (identical(use, "confirmatory_reporting") &&
      !isTRUE(policy$confirmatory_reporting_allowed)) {
    stop("[swl_stack_vc_policy] variance-component draws are not reportable confirmatory quantities without dedicated validation",
         call. = FALSE)
  }
  if (identical(use, "pipeline_b_consumption") &&
      !isTRUE(policy$pipeline_b_consumption_allowed)) {
    stop("[swl_stack_vc_policy] Pipeline B may not consume unvalidated stacked variance-component posterior draws",
         call. = FALSE)
  }
  invisible(TRUE)
}

swl_reportable_fixed_effect_draws <- function(stack_result) {
  if (!is.list(stack_result) ||
      !is.matrix(stack_result$stacked_draws) ||
      !is.list(stack_result$param_map)) {
    stop("[swl_reportable_fixed_effect_draws] stack_result must come from stack_fractional_posterior",
         call. = FALSE)
  }
  idx <- stack_result$param_map$fe_idx
  if (!length(idx)) {
    stop("[swl_reportable_fixed_effect_draws] no fixed-effect columns available",
         call. = FALSE)
  }
  stack_result$stacked_draws[, idx, drop = FALSE]
}

swl_hash_long_data <- function(data,
                               columns = c(".swl_original_row_id", ".swl_pv_id",
                                           ".swl_y", ".swl_w")) {
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols)) {
    stop("[swl_hash_long_data] missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  payload <- data[columns]
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(payload, algo = "sha256")
  } else {
    paste(utils::capture.output(str(payload)), collapse = "\n")
  }
}

swl_build_brms_fit_args <- function(prepared,
                                    family,
                                    prior,
                                    chains,
                                    iter,
                                    warmup,
                                    cores,
                                    seed,
                                    backend,
                                    cache_dir,
                                    cache_stem,
                                    additional_brms_args = list()) {
  if (!is.list(prepared) || !all(c("formula", "data") %in% names(prepared))) {
    stop("[swl_build_brms_fit_args] prepared must come from swl_prepare_fractional_long_data",
         call. = FALSE)
  }
  if (!is.list(additional_brms_args) ||
      (length(additional_brms_args) && is.null(names(additional_brms_args))) ||
      any(!nzchar(names(additional_brms_args)))) {
    stop("[swl_build_brms_fit_args] additional_brms_args must be a named list",
         call. = FALSE)
  }
  if (length(additional_brms_args) && anyDuplicated(names(additional_brms_args))) {
    stop("[swl_build_brms_fit_args] additional_brms_args must have unique names",
         call. = FALSE)
  }
  protected <- c(
    "formula", "data", "family", "prior", "chains", "iter", "warmup",
    "cores", "seed", "refresh", "silent", "backend", "file", "file_refit"
  )
  overrides <- intersect(names(additional_brms_args), protected)
  if (length(overrides)) {
    stop("[swl_build_brms_fit_args] additional_brms_args may not override protected brms argument(s): ",
         paste(overrides, collapse = ", "), call. = FALSE)
  }
  c(
    list(
      formula = prepared$formula,
      data = prepared$data,
      family = family,
      prior = prior,
      chains = as.integer(chains),
      iter = as.integer(iter),
      warmup = as.integer(warmup),
      cores = as.integer(cores),
      seed = as.integer(seed),
      refresh = 0,
      silent = 2,
      backend = backend,
      file = file.path(cache_dir, cache_stem),
      file_refit = "on_change"
    ),
    additional_brms_args
  )
}

swl_default_brms_diagnostics <- function(fit) {
  diagnostics <- list(fit_class = class(fit))
  if (!requireNamespace("brms", quietly = TRUE)) {
    diagnostics$brms_diagnostics_available <- FALSE
    return(diagnostics)
  }
  diagnostics$brms_diagnostics_available <- TRUE

  rhat <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  if (!is.null(rhat)) {
    vals <- suppressWarnings(as.numeric(unlist(rhat, use.names = FALSE)))
    vals <- vals[is.finite(vals)]
    diagnostics$rhat_max <- if (length(vals)) max(vals) else NA_real_
  }

  neff <- tryCatch(brms::neff_ratio(fit), error = function(e) NULL)
  if (!is.null(neff)) {
    vals <- suppressWarnings(as.numeric(unlist(neff, use.names = FALSE)))
    vals <- vals[is.finite(vals)]
    diagnostics$neff_ratio_min <- if (length(vals)) min(vals) else NA_real_
  }

  nuts <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  if (is.data.frame(nuts) && all(c("Parameter", "Value") %in% names(nuts))) {
    diagnostics$divergences <- sum(nuts$Parameter == "divergent__" &
                                     as.numeric(nuts$Value) == 1,
                                   na.rm = TRUE)
  }
  diagnostics
}

#' Prepare long stacked plausible-value data with fractional likelihood weights
#'
#' @param data Data frame containing all plausible-value columns, predictors,
#'   optional grouping variables, and optionally a base sampling weight column.
#' @param formula Formula template whose left-hand side is the literal
#'   placeholder `OUTCOME`; do not include `weights(...)`.
#' @param pv_cols Character vector of plausible-value columns to stack.
#' @param weight_col Optional base sampling weight column. When supplied, the
#'   base weights are normalized to mean 1 before the `1 / M` fractional
#'   plausible-value scaling is applied.
#' @param outcome_col Internal long-data outcome column name.
#' @param pv_col Internal long-data plausible-value indicator column name.
#' @param row_id_col Internal long-data original-row identifier column name.
#' @param weight_out_col Internal long-data fractional weight column name.
#' @return Named list with components `data`, `formula`, `formula_string`,
#'   `weight_summary`, and `pv_cols`.
swl_prepare_fractional_long_data <- function(data,
                                             formula,
                                             pv_cols,
                                             weight_col = NULL,
                                             outcome_col = ".swl_y",
                                             pv_col = ".swl_pv_id",
                                             row_id_col = ".swl_original_row_id",
                                             weight_out_col = ".swl_w") {
  if (!is.data.frame(data)) {
    stop("[swl_prepare_fractional_long_data] data must be a data.frame", call. = FALSE)
  }
  if (!is.character(pv_cols) || length(pv_cols) < 2L || anyNA(pv_cols)) {
    stop("[swl_prepare_fractional_long_data] pv_cols must name at least two PV columns",
         call. = FALSE)
  }
  if (anyDuplicated(pv_cols)) {
    stop("[swl_prepare_fractional_long_data] pv_cols must be unique", call. = FALSE)
  }
  internal_cols <- c(outcome_col, pv_col, row_id_col, weight_out_col)
  collisions <- intersect(internal_cols, names(data))
  if (length(collisions)) {
    stop("[swl_prepare_fractional_long_data] data already contains reserved internal column(s): ",
         paste(collisions, collapse = ", "), call. = FALSE)
  }
  missing_pv <- setdiff(pv_cols, names(data))
  if (length(missing_pv)) {
    stop("[swl_prepare_fractional_long_data] missing PV column(s): ",
         paste(missing_pv, collapse = ", "), call. = FALSE)
  }
  if (any(vapply(data[pv_cols], function(x) any(!is.finite(x) | is.na(x)), logical(1)))) {
    stop("[swl_prepare_fractional_long_data] PV columns contain missing/non-finite values",
         call. = FALSE)
  }

  formula_str <- paste(deparse(formula, width.cutoff = 500L), collapse = "")
  if (!grepl("OUTCOME", formula_str, fixed = TRUE)) {
    stop("[swl_prepare_fractional_long_data] formula left-hand side must use OUTCOME placeholder",
         call. = FALSE)
  }
  if (grepl("\\|\\s*weights\\s*\\(", formula_str)) {
    stop("[swl_prepare_fractional_long_data] pass an unweighted formula; weights(.swl_w) is constructed internally",
         call. = FALSE)
  }

  lhs_rhs <- strsplit(formula_str, "~", fixed = TRUE)[[1L]]
  if (length(lhs_rhs) < 2L) {
    stop("[swl_prepare_fractional_long_data] formula must contain a right-hand side",
         call. = FALSE)
  }
  if (!identical(trimws(lhs_rhs[[1L]]), "OUTCOME")) {
    stop("[swl_prepare_fractional_long_data] formula left-hand side must be exactly OUTCOME",
         call. = FALSE)
  }
  rhs <- trimws(paste(lhs_rhs[-1L], collapse = "~"))
  M <- length(pv_cols)
  n <- nrow(data)
  w_frac <- 1 / M

  if (!is.null(weight_col)) {
    if (!weight_col %in% names(data)) {
      stop("[swl_prepare_fractional_long_data] weight_col not found: ", weight_col,
           call. = FALSE)
    }
    base_w <- data[[weight_col]]
    if (!is.numeric(base_w) || any(!is.finite(base_w) | is.na(base_w))) {
      stop("[swl_prepare_fractional_long_data] weight_col must be finite numeric",
           call. = FALSE)
    }
    if (any(base_w < 0)) {
      stop("[swl_prepare_fractional_long_data] weight_col contains negative weights",
           call. = FALSE)
    }
    mean_w <- mean(base_w)
    if (!is.finite(mean_w) || mean_w <= 0) {
      stop("[swl_prepare_fractional_long_data] weight_col has non-positive mean",
           call. = FALSE)
    }
    base_w_norm <- base_w / mean_w
    weight_source <- weight_col
  } else {
    base_w_norm <- rep(1, n)
    weight_source <- "constant_fractional"
  }

  long_parts <- lapply(seq_along(pv_cols), function(m) {
    pv_name <- pv_cols[[m]]
    d_m <- data
    d_m[[outcome_col]] <- d_m[[pv_name]]
    d_m[[pv_col]] <- factor(pv_name, levels = pv_cols)
    d_m[[row_id_col]] <- seq_len(n)
    d_m[[weight_out_col]] <- base_w_norm * w_frac
    d_m
  })
  long_data <- do.call(rbind, long_parts)
  rownames(long_data) <- NULL

  formula_weighted <- stats::as.formula(
    sprintf("%s | weights(%s) ~ %s", outcome_col, weight_out_col, rhs),
    env = environment(formula)
  )

  per_pv_weight_sum <- vapply(pv_cols, function(pv_name) {
    sum(long_data[[weight_out_col]][long_data[[pv_col]] == pv_name])
  }, numeric(1))

  list(
    data = long_data,
    formula = formula_weighted,
    formula_string = paste(deparse(formula_weighted, width.cutoff = 500L), collapse = ""),
    weight_summary = list(
      topology = "single_long_fit",
      M = M,
      n_original = n,
      n_long = nrow(long_data),
      fractional_w = w_frac,
      weight_source = weight_source,
      weight_col = weight_col,
      mean_long_weight = mean(long_data[[weight_out_col]]),
      total_long_weight = sum(long_data[[weight_out_col]]),
      per_pv_weight_sum = per_pv_weight_sum,
      long_data_hash = swl_hash_long_data(
        long_data,
        columns = c(row_id_col, pv_col, outcome_col, weight_out_col)
      ),
      long_data_hash_columns = c(row_id_col, pv_col, outcome_col, weight_out_col)
    ),
    pv_cols = pv_cols
  )
}

#' Fit one long-data fractional posterior and return CCC-ready draws
#'
#' @param data Data frame containing all plausible-value columns, predictors,
#'   optional grouping variables, and optionally a base sampling weight column.
#' @param formula Formula template whose left-hand side is the literal
#'   placeholder `OUTCOME`; do not include `weights(...)`.
#' @param pv_cols Character vector of plausible-value columns to stack.
#' @param weight_col Optional base sampling weight column normalized to mean 1
#'   before fractional `1 / M` scaling.
#' @param iter brms iteration count for the single long-data fit.
#' @param chains brms chain count for the single long-data fit.
#' @param warmup brms warmup count; defaults to `iter / 2`.
#' @param cores brms core count.
#' @param seed Integer seed for the single long-data fit.
#' @param prior Optional brms prior specification.
#' @param backend brms backend; default `cmdstanr`.
#' @param family Optional brms family; defaults to `stats::gaussian()` when
#'   production brms fitting is used.
#' @param extract_log_lik Logical; if `TRUE`, extract a single long-data
#'   log-likelihood matrix.
#' @param cache_dir Cache directory for brms file caching.
#' @param cache_stem Cache file stem for the single long-data fit.
#' @param fit_function Optional dependency-injected fit function for tests;
#'   production default is `brms::brm`.
#' @param draws_function Optional dependency-injected draw extractor for tests;
#'   production default is `posterior::as_draws_matrix`.
#' @param diagnose_function Optional diagnostic function called once on the fit.
#' @param log_lik_function Optional log-likelihood extractor for tests; when
#'   omitted and `extract_log_lik = TRUE`, production default is `brms::log_lik`.
#' @param additional_brms_args Named list of extra arguments passed to the fit
#'   function.
#' @param return_fit Logical; if `TRUE`, include the fit object in the output.
#' @param verbose Logical; print progress messages.
#' @return Named list with components `stacked_draws`, `diagnostics`, `log_lik`,
#'   `psi_hat_fe`, `param_map`, `formula`, `weight_summary`, `meta`, and
#'   optionally `fit`.
stack_fractional_posterior <- function(data,
                                       formula,
                                       pv_cols,
                                       weight_col = NULL,
                                       iter = 2000L,
                                       chains = 4L,
                                       warmup = NULL,
                                       cores = 1L,
                                       seed = 42L,
                                       prior = NULL,
                                       backend = "cmdstanr",
                                       family = NULL,
                                       extract_log_lik = FALSE,
                                       cache_dir = "cache",
                                       cache_stem = "swl_stack_fractional",
                                       fit_function = NULL,
                                       draws_function = NULL,
                                       diagnose_function = NULL,
                                       log_lik_function = NULL,
                                       additional_brms_args = list(),
                                       return_fit = FALSE,
                                       verbose = TRUE) {
  .swl_ensure_engine_facade()
  if (is.null(warmup)) warmup <- as.integer(iter / 2L)
  if (!is.numeric(iter) || iter <= warmup) {
    stop("[stack_fractional_posterior] iter must be greater than warmup", call. = FALSE)
  }
  if (!is.numeric(chains) || chains < 1L) {
    stop("[stack_fractional_posterior] chains must be >= 1", call. = FALSE)
  }
  if (!is.list(additional_brms_args) ||
      (length(additional_brms_args) && is.null(names(additional_brms_args))) ||
      any(!nzchar(names(additional_brms_args)))) {
    stop("[stack_fractional_posterior] additional_brms_args must be a named list",
         call. = FALSE)
  }
  if (length(additional_brms_args) && anyDuplicated(names(additional_brms_args))) {
    stop("[stack_fractional_posterior] additional_brms_args must have unique names",
         call. = FALSE)
  }

  prepared <- swl_prepare_fractional_long_data(
    data = data,
    formula = formula,
    pv_cols = pv_cols,
    weight_col = weight_col
  )
  using_default_brms <- is.null(fit_function)
  fit_engine <- if (using_default_brms) "brms::brm" else "injected_fit_function"

  if (using_default_brms) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("[stack_fractional_posterior] brms is required unless fit_function is supplied",
           call. = FALSE)
    }
    fit_function <- brms::brm
    family <- family %||% stats::gaussian()
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  }
  if (using_default_brms && is.null(diagnose_function)) {
    diagnose_function <- swl_default_brms_diagnostics
  }
  if (is.null(draws_function)) {
    if (!requireNamespace("posterior", quietly = TRUE)) {
      stop("[stack_fractional_posterior] posterior is required unless draws_function is supplied",
           call. = FALSE)
    }
    draws_function <- function(fit) posterior::as_draws_matrix(fit)
  }
  if (isTRUE(extract_log_lik) && is.null(log_lik_function)) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("[stack_fractional_posterior] brms is required for log_lik unless log_lik_function is supplied",
           call. = FALSE)
    }
    log_lik_function <- function(fit) as.matrix(brms::log_lik(fit))
  }

  if (verbose) {
    message(sprintf(
      "[stack_fractional_posterior] topology=single_long_fit; M=%d; long rows=%d; total weight=%.6f",
      prepared$weight_summary$M,
      prepared$weight_summary$n_long,
      prepared$weight_summary$total_long_weight
    ))
  }

  fit_args <- swl_build_brms_fit_args(
    prepared = prepared,
    family = family,
    prior = prior,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    backend = backend,
    cache_dir = cache_dir,
    cache_stem = cache_stem,
    additional_brms_args = additional_brms_args
  )
  fit <- do.call(fit_function, fit_args)
  draws <- as.matrix(draws_function(fit))
  if (is.null(colnames(draws))) {
    stop("[stack_fractional_posterior] draw matrix must have column names", call. = FALSE)
  }

  moments <- engine_moments(draws, center = "posterior_mean")
  stacked_draws <- unclass(moments$draws_selected)
  fe_idx <- moments$param_map$fe_idx
  vc_idx <- moments$param_map$vc_idx

  diagnostics <- if (is.null(diagnose_function)) NULL else diagnose_function(fit)
  log_lik <- if (isTRUE(extract_log_lik)) {
    engine_loglik(
      fit = fit,
      extractor = log_lik_function,
      expected_n_draws = nrow(stacked_draws),
      expected_n_obs = prepared$weight_summary$n_long,
      context = "stack_fractional_posterior"
    )
  } else {
    NULL
  }

  out <- list(
    stacked_draws = stacked_draws,
    diagnostics = diagnostics,
    log_lik = log_lik,
    psi_hat_fe = moments$psi_hat_fe,
    param_map = list(
      fe_idx = fe_idx,
      vc_idx = vc_idx,
      fe_names = moments$param_map$fe_names,
      vc_names = moments$param_map$vc_names,
      engine_facade_id = moments$param_map$engine_facade_id,
      vc_policy_id = swl_stack_vc_policy()$policy_id
    ),
    formula = prepared$formula,
    weight_summary = prepared$weight_summary,
    meta = list(
      topology = "single_long_fit",
      engine_id = "single_long_fit",
      engine_facade_id = swl_engine_contract()$facade_id,
      engine_moments_function = "engine_moments",
      engine_loglik_function = if (isTRUE(extract_log_lik)) "engine_loglik" else NA_character_,
      dg1_decision_id = "DG-1",
      dg1_status = "autonomous_recommendation_pending_explicit_user_signoff",
      dg1_signoff_status = "autonomous_recommendation_pending_explicit_user_signoff",
      dg1_freeze_status = "not_frozen_pending_explicit_user_signoff",
      dg1_user_signoff_recorded = FALSE,
      vc_policy_id = swl_stack_vc_policy()$policy_id,
      vc_status = swl_stack_vc_policy()$status,
      vc_calibration_status = swl_stack_vc_policy()$calibration_status,
      vc_validation_status = swl_stack_vc_policy()$validation_status,
      vc_reporting_status = swl_stack_vc_policy()$reporting_status,
      vc_confirmatory_reporting_allowed = swl_stack_vc_policy()$confirmatory_reporting_allowed,
      vc_pipeline_b_consumption_allowed = swl_stack_vc_policy()$pipeline_b_consumption_allowed,
      reportable_parameter_scope = swl_stack_vc_policy()$reportable_parameter_scope,
      reportable_parameter_regex = swl_stack_vc_policy()$reportable_parameter_regex,
      vc_required_before_reporting = swl_stack_vc_policy()$vc_required_before_reporting,
      n_fits = 1L,
      n_model_fits = 1L,
      n_brms_calls = 1L,
      M = prepared$weight_summary$M,
      n_original = prepared$weight_summary$n_original,
      n_long = prepared$weight_summary$n_long,
      long_data_rows = prepared$weight_summary$n_long,
      S_total = nrow(stacked_draws),
      iter = as.integer(iter),
      chains = as.integer(chains),
      warmup = as.integer(warmup),
      seed = as.integer(seed),
      weight_col = weight_col,
      pv_cols = pv_cols,
      fractional_weight_rule = "(base_weight / mean(base_weight)) / M, or 1 / M without base weights",
      long_data_hash = prepared$weight_summary$long_data_hash,
      cache_file = file.path(cache_dir, cache_stem),
      fit_engine = fit_engine
    )
  )
  if (isTRUE(return_fit)) out$fit <- fit
  out
}
