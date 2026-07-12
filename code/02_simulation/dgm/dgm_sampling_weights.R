# Survey sampling and weight DGM for Phase 4 Step 4.1a.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

.swg_digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(x, algo = "sha256")
  } else {
    paste(utils::capture.output(str(x)), collapse = "\n")
  }
}

.swg_with_seed <- function(seed, code) {
  if (is.null(seed)) return(code())
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed)) {
    stop("[dgm_sampling_weights] seed must be one finite integer-like value",
         call. = FALSE)
  }
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (old_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  code()
}

.swg_check_positive_finite <- function(x, label, allow_zero = FALSE) {
  x <- as.numeric(x)
  bad <- anyNA(x) || any(!is.finite(x)) ||
    if (isTRUE(allow_zero)) any(x < 0) else any(x <= 0)
  if (bad) {
    stop(sprintf("[dgm_sampling_weights] %s must be finite and %s",
                 label, if (allow_zero) "non-negative" else "strictly positive"),
         call. = FALSE)
  }
  x
}

.swg_check_pi <- function(x, label) {
  x <- as.numeric(x)
  if (anyNA(x) || any(!is.finite(x)) || any(x <= 0) || any(x > 1)) {
    stop(sprintf("[dgm_sampling_weights] %s must satisfy 0 < pi <= 1", label),
         call. = FALSE)
  }
  x
}

swg_inclusion_probability_semantics <- function() {
  data.frame(
    field = c("pi_school", "pi_student_cond",
              "w_school_raw", "w_student_cond_raw"),
    semantics = c(
      "nominal_selection_propensity_not_exact_design_inclusion_probability",
      "nominal_selection_propensity_not_exact_design_inclusion_probability",
      "inverse_nominal_selection_propensity",
      "inverse_nominal_selection_propensity"
    ),
    exact_design_pi_claim_allowed = FALSE,
    exact_pi_validation_required_for_exact_claim = TRUE,
    current_generator_note = paste(
      "Current without-replacement PPS/probability sampling stores nominal",
      "selection propensities. Exact design inclusion-probability claims",
      "require a future known-pi/calibrated design and Monte Carlo or",
      "analytic validation before informative-weight performance evidence."
    ),
    stringsAsFactors = FALSE
  )
}

.swg_scale <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  as.numeric((x - mean(x)) / s)
}

.swg_design_value <- function(design_row, name, default = NULL) {
  if (is.null(design_row)) return(default)
  if (is.list(design_row) && !is.null(design_row[[name]])) {
    val <- design_row[[name]]
    if (length(val) == 1L && !is.na(val)) return(val)
  }
  default
}

.swg_required_population_cols <- function() {
  c("school_id", "student_id", "N_j", "xbar_j", "zeta_j", "x_ij", "theta")
}

.swg_population_preflight <- function(population) {
  if (!is.data.frame(population)) {
    stop("[sample_survey_design] population must be a data.frame",
         call. = FALSE)
  }
  missing <- setdiff(.swg_required_population_cols(), names(population))
  if (length(missing)) {
    stop("[sample_survey_design] population missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyNA(population$school_id) || anyNA(population$student_id) ||
      any(!nzchar(as.character(population$school_id))) ||
      any(!nzchar(as.character(population$student_id)))) {
    stop("[sample_survey_design] school_id and student_id must be non-missing",
         call. = FALSE)
  }
  if (any(duplicated(population$student_id))) {
    stop("[sample_survey_design] student_id must be unique in the population",
         call. = FALSE)
  }
  population
}

swg_row_support_hash <- function(data) {
  if (!all(c("school_id", "student_id") %in% names(data))) {
    stop("[swg_row_support_hash] data needs school_id and student_id",
         call. = FALSE)
  }
  .swg_digest(paste(as.character(data$school_id),
                    as.character(data$student_id), sep = "::"))
}

swg_weight_diagnostics <- function(weights) {
  w <- .swg_check_positive_finite(weights, "weights")
  ess <- sum(w)^2 / sum(w^2)
  w_sorted <- sort(w / sum(w), decreasing = TRUE)
  list(
    weight_cv = if (length(w) > 1L) stats::sd(w) / mean(w) else 0,
    weight_ess = ess,
    weight_ess_ratio = ess / length(w),
    weight_max_to_median = max(w) / stats::median(w),
    weight_top1_share = sum(head(w_sorted, max(1L, ceiling(0.01 * length(w))))),
    weight_top5_share = sum(head(w_sorted, max(1L, ceiling(0.05 * length(w)))))
  )
}

.swg_recompute_sns01 <- function(data,
                                 emit_population_weights = FALSE,
                                 population_total = NA_real_) {
  data$w_school_raw <- .swg_check_positive_finite(data$w_school_raw,
                                                  "w_school_raw")
  data$w_student_cond_raw <- .swg_check_positive_finite(
    data$w_student_cond_raw, "w_student_cond_raw"
  )
  data$w_final_raw <- data$w_school_raw * data$w_student_cond_raw
  by_school <- tapply(data$w_school_raw, data$school_id, function(x) {
    if (length(unique(x)) != 1L) {
      stop("[sample_survey_design] w_school_raw must be constant within school",
           call. = FALSE)
    }
    x[[1L]]
  })
  mean_school_raw <- mean(by_school)
  c_A <- nrow(data) / sum(data$w_final_raw)
  data$w_school <- data$w_school_raw / mean_school_raw
  data$w_student_cond <- c_A * mean_school_raw * data$w_student_cond_raw
  data$w_final <- data$w_school * data$w_student_cond
  data$w_norm_sample <- data$w_final
  data$w_norm_population <- if (isTRUE(emit_population_weights)) {
    target_total <- if (is.finite(population_total)) population_total else
      sum(data$w_final_raw)
    target_total * data$w_final_raw / sum(data$w_final_raw)
  } else {
    NA_real_
  }
  data
}

.swg_apply_nonresponse <- function(sample,
                                   nonresponse_model,
                                   seed = NULL,
                                   target_response_rate = 0.90) {
  if (identical(nonresponse_model, "none")) {
    sample$response_prob <- 1
    sample$response_indicator <- TRUE
    return(sample)
  }
  if (!identical(nonresponse_model, "logistic_x_xbar")) {
    stop("[sample_survey_design] unsupported nonresponse_model",
         call. = FALSE)
  }
  .swg_with_seed(seed, function() {
    eta <- stats::qlogis(target_response_rate) -
      0.25 * .swg_scale(sample$x_ij) - 0.20 * .swg_scale(sample$xbar_j)
    response_prob <- pmin(0.99, pmax(0.05, stats::plogis(eta)))
    response_indicator <- stats::runif(nrow(sample)) <= response_prob
    by_school <- split(seq_len(nrow(sample)), sample$school_id)
    for (idx in by_school) {
      if (!any(response_indicator[idx])) {
        keep <- idx[which.max(response_prob[idx])]
        response_indicator[keep] <- TRUE
      }
    }
    if (!any(response_indicator)) {
      stop("[sample_survey_design] nonresponse model produced zero respondents",
           call. = FALSE)
    }
    sample$response_prob <- response_prob
    sample$response_indicator <- response_indicator
	    sample$response_rate_raw <- mean(response_indicator)
	    sample <- sample[response_indicator, , drop = FALSE]
	    sample$w_student_cond_raw <- sample$w_student_cond_raw / sample$response_prob
	    sample$pi_student_cond <- 1 / sample$w_student_cond_raw
	    sample
	  })
	}

.swg_apply_trimming <- function(sample, trim_rule) {
  sample$trim_threshold <- NA_real_
  sample$trimmed <- FALSE
  if (identical(trim_rule, "none")) return(sample)
  if (!identical(trim_rule, "q99_by_cell")) {
    stop("[sample_survey_design] unsupported trim_rule", call. = FALSE)
  }
  raw_final <- sample$w_school_raw * sample$w_student_cond_raw
  cap <- as.numeric(stats::quantile(raw_final, probs = 0.99,
                                    names = FALSE, type = 8))
  sample$trim_threshold <- cap
  sample$trimmed <- raw_final > cap
	  if (any(sample$trimmed)) {
	    final_trimmed <- pmin(raw_final, cap)
	    sample$w_student_cond_raw <- final_trimmed / sample$w_school_raw
	    sample$pi_student_cond <- 1 / sample$w_student_cond_raw
	  }
	  sample
	}

sample_survey_design <- function(population,
                                 design_row = NULL,
                                 seed = NULL,
                                 J = NULL,
                                 nbar = NULL,
                                 weight_info = NULL,
                                 sampling_design = NULL,
                                 nonresponse_model = NULL,
                                 trim_rule = NULL,
                                 weight_scaling_rule = NULL,
                                 emit_population_weights = FALSE) {
  population <- .swg_population_preflight(population)
  J <- as.integer(J %||% .swg_design_value(design_row, "J", NULL))
  nbar <- as.integer(nbar %||% .swg_design_value(design_row, "nbar", NULL) %||%
                       .swg_design_value(design_row, "n_bar_j", NULL))
  if (is.na(J) || J < 2L || is.na(nbar) || nbar < 1L) {
    stop("[sample_survey_design] J and nbar must be positive integer inputs",
         call. = FALSE)
  }
  weight_info <- as.character(weight_info %||%
                                .swg_design_value(design_row, "weight_info", "none"))
  sampling_design <- as.character(sampling_design %||%
                                    .swg_design_value(design_row, "sampling_design",
                                                      "two_stage_pps_srs"))
  nonresponse_model <- as.character(nonresponse_model %||%
                                      .swg_design_value(design_row,
                                                        "nonresponse_model",
                                                        "none"))
  trim_rule <- as.character(trim_rule %||%
                              .swg_design_value(design_row, "trim_rule", "none"))
  weight_scaling_rule <- as.character(weight_scaling_rule %||%
                                        .swg_design_value(
                                          design_row,
                                          "weight_scaling_rule",
                                          "sns01_stagewise_sample_normalized"
                                        ))
  if (!identical(weight_scaling_rule, "sns01_stagewise_sample_normalized")) {
    stop("[sample_survey_design] confirmatory weight_scaling_rule must be sns01_stagewise_sample_normalized",
         call. = FALSE)
  }
  if (!weight_info %in% c("none", "pisa_like")) {
    stop("[sample_survey_design] weight_info must be none or pisa_like",
         call. = FALSE)
  }
  if (!sampling_design %in% c("two_stage_pps_srs", "two_stage_pps_informative")) {
    stop("[sample_survey_design] unsupported sampling_design", call. = FALSE)
  }

  .swg_with_seed(seed, function() {
    school_frame <- aggregate(
      cbind(N_j = population$N_j, xbar_j = population$xbar_j,
            zeta_j = population$zeta_j),
      by = list(school_id = population$school_id),
      FUN = function(x) x[[1L]]
    )
    J_pop <- nrow(school_frame)
    if (J_pop < 4L * J) {
      stop("[sample_survey_design] finite population must have J_pop >= 4 * J",
           call. = FALSE)
    }
    mos <- as.numeric(school_frame$N_j)
    if (identical(weight_info, "pisa_like")) {
      mos <- mos * exp(0.25 * .swg_scale(school_frame$xbar_j) +
                         0.20 * .swg_scale(school_frame$zeta_j))
    }
    pi_school <- J * mos / sum(mos)
    pi_school <- .swg_check_pi(pi_school, "pi_school")
    selected_school <- sample(school_frame$school_id, size = J, replace = FALSE,
                              prob = mos)
    school_frame$pi_school <- pi_school
    school_frame$w_school_raw <- 1 / school_frame$pi_school
    school_frame$sampled_school <- school_frame$school_id %in% selected_school

    pieces <- lapply(selected_school, function(sid) {
      rows <- population[population$school_id == sid, , drop = FALSE]
      N <- nrow(rows)
      n_j <- min(nbar, N)
      if (n_j < 1L) {
        stop("[sample_survey_design] selected school has no students",
             call. = FALSE)
      }
      if (identical(weight_info, "pisa_like")) {
        student_score <- exp(0.30 * .swg_scale(rows$x_ij))
      } else {
        student_score <- rep(1, N)
      }
      pi_student <- n_j * student_score / sum(student_score)
      pi_student <- .swg_check_pi(pi_student, "pi_student_cond")
      idx <- sample(seq_len(N), size = n_j, replace = FALSE,
                    prob = student_score)
      out <- rows[idx, , drop = FALSE]
      out$pi_student_cond <- pi_student[idx]
      out$w_student_cond_raw <- 1 / out$pi_student_cond
      out
    })
    sample <- do.call(rbind, pieces)
    rownames(sample) <- NULL

    sf <- school_frame[match(sample$school_id, school_frame$school_id), ]
	    sample$pi_school <- sf$pi_school
	    sample$w_school_raw <- sf$w_school_raw
	    sample$pi_student_cond_base <- sample$pi_student_cond
	    sample$w_student_cond_raw_base <- sample$w_student_cond_raw
	    sample$sampled_school <- TRUE
    sample <- .swg_apply_nonresponse(sample, nonresponse_model,
                                     seed = if (is.null(seed)) NULL else seed + 101L)
    sample <- .swg_apply_trimming(sample, trim_rule)
    sample <- .swg_recompute_sns01(
      sample,
      emit_population_weights = emit_population_weights,
      population_total = nrow(population)
    )
    sample$weight_scaling_rule <- weight_scaling_rule
    sample$weight_info <- weight_info
    sample$sampling_design <- sampling_design
    sample$nonresponse_model <- nonresponse_model
    sample$trim_rule <- trim_rule
    sample$school_selection_propensity <- sample$pi_school
    sample$student_selection_propensity <- sample$pi_student_cond
    sample$inclusion_probability_semantics <-
      "nominal_selection_propensity_not_exact_design_inclusion_probability"
    sample$exact_inclusion_probability_validated <- FALSE
    sample$x <- sample$x_ij
    sample$school_weight_raw <- sample$w_school_raw
    sample$student_cond_weight_raw <- sample$w_student_cond_raw
    sample$row_id <- paste(sample$school_id, sample$student_id, sep = "::")
    sample$row_support_hash <- swg_row_support_hash(sample)

    school_sums <- aggregate(w_school ~ school_id, sample, function(x) x[[1L]])
    diagnostics <- swg_weight_diagnostics(sample$w_final)
    metadata <- c(list(
      weight_version = "step4.1a_sns01_v1",
      weight_scaling_rule = weight_scaling_rule,
      weight_info = weight_info,
      sampling_design = sampling_design,
      nonresponse_model = nonresponse_model,
      trim_rule = trim_rule,
      inclusion_probability_semantics =
        "nominal_selection_propensity_not_exact_design_inclusion_probability",
      exact_inclusion_probability_validated = FALSE,
      exact_inclusion_probability_claim_allowed = FALSE,
      J_pop = J_pop,
      J_sample = length(unique(sample$school_id)),
      n_sample = nrow(sample),
      row_support_hash = unique(sample$row_support_hash)[[1L]],
      weight_metadata_hash = .swg_digest(sample[, c(
        "school_id", "student_id", "pi_school", "pi_student_cond",
        "w_school_raw", "w_student_cond_raw", "w_final_raw",
        "w_school", "w_student_cond", "w_final", "w_norm_sample"
      ), drop = FALSE]),
      sum_school_w = sum(school_sums$w_school),
      sum_w_norm_sample = sum(sample$w_norm_sample),
      max_staged_reconstruction_error = max(abs(sample$w_final -
                                                   sample$w_school *
                                                   sample$w_student_cond)),
      max_raw_reconstruction_error = max(abs(sample$w_final_raw -
                                                 sample$w_school_raw *
                                                 sample$w_student_cond_raw)),
      response_rate = unique(sample$response_rate_raw %||% 1)[[1L]],
      trim_rate = mean(sample$trimmed),
      corr_w_school_raw_xbar = suppressWarnings(stats::cor(
        unique(sample[c("school_id", "w_school_raw", "xbar_j")])$w_school_raw,
        unique(sample[c("school_id", "w_school_raw", "xbar_j")])$xbar_j
      )),
      corr_w_school_raw_zeta = suppressWarnings(stats::cor(
        unique(sample[c("school_id", "w_school_raw", "zeta_j")])$w_school_raw,
        unique(sample[c("school_id", "w_school_raw", "zeta_j")])$zeta_j
      )),
      corr_w_student_cond_raw_x = suppressWarnings(stats::cor(
        sample$w_student_cond_raw, sample$x_ij
      ))
    ), diagnostics)
    attr(sample, "weight_metadata") <- metadata
    sample
  })
}

validate_survey_sample_weights <- function(data,
                                           J = NULL,
                                           weight_scaling_rule =
                                             "sns01_stagewise_sample_normalized",
                                           require_repweights = FALSE,
                                           brr_R = NULL) {
  needed <- c("school_id", "student_id", "pi_school", "pi_student_cond",
              "w_school_raw", "w_student_cond_raw", "w_final_raw",
              "w_school", "w_student_cond", "w_final", "w_norm_sample")
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop("[validate_survey_sample_weights] missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  .swg_check_pi(data$pi_school, "pi_school")
  .swg_check_pi(data$pi_student_cond, "pi_student_cond")
  semantics <- data$inclusion_probability_semantics %||%
    rep("legacy_unspecified", nrow(data))
  if (length(unique(semantics)) != 1L || anyNA(semantics)) {
    stop("[validate_survey_sample_weights] inclusion-probability semantics must be a single non-missing value",
         call. = FALSE)
  }
  exact_validated <- data$exact_inclusion_probability_validated %||%
    rep(FALSE, nrow(data))
  if (anyNA(exact_validated) || any(!(exact_validated %in% c(TRUE, FALSE)))) {
    stop("[validate_survey_sample_weights] exact inclusion-probability validation flag must be logical",
         call. = FALSE)
  }
  for (nm in c("w_school_raw", "w_student_cond_raw", "w_final_raw",
               "w_school", "w_student_cond", "w_final", "w_norm_sample")) {
    .swg_check_positive_finite(data[[nm]], nm)
  }
  if (!is.null(J) && length(unique(data$school_id)) != as.integer(J)) {
    stop("[validate_survey_sample_weights] sampled school count does not match J",
         call. = FALSE)
  }
  if (!identical(unique(data$weight_scaling_rule), weight_scaling_rule)) {
    stop("[validate_survey_sample_weights] unexpected weight_scaling_rule",
         call. = FALSE)
  }
  school_w <- aggregate(w_school ~ school_id, data, function(x) x[[1L]])
  checks <- list(
    n_school = length(unique(data$school_id)),
    n = nrow(data),
    raw_inverse_school = max(abs(data$w_school_raw - 1 / data$pi_school)),
    raw_inverse_student = max(abs(data$w_student_cond_raw -
                                    1 / data$pi_student_cond)),
    raw_reconstruction = max(abs(data$w_final_raw -
                                   data$w_school_raw *
                                   data$w_student_cond_raw)),
    staged_reconstruction = max(abs(data$w_final -
                                      data$w_school *
                                      data$w_student_cond)),
    sum_school_w = sum(school_w$w_school),
    sum_norm_sample = sum(data$w_norm_sample),
    row_support_hash = swg_row_support_hash(data),
    weight_metadata_hash = (attr(data, "weight_metadata") %||%
                              list())$weight_metadata_hash %||% NA_character_,
    inclusion_probability_semantics = unique(semantics)[[1L]],
    exact_inclusion_probability_validated = all(exact_validated),
    exact_inclusion_probability_claim_allowed = FALSE
  )
  recomputed_weight_hash <- .swg_digest(data[, c(
    "school_id", "student_id", "pi_school", "pi_student_cond",
    "w_school_raw", "w_student_cond_raw", "w_final_raw",
    "w_school", "w_student_cond", "w_final", "w_norm_sample"
  ), drop = FALSE])
  checks$recomputed_weight_metadata_hash <- recomputed_weight_hash
  checks$metadata_hash_current <- is.na(checks$weight_metadata_hash) ||
    identical(checks$weight_metadata_hash, recomputed_weight_hash)
  checks$row_support_hash_current <- identical(unique(data$row_support_hash),
                                               swg_row_support_hash(data))
  pass <- checks$raw_inverse_school < 1e-10 &&
    checks$raw_inverse_student < 1e-10 &&
    checks$raw_reconstruction < 1e-10 &&
    checks$staged_reconstruction < 1e-10 &&
    abs(checks$sum_school_w - checks$n_school) < 1e-10 &&
    abs(checks$sum_norm_sample - checks$n) < 1e-10 &&
    isTRUE(checks$metadata_hash_current) &&
    isTRUE(checks$row_support_hash_current)
  if (isTRUE(require_repweights)) {
    rep_cols <- grep("^repwt_", names(data), value = TRUE)
    if (is.null(brr_R)) brr_R <- length(rep_cols)
    pass <- pass && length(rep_cols) == as.integer(brr_R) &&
      all(vapply(data[rep_cols], function(x) {
        length(x) == nrow(data) && all(is.finite(x)) && all(x > 0)
      }, logical(1L)))
    checks$n_repwt <- length(rep_cols)
  }
  list(ok = isTRUE(pass), checks = checks)
}

survey_weight_golden_fixtures <- function() {
  c(
    "constant_unweighted_limit",
    "informative_school_weights",
    "informative_student_weights",
    "combined_staged_weights",
    "nonresponse_trimming_sensitivity",
    "brr_fay_replicate_support",
    "malformed_target_registry",
    "pathological_inclusion_probabilities"
  )
}
