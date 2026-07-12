# =============================================================================
# theorem.R: Theorem 2.2 fixed-effect point-identity validator
# =============================================================================
#
# Purpose : Numerically verify the paper's fixed-effect point identity: under
#           a common realized design X, a common fixed SPD covariance V, a
#           flat prior, common row support, and equal plausible-value weights,
#           the GLS fit to the averaged outcome equals the weighted average of
#           the per-PV GLS fits. Fits each PV and the averaged outcome with
#           gls_common_v (gls_solver.R), then compares the fixed-effect
#           vectors against a conditioning-aware tolerance. Scope is the point
#           identity only: no covariance, posterior, variance-component,
#           coverage, or CCC claim is checked. Inputs: X, V, and a list /
#           matrix of PV outcome vectors. Output: a pass flag with the
#           discrepancy delta, tolerance, both beta vectors, and assumptions.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   validate_theorem22_gls: the public point-identity validator above.
#   .t22_check_y_list / .t22_equal_pv_weights / .t22_tol /
#     .t22_check_no_prior_penalty: internal input and assumption guards and
#     the conditioning-aware tolerance used by the validator.
# =============================================================================

.t22_check_y_list <- function(Y_list, X) {
  if (is.data.frame(Y_list)) Y_list <- as.list(Y_list)
  if (is.matrix(Y_list)) {
    Y_list <- lapply(seq_len(ncol(Y_list)), function(j) Y_list[, j])
    names(Y_list) <- colnames(Y_list)
  }
  if (!is.list(Y_list) || length(Y_list) < 2L) {
    stop("[validate_theorem22_gls] Y_list must contain at least two PV vectors",
         call. = FALSE)
  }
  n <- nrow(X)
  out <- vector("list", length(Y_list))
  names(out) <- names(Y_list) %||% paste0("PV", seq_along(Y_list))
  row_ids <- vector("list", length(Y_list))
  for (m in seq_along(Y_list)) {
    y <- .swl_check_finite_vector(Y_list[[m]], sprintf("Y_list[[%d]]", m))
    if (length(y) != n) {
      stop("[validate_theorem22_gls] every PV vector must have length nrow(X)",
           call. = FALSE)
    }
    .swl_check_matching_support(.swl_row_support(y), .swl_row_support(X),
                                sprintf("Y_list[[%d]]", m), "X")
    out[[m]] <- y
    row_ids[[m]] <- .swl_row_support(y) %||% as.character(seq_along(y))
  }
  common <- swl_check_common_row_support(row_ids)
  list(Y = out, row_support = common)
}

.t22_equal_pv_weights <- function(weights, M, tol = 1e-12) {
  if (is.null(weights)) return(rep(1 / M, M))
  if (!is.numeric(weights) || !is.null(dim(weights)) || length(weights) != M ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("[validate_theorem22_gls] weights must be positive finite PV weights",
         call. = FALSE)
  }
  weights <- as.numeric(weights / sum(weights))
  target <- rep(1 / M, M)
  if (max(abs(weights - target)) > tol) {
    stop("[validate_theorem22_gls] Theorem 2.2 harness requires equal PV weights",
         call. = FALSE)
  }
  weights
}

.t22_tol <- function(beta_ref, kappa_system, n, p) {
  scale <- max(1, max(abs(beta_ref)))
  .Machine$double.eps * scale * max(100, 20 * kappa_system * max(1, n, p))
}

.t22_check_no_prior_penalty <- function(prior_precision, prior_mean,
                                        penalty_matrix) {
  supplied <- c(
    prior_precision = !missing(prior_precision) && !is.null(prior_precision),
    prior_mean = !missing(prior_mean) && !is.null(prior_mean),
    penalty_matrix = !missing(penalty_matrix) && !is.null(penalty_matrix)
  )
  if (any(supplied)) {
    stop(
      "[validate_theorem22_gls] flat prior/no fixed-effect penalty required; prior or penalty arguments are outside the Theorem 2.2 harness",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate Theorem 2.2 in the fixed-effect GLS/MLE harness
#'
#' This binding check verifies only the fixed-effect point identity under common
#' realized `X`, common fixed SPD `V`, flat prior, common row support, and equal
#' PV weights. It does not validate covariance, posterior, variance-component,
#' coverage, production/PISA, or CCC claims.
validate_theorem22_gls <- function(X,
                                   V,
                                   Y_list,
                                   weights = NULL,
                                   tol = NULL,
                                   cond_abort = 1e12,
                                   prior_precision = NULL,
                                   prior_mean = NULL,
                                   penalty_matrix = NULL) {
  .t22_check_no_prior_penalty(prior_precision, prior_mean, penalty_matrix)

  X <- .swl_check_finite_matrix(X, "X")
  if (nrow(X) < 2L || ncol(X) < 1L) {
    stop("[validate_theorem22_gls] X must have at least two rows and one column",
         call. = FALSE)
  }
  if (qr(X)$rank < ncol(X)) {
    stop("[validate_theorem22_gls] X is rank deficient", call. = FALSE)
  }

  if (missing(V) || is.null(V)) {
    stop("[validate_theorem22_gls] common fixed V is required", call. = FALSE)
  }
  V <- .swl_check_finite_matrix(V, "V")
  if (nrow(V) != nrow(X) || ncol(V) != nrow(X)) {
    stop("[validate_theorem22_gls] V dimensions must match nrow(X)",
         call. = FALSE)
  }
  .swl_check_matching_support(.swl_row_support(X), .swl_row_support(V, "row"),
                              "X", "V rows")
  .swl_check_matching_support(.swl_row_support(X), .swl_row_support(V, "col"),
                              "X", "V columns")

  y_checked <- .t22_check_y_list(Y_list, X)
  M <- length(y_checked$Y)
  pv_weights <- .t22_equal_pv_weights(weights, M)

  fit_by_pv <- lapply(
    y_checked$Y,
    function(y) gls_common_v(y = y, X = X, V = V, cond_abort = cond_abort)
  )
  beta_by_pv <- vapply(fit_by_pv, `[[`, numeric(ncol(X)), "beta")
  rownames(beta_by_pv) <- colnames(X) %||% paste0("x", seq_len(ncol(X)))
  colnames(beta_by_pv) <- names(y_checked$Y)
  beta_pooled <- as.numeric(beta_by_pv %*% pv_weights)
  names(beta_pooled) <- rownames(beta_by_pv)

  y_bar <- Reduce(`+`, Map(`*`, y_checked$Y, pv_weights))
  names(y_bar) <- .swl_row_support(X)
  stacked_fit <- gls_common_v(y = y_bar, X = X, V = V,
                              cond_abort = cond_abort)
  beta_stacked <- stacked_fit$beta

  beta_diff <- beta_stacked - beta_pooled
  beta_scale <- max(1, max(abs(beta_stacked)), max(abs(beta_pooled)))
  delta <- max(abs(beta_diff))
  kappa_v <- stacked_fit$kappa_system
  tol <- tol %||% .t22_tol(beta_pooled, kappa_v, nrow(X), ncol(X))
  delta_gls_rel_inf <- delta / beta_scale
  delta_over_eps <- delta / (.Machine$double.eps * beta_scale)
  delta_over_tolerance <- delta / tol

  list(
    pass = isTRUE(delta <= tol),
    ok = isTRUE(delta <= tol),
    delta = delta,
    delta_gls_inf_norm = delta,
    delta_gls_rel_inf = delta_gls_rel_inf,
    delta_over_eps = delta_over_eps,
    delta_over_tolerance = delta_over_tolerance,
    tol = tol,
    tolerance = tol,
    tolerance_abs = tol,
    beta_stacked = beta_stacked,
    beta_rubin = beta_pooled,
    beta_pooled = beta_pooled,
    beta_diff = beta_diff,
    beta_by_pv = t(beta_by_pv),
    weights = pv_weights,
    pv_weights = pv_weights,
    equal_pv_weights = TRUE,
    precision_source = stacked_fit$precision_source,
    rank = stacked_fit$rank,
    df_residual = stacked_fit$df_residual,
    row_ids = y_checked$row_support,
    x_colnames = colnames(X) %||% paste0("x", seq_len(ncol(X))),
    assumptions = c(
      "common analytic row support",
      "common cluster membership",
      "common realized design matrix X",
      "fixed common SPD working covariance V",
      "flat fixed-effect prior / MLE regime",
      "equal plausible-value weights",
      "fixed-effect point identity only"
    ),
    n = nrow(X),
    p = ncol(X),
    M = M,
    row_support_hash = if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(y_checked$row_support, algo = "sha256")
    } else {
      NA_character_
    },
    kappa_V = kappa_v
  )
}
