# =============================================================================
# gls_solver.R: Weighted GLS and random-intercept ML/REML helpers
# =============================================================================
#
# Purpose : Low-level linear-model solvers used across the engine.
#           gls_common_v solves generalized least squares by Cholesky
#           whitening given a known covariance V, precision V_inv, or diagonal
#           weights, returning beta, its covariance, standard errors, fitted
#           values, and conditioning diagnostics. gls_lmer_fit wraps
#           lme4::lmer for the jointly estimated random-intercept (ML/REML)
#           path. These feed the Theorem 2.2 validator (theorem.R) and the
#           BRR-Fay replicate fits (targets.R). Inputs: y and design X (plus
#           V / V_inv / weights), or a mixed-model formula and data.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   gls_common_v: common-V GLS by Cholesky whitening; beta, vcov, se, fitted.
#   gls_lmer_fit: jointly estimated random-intercept fit via lme4::lmer.
#   gls_solver: dispatch between the "common_v" and "joint_lmer" paths.
#   swl_gls_common_v / swl_lmm_fit / swl_gls_solver: swl_-prefixed aliases.
# =============================================================================

.swl_check_finite_matrix <- function(x, label) {
  if (!is.matrix(x) || !is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("[%s] must be a finite numeric matrix", label), call. = FALSE)
  }
  x
}

.swl_check_finite_vector <- function(x, label) {
  if (!is.numeric(x) || !is.null(dim(x)) || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("[%s] must be a finite numeric vector", label), call. = FALSE)
  }
  out <- as.numeric(x)
  names(out) <- names(x)
  out
}

.swl_sym <- function(M) 0.5 * (M + t(M))

.swl_spd_chol <- function(M, label = "V", cond_abort = 1e12) {
  M <- .swl_check_finite_matrix(M, label)
  if (nrow(M) != ncol(M)) stop(sprintf("[%s] must be square", label), call. = FALSE)
  if (max(abs(M - t(M))) > 1e-10) {
    stop(sprintf("[%s] must be symmetric", label), call. = FALSE)
  }
  M <- .swl_sym(M)
  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) <= 0) {
    stop(sprintf("[%s] must be positive definite", label), call. = FALSE)
  }
  kap <- max(ev) / min(ev)
  if (is.finite(kap) && kap >= cond_abort) {
    stop(sprintf("[%s] is ill-conditioned: kappa = %.3e", label, kap),
         call. = FALSE)
  }
  list(U = chol(M), kappa = kap, eigenvalues = ev)
}

.swl_row_support <- function(x, margin = c("row", "col")) {
  margin <- match.arg(margin)
  if (is.null(dim(x))) return(names(x))
  if (identical(margin, "row")) rownames(x) else colnames(x)
}

.swl_check_matching_support <- function(lhs, rhs, lhs_label, rhs_label) {
  if (is.null(lhs) || is.null(rhs)) return(invisible(TRUE))
  if (!identical(as.character(lhs), as.character(rhs))) {
    stop(sprintf(
      "[gls_common_v] row support mismatch between %s and %s",
      lhs_label, rhs_label
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.swl_check_covariance_support <- function(M, y, X, label) {
  row_id <- .swl_row_support(M, "row")
  col_id <- .swl_row_support(M, "col")
  .swl_check_matching_support(row_id, col_id, paste0(label, " rows"),
                              paste0(label, " columns"))
  .swl_check_matching_support(.swl_row_support(y), .swl_row_support(X),
                              "y", "X")
  .swl_check_matching_support(.swl_row_support(y), row_id, "y",
                              paste0(label, " rows"))
  .swl_check_matching_support(.swl_row_support(X), row_id, "X",
                              paste0(label, " rows"))
  invisible(TRUE)
}

swl_check_common_row_support <- function(row_ids_by_pv) {
  if (!is.list(row_ids_by_pv) || !length(row_ids_by_pv)) {
    stop("[swl_check_common_row_support] row_ids_by_pv must be a nonempty list",
         call. = FALSE)
  }
  ids <- lapply(row_ids_by_pv, function(x) {
    out <- as.character(x)
    if (!length(out) || anyNA(out) || any(!nzchar(out))) {
      stop("[swl_check_common_row_support] row support ids must be nonempty",
           call. = FALSE)
    }
    if (any(duplicated(out))) {
      stop("[swl_check_common_row_support] duplicate row support ids",
           call. = FALSE)
    }
    out
  })
  ref <- ids[[1L]]
  bad <- vapply(ids[-1L], function(x) !identical(x, ref), logical(1L))
  if (any(bad)) {
    stop("[swl_check_common_row_support] row support mismatch across PVs",
         call. = FALSE)
  }
  ref
}

#' Common-V GLS by Cholesky whitening
#'
#' @param y numeric response vector.
#' @param X finite numeric design matrix, including any intercept column.
#' @param V optional known covariance matrix of `y`.
#' @param V_inv optional known precision matrix of `y`.
#' @param weights optional diagonal precision weights. Equivalent to
#'   `V_inv = diag(weights)`.
#' @param sigma2 optional scalar multiplier for `vcov_beta` when `V_inv` or
#'   `weights` are supplied as relative precision weights.
#' @param cond_abort condition-number threshold for aborting ill-conditioned
#'   covariance or precision systems.
#' @return list with beta, vcov_beta, se, fitted, residuals, rank, df_residual,
#'   sigma2, path, and diagnostic fields.
gls_common_v <- function(y,
                         X,
                         V = NULL,
                         V_inv = NULL,
                         weights = NULL,
                         sigma2 = NULL,
                         cond_abort = 1e12) {
  y <- .swl_check_finite_vector(y, "y")
  X <- .swl_check_finite_matrix(X, "X")
  if (nrow(X) != length(y)) {
    stop("[gls_common_v] nrow(X) must equal length(y)", call. = FALSE)
  }
  .swl_check_matching_support(.swl_row_support(y), .swl_row_support(X),
                              "y", "X")
  if (ncol(X) < 1L) stop("[gls_common_v] X must have at least one column", call. = FALSE)
  if (qr(X)$rank < ncol(X)) {
    stop("[gls_common_v] X is rank deficient", call. = FALSE)
  }
  supplied <- sum(!vapply(list(V, V_inv, weights), is.null, logical(1)))
  if (supplied > 1L) {
    stop("[gls_common_v] supply only one of V, V_inv, or weights", call. = FALSE)
  }
  if (!is.null(sigma2)) {
    if (!is.numeric(sigma2) || length(sigma2) != 1L || !is.finite(sigma2) || sigma2 <= 0) {
      stop("[gls_common_v] sigma2 must be a positive finite scalar", call. = FALSE)
    }
  }

  n <- length(y)
  p <- ncol(X)

  if (!is.null(V)) {
    if (nrow(V) != n) stop("[gls_common_v] V dimension must match y", call. = FALSE)
    .swl_check_covariance_support(V, y, X, "V")
    chol_v <- .swl_spd_chol(V, label = "V", cond_abort = cond_abort)
    y_w <- backsolve(chol_v$U, y, transpose = TRUE)
    X_w <- backsolve(chol_v$U, X, transpose = TRUE)
    precision_source <- "V"
    sigma2_out <- 1
    kappa_system <- chol_v$kappa
  } else if (!is.null(V_inv)) {
    if (nrow(V_inv) != n) stop("[gls_common_v] V_inv dimension must match y", call. = FALSE)
    .swl_check_covariance_support(V_inv, y, X, "V_inv")
    chol_p <- .swl_spd_chol(V_inv, label = "V_inv", cond_abort = cond_abort)
    y_w <- chol_p$U %*% y
    X_w <- chol_p$U %*% X
    precision_source <- "V_inv"
    sigma2_out <- sigma2 %||% 1
    kappa_system <- chol_p$kappa
  } else if (!is.null(weights)) {
    weights <- .swl_check_finite_vector(weights, "weights")
    if (length(weights) != n) {
      stop("[gls_common_v] weights length must match y", call. = FALSE)
    }
    .swl_check_matching_support(.swl_row_support(y), .swl_row_support(weights),
                                "y", "weights")
    .swl_check_matching_support(.swl_row_support(X), .swl_row_support(weights),
                                "X", "weights")
    if (any(weights <= 0)) {
      stop("[gls_common_v] weights must be positive", call. = FALSE)
    }
    sqrt_w <- sqrt(weights)
    y_w <- sqrt_w * y
    X_w <- X * sqrt_w
    precision_source <- "weights"
    sigma2_out <- sigma2 %||% 1
    kappa_system <- max(weights) / min(weights)
    if (is.finite(kappa_system) && kappa_system >= cond_abort) {
      stop(sprintf("[gls_common_v] weights are ill-conditioned: kappa = %.3e",
                   kappa_system), call. = FALSE)
    }
  } else {
    y_w <- y
    X_w <- X
    precision_source <- "identity"
    sigma2_out <- sigma2 %||% 1
    kappa_system <- 1
  }

  qr_x <- qr(X_w, LAPACK = TRUE)
  if (qr_x$rank < p) {
    stop("[gls_common_v] whitened design is rank deficient", call. = FALSE)
  }
  beta <- as.numeric(qr.coef(qr_x, y_w))
  names(beta) <- colnames(X) %||% paste0("x", seq_len(p))

  XtX_inv <- chol2inv(chol(crossprod(X_w)))
  dimnames(XtX_inv) <- list(names(beta), names(beta))
  fitted <- as.numeric(X %*% beta)
  residuals <- y - fitted
  vcov_beta <- sigma2_out * XtX_inv

  list(
    beta = beta,
    coef = beta,
    vcov_beta = vcov_beta,
    se = sqrt(diag(vcov_beta)),
    fitted = fitted,
    residuals = residuals,
    rank = p,
    df_residual = n - p,
    sigma2 = sigma2_out,
    path = "common_v",
    precision_source = precision_source,
    kappa_system = kappa_system,
    n = n,
    p = p
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Jointly estimated random-intercept path using lme4
#'
#' @param formula mixed-model formula accepted by `lme4::lmer`.
#' @param data data.frame.
#' @param REML logical; passed to `lme4::lmer`.
#' @param weights optional prior weights.
#' @return list with fixed effects, fixed-effect covariance, residual sigma,
#'   variance components, convergence status, and the fitted model.
gls_lmer_fit <- function(formula,
                         data,
                         REML = TRUE,
                         weights = NULL,
                         control = NULL) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("[gls_lmer_fit] lme4 is required", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("[gls_lmer_fit] data must be a data.frame", call. = FALSE)
  control <- control %||% lme4::lmerControl(calc.derivs = FALSE)
  fit <- lme4::lmer(
    formula = formula,
    data = data,
    REML = REML,
    weights = weights,
    control = control
  )
  vc <- as.data.frame(lme4::VarCorr(fit))
  beta <- lme4::fixef(fit)
  vcov_beta <- as.matrix(stats::vcov(fit))
  list(
    beta = beta,
    coef = beta,
    vcov_beta = vcov_beta,
    se = sqrt(diag(vcov_beta)),
    sigma = stats::sigma(fit),
    var_components = vc,
    theta = lme4::getME(fit, "theta"),
    convergence = fit@optinfo$conv$lme4$messages %||% character(0),
    path = "joint_lmer",
    REML = REML,
    fit = fit
  )
}

#' Dispatch helper for common-V or jointly estimated paths
gls_solver <- function(path = c("common_v", "joint_lmer"), ...) {
  path <- match.arg(path)
  switch(
    path,
    common_v = gls_common_v(...),
    joint_lmer = gls_lmer_fit(...)
  )
}

swl_gls_common_v <- gls_common_v
swl_lmm_fit <- gls_lmer_fit
swl_gls_solver <- gls_solver
