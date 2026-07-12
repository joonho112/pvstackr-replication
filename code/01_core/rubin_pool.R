# =============================================================================
# rubin_pool.R: Rubin's multiple-imputation pooling primitives
# =============================================================================
#
# Purpose : Combine per-plausible-value estimates by Rubin's rules. Given the
#           per-PV estimates and their (co)variances, the routines form the
#           within-imputation variance U_bar, the between-imputation variance
#           B, and the total T = U_bar + (1 + 1/M) B, then report standard
#           errors, degrees of freedom (classic or Barnard-Rubin), and
#           confidence limits. Shared MI backbone reused by per_pv.R and the
#           BRR-Fay target assembly (targets.R). Inputs: a beta matrix and
#           matching covariances (list / array), or scalar q / u vectors.
#           Output: a pooled-result list (beta, U_bar, B, T_MI, se, df, CI).
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   rubin_pool_matrix (alias rubin_pool): multivariate Rubin pooling of a
#     beta matrix and per-PV covariances; U_bar, B, T_MI, se, df, CI.
#   rubin_pool_scalar: scalar-parameter Rubin pooling from q / u vectors.
#   rubin_df_classic: classic (M-1)/rho^2 multiple-imputation df.
#   rubin_df_barnard_rubin: small-sample Barnard-Rubin adjusted df.
# =============================================================================

.swl_symmetrize <- function(M) 0.5 * (M + t(M))

.swl_check_numeric_matrix <- function(x, label) {
  if (!is.matrix(x) || !is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("[%s] must be a finite numeric matrix", label), call. = FALSE)
  }
  x
}

.swl_U_array <- function(U, M, p) {
  if (is.list(U)) {
    if (length(U) != M) {
      stop("[rubin_pool_matrix] U list length must equal M", call. = FALSE)
    }
    out <- array(NA_real_, dim = c(p, p, M))
    for (m in seq_len(M)) {
      .swl_check_numeric_matrix(U[[m]], sprintf("U[[%d]]", m))
      if (!identical(dim(U[[m]]), c(p, p))) {
        stop("[rubin_pool_matrix] every U matrix must be p x p", call. = FALSE)
      }
      if (max(abs(U[[m]] - t(U[[m]]))) > 1e-10) {
        stop("[rubin_pool_matrix] every U matrix must be symmetric", call. = FALSE)
      }
      ev <- eigen(.swl_symmetrize(U[[m]]), symmetric = TRUE,
                  only.values = TRUE)$values
      if (min(ev) < -1e-10) {
        stop("[rubin_pool_matrix] every U matrix must be positive semidefinite",
             call. = FALSE)
      }
      out[, , m] <- .swl_symmetrize(U[[m]])
    }
    out
  } else if (is.array(U) && length(dim(U)) == 3L) {
    if (!identical(dim(U), c(p, p, M))) {
      stop("[rubin_pool_matrix] U array must have dimension p x p x M",
           call. = FALSE)
    }
    if (!is.numeric(U) || anyNA(U) || any(!is.finite(U))) {
      stop("[rubin_pool_matrix] U array must be finite numeric", call. = FALSE)
    }
    for (m in seq_len(M)) {
      if (max(abs(U[, , m] - t(U[, , m]))) > 1e-10) {
        stop("[rubin_pool_matrix] every U matrix must be symmetric", call. = FALSE)
      }
      ev <- eigen(.swl_symmetrize(U[, , m]), symmetric = TRUE,
                  only.values = TRUE)$values
      if (min(ev) < -1e-10) {
        stop("[rubin_pool_matrix] every U matrix must be positive semidefinite",
             call. = FALSE)
      }
      U[, , m] <- .swl_symmetrize(U[, , m])
    }
    U
  } else {
    stop("[rubin_pool_matrix] U must be a list of p x p matrices or a p x p x M array",
         call. = FALSE)
  }
}

rubin_df_classic <- function(M, rho) {
  if (!is.numeric(M) || length(M) != 1L || is.na(M) || M < 1L) {
    stop("[rubin_df_classic] M must be a positive scalar", call. = FALSE)
  }
  if (!is.numeric(rho) || anyNA(rho) || any(!is.finite(rho)) ||
      any(rho < 0) || any(rho > 1 + sqrt(.Machine$double.eps))) {
    stop("[rubin_df_classic] rho must be finite and in [0, 1]",
         call. = FALSE)
  }
  if (M > 1L) {
    (M - 1L) / pmax(rho^2, .Machine$double.eps)
  } else {
    rep(Inf, length(rho))
  }
}

rubin_df_barnard_rubin <- function(M, rho, df_complete) {
  nu_old <- rubin_df_classic(M, rho)
  if (missing(df_complete) || is.null(df_complete)) {
    stop("[rubin_df_barnard_rubin] df_complete is required",
         call. = FALSE)
  }
  if (!is.numeric(df_complete) || anyNA(df_complete) ||
      any(df_complete <= 0)) {
    stop("[rubin_df_barnard_rubin] df_complete must be positive",
         call. = FALSE)
  }
  if (length(df_complete) == 1L) {
    df_complete <- rep(df_complete, length(rho))
  }
  if (length(df_complete) != length(rho)) {
    stop("[rubin_df_barnard_rubin] df_complete must be scalar or match rho length",
         call. = FALSE)
  }
  if (any(is.infinite(df_complete))) {
    out <- nu_old
    finite <- is.finite(df_complete)
    if (!any(finite)) return(out)
    out[finite] <- rubin_df_barnard_rubin(M, rho[finite],
                                          df_complete[finite])
    return(out)
  }
  nu_obs <- ((df_complete + 1) / (df_complete + 3)) *
    df_complete * pmax(1 - rho, 0)
  1 / (1 / nu_old + 1 / pmax(nu_obs, .Machine$double.eps))
}

rubin_pool_matrix <- function(beta,
                              U,
                              orientation = c("rows_pv", "cols_pv"),
                              conf_level = 0.95,
                              allow_m1 = FALSE,
                              df_method = c("classic", "barnard_rubin"),
                              df_complete = NULL) {
  orientation <- match.arg(orientation)
  df_method <- match.arg(df_method)
  beta <- .swl_check_numeric_matrix(beta, "beta")
  if (orientation == "rows_pv") {
    M <- nrow(beta)
    p <- ncol(beta)
    beta_pxm <- t(beta)
  } else {
    p <- nrow(beta)
    M <- ncol(beta)
    beta_pxm <- beta
  }
  if (M < 2L && !isTRUE(allow_m1)) {
    stop("[rubin_pool_matrix] need M >= 2 imputations unless allow_m1 = TRUE",
         call. = FALSE)
  }
  if (p < 1L) {
    stop("[rubin_pool_matrix] beta must contain at least one parameter",
         call. = FALSE)
  }

  U_array <- .swl_U_array(U, M = M, p = p)
  beta_bar <- rowMeans(beta_pxm)
  U_bar <- apply(U_array, c(1, 2), mean)
  U_bar <- .swl_symmetrize(U_bar)

  if (M > 1L) {
    beta_centered <- sweep(beta_pxm, 1L, beta_bar, FUN = "-")
    B <- (beta_centered %*% t(beta_centered)) / (M - 1L)
  } else {
    B <- matrix(0, p, p)
  }
  B <- .swl_symmetrize(B)
  T_MI <- .swl_symmetrize(U_bar + (1 + 1 / M) * B)

  eps <- .Machine$double.eps
  rho <- diag((1 + 1 / M) * B) / pmax(diag(T_MI), eps)
  df <- if (identical(df_method, "barnard_rubin")) {
    rubin_df_barnard_rubin(M, rho, df_complete)
  } else {
    rubin_df_classic(M, rho)
  }
  se <- sqrt(diag(T_MI))
  alpha <- 1 - conf_level
  t_quantile <- stats::qt(1 - alpha / 2, df = df)

  names(beta_bar) <- if (orientation == "rows_pv") colnames(beta) else rownames(beta)
  if (is.null(names(beta_bar))) names(beta_bar) <- paste0("theta", seq_len(p))
  dimnames(U_bar) <- list(names(beta_bar), names(beta_bar))
  dimnames(B) <- list(names(beta_bar), names(beta_bar))
  dimnames(T_MI) <- list(names(beta_bar), names(beta_bar))
  names(se) <- names(df) <- names(rho) <- names(beta_bar)

  list(
    beta = beta_bar,
    beta_bar = beta_bar,
    U_bar = U_bar,
    B = B,
    T_MI = T_MI,
    total_var = T_MI,
    se = se,
    df = df,
    df_classic = rubin_df_classic(M, rho),
    df_method = df_method,
    df_complete = if (is.null(df_complete)) NA_real_ else df_complete,
    rho = rho,
    ci_low = beta_bar - t_quantile * se,
    ci_high = beta_bar + t_quantile * se,
    M = M,
    p = p,
    orientation = orientation,
    conf_level = conf_level
  )
}

rubin_pool <- rubin_pool_matrix

rubin_pool_scalar <- function(q, u, conf_level = 0.95, allow_m1 = FALSE,
                              df_method = c("classic", "barnard_rubin"),
                              df_complete = NULL) {
  df_method <- match.arg(df_method)
  if (!is.numeric(q) || !is.numeric(u) || length(q) != length(u) ||
      anyNA(q) || anyNA(u) || any(!is.finite(q)) || any(!is.finite(u))) {
    stop("[rubin_pool_scalar] q and u must be finite numeric vectors of equal length",
         call. = FALSE)
  }
  U <- lapply(u, function(x) matrix(x, 1, 1))
  out <- rubin_pool_matrix(
    beta = matrix(q, ncol = 1, dimnames = list(NULL, "theta")),
    U = U,
    orientation = "rows_pv",
    conf_level = conf_level,
    allow_m1 = allow_m1,
    df_method = df_method,
    df_complete = df_complete
  )
  list(
    q_bar = unname(out$beta[[1]]),
    u_bar = unname(out$U_bar[1, 1]),
    b = unname(out$B[1, 1]),
    total_var = unname(out$T_MI[1, 1]),
    se = unname(out$se[[1]]),
    df = unname(out$df[[1]]),
    df_classic = unname(out$df_classic[[1]]),
    df_method = out$df_method,
    df_complete = unname(out$df_complete[[1]]),
    ci_low = unname(out$ci_low[[1]]),
    ci_high = unname(out$ci_high[[1]]),
    M = out$M
  )
}
