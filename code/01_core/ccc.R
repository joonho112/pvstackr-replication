# =============================================================================
# ccc.R: Cholesky Calibration Correction (CCC) affine map
# =============================================================================
#
# Purpose : Calibrate the raw stacked posterior so its fixed-effect covariance
#           matches an external design-based target while leaving the point
#           estimate fixed. Forms the affine map A = L_target L_raw^{-1} from
#           Cholesky factors of the raw and target covariances, applies
#           psi* = psi_hat + A (psi - psi_hat) to the FE draws, and passes
#           variance components through untouched. Inputs: a stacked draw
#           matrix, a fixed / variance-component param_map, and a symmetric PD
#           Sigma_target (the Rubin-pooled BRR-Fay sandwich). Output: an S3
#           "ccc_twolevel" object with calibrated draws, A, psi_hat, the
#           calibrated covariance, and a diagnostic ladder.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   ccc_twolevel: integrated Steps 1-8 wrapper; returns the ccc_twolevel S3
#     object (calibrated draws, A, psi_hat, covariance, diagnostics, flags).
#   ccc_step1_assemble: split draws into FE / VC blocks and resolve center.
#   ccc_step2_cov_raw: empirical raw FE covariance (symmetrized).
#   ccc_step3_chol_raw / ccc_step4_chol_target: safe Cholesky of the raw and
#     target covariances with condition logging and nearest-PD fallback.
#   ccc_step5_calibration_matrix: form A = L_target L_raw^{-1}.
#   ccc_step6_apply_calibration: apply the affine map to FE draws; pass VC.
#   ccc_step7_stabilize: symmetrize / nearest-PD the calibrated covariance.
#   ccc_step8_diagnostics: calibration diagnostic ladder (delta_c_rel, rho).
#   assemble_rubin_brrfay_target: build Sigma_target = U_bar + (1+1/M) B.
#   swl_ccc_confirmatory_result_row: emit a schema row, withholding an
#     estimate whenever CCC status is not ok or the target was PD-repaired.
#   print.ccc_twolevel: S3 print method.
# =============================================================================

# ============================================================================
# codebase/R/ccc_twolevel.R
# SWL Paper 2 — Cholesky Calibration Correction (CCC) for two-level
# random-intercept models.
#
# Spec: log/015_step2.2-ccc-twolevel-spec.qmd
# Implementation:
#   - Step 2.3 (this file, Part A: Steps 1-4 of the 8-step algorithm)
#   - Step 2.4 (Part B: Steps 5-8 + 7-property unit tests)
#
# Reference: bayesRVE 0.6.0 technical report ch04 (single-level derivation)
# and ch09 (numerical hygiene). Two-level new derivations: block-diagonal
# embedding (Step 5), VC passthrough (Step 6).
# ============================================================================

# ---------------------------------------------------------------------------
# Helper: symmetrization
# ---------------------------------------------------------------------------
.ccc_symmetrize <- function(M) {
  0.5 * (M + t(M))
}

# ---------------------------------------------------------------------------
# Helper: resolve an external numeric center vector to a named numeric aligned
# to fe_names. Named center -> subset/reorder to fe_names (error if any FE name
# missing). Unnamed center -> require length == length(fe_names), attach names.
# Shared by ccc_step1_assemble (here) and engine_moments (engine_facade.R).
# ---------------------------------------------------------------------------
.swl_resolve_external_center <- function(center, fe_names) {
  if (!is.numeric(center)) {
    stop("[.swl_resolve_external_center] center must be numeric", call. = FALSE)
  }
  if (!is.null(names(center))) {
    missing_names <- setdiff(fe_names, names(center))
    if (length(missing_names)) {
      stop("[.swl_resolve_external_center] external center is missing names: ",
           paste(missing_names, collapse = ", "), call. = FALSE)
    }
    return(center[fe_names])
  }
  if (length(center) != length(fe_names)) {
    stop(sprintf(
      "[.swl_resolve_external_center] unnamed external center has length %d but %d FE names expected",
      length(center), length(fe_names)), call. = FALSE)
  }
  stats::setNames(as.numeric(center), fe_names)
}

# ---------------------------------------------------------------------------
# Helper: nearest positive-definite projection (Higham 1988-style)
# Uses Matrix::nearPD when available; otherwise eigen-flooring.
# ---------------------------------------------------------------------------
.ccc_nearpd <- function(M, eps = 1e-10) {
  if (requireNamespace("Matrix", quietly = TRUE)) {
    out <- tryCatch(
      as.matrix(Matrix::nearPD(M, base.matrix = TRUE, eig.tol = eps)$mat),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }
  # Fallback: spectral floor
  e <- eigen(M, symmetric = TRUE)
  lam_floor <- eps * sum(diag(M)) / nrow(M)
  lam <- pmax(e$values, lam_floor)
  Mpd <- e$vectors %*% diag(lam, nrow = length(lam)) %*% t(e$vectors)
  .ccc_symmetrize(Mpd)
}

# ---------------------------------------------------------------------------
# Helper: safe Cholesky with nearest-PD fallback and condition logging
# Returns list(L_upper, kappa, nearpd_applied)
# Convention: R's chol() returns upper-triangular U with t(U) %*% U = M.
# We store U (upper) and call sites use t(U) when lower-tri is needed.
# ---------------------------------------------------------------------------
.ccc_safe_chol <- function(M, eps = 1e-10, cond_warn = 1e8, cond_abort = 1e12,
                           label = "matrix", allow_nearpd = TRUE) {
  Msym <- .ccc_symmetrize(M)
  nearpd_applied <- FALSE

	  U <- tryCatch(
	    chol(Msym, pivot = FALSE),
	    error = function(e) {
	      if (!isTRUE(allow_nearpd)) {
	        stop(sprintf(
	          "[ccc_twolevel] %s is not positive definite; refusing nearest-PD repair",
	          label
	        ), call. = FALSE)
	      }
	      Mpd <<- .ccc_nearpd(Msym, eps = eps)
	      nearpd_applied <<- TRUE
      chol(Mpd, pivot = FALSE)
    }
  )

  # If the tryCatch did not error, Mpd is unset; create alias
  if (!nearpd_applied) Mpd <- Msym

  # Condition number via singular values
  sv <- svd(U)$d
  kap <- if (min(sv) > 0) max(sv) / min(sv) else Inf

  if (is.finite(kap) && kap >= cond_abort) {
    stop(sprintf("[ccc_twolevel] %s ill-conditioned: kappa = %.3e (>= %.3e)",
                 label, kap, cond_abort))
  }
  if (is.finite(kap) && kap >= cond_warn) {
    warning(sprintf("[ccc_twolevel] %s near singular: kappa = %.3e (>= %.3e)",
                    label, kap, cond_warn))
  }

  list(U = U, kappa = kap, nearpd_applied = nearpd_applied, M_pd = Mpd)
}

# ---------------------------------------------------------------------------
# Step 1 — Assemble stacked posterior moments
# Input:
#   posterior_stacked : matrix-coercible draws object (rows = S draws,
#                       cols = parameters)
#   param_map         : list(fe_idx = ..., vc_idx = ...)
#   center            : "posterior_mean" or an external numeric target
# Output: list(psi_hat, draws_fe, draws_vc, S, p_fe, p_vc, fe_names, vc_names)
# ---------------------------------------------------------------------------
ccc_step1_assemble <- function(posterior_stacked, param_map,
                               center = "posterior_mean") {
  # Allow numeric external center (named or positional vector); skip match.arg.
  external_center <- is.numeric(center)
  if (!external_center) {
    center <- match.arg(center, "posterior_mean")
  }

  # Coerce to plain matrix (handle posterior::draws_matrix / array / data.frame)
  draws <- if (inherits(posterior_stacked, "draws")) {
    suppressPackageStartupMessages({
      requireNamespace("posterior", quietly = TRUE)
    })
    as.matrix(posterior::as_draws_matrix(posterior_stacked))
  } else if (is.matrix(posterior_stacked)) {
    posterior_stacked
  } else if (is.data.frame(posterior_stacked)) {
    as.matrix(posterior_stacked)
  } else {
    stop("[ccc_step1] unsupported posterior_stacked class: ",
         paste(class(posterior_stacked), collapse = "/"))
  }

  if (is.null(colnames(draws))) {
    stop("[ccc_step1] posterior_stacked must have colnames")
  }

  # Resolve param_map indices (allow names or integers)
  resolve_idx <- function(idx, all_names) {
    if (is.character(idx)) {
      out <- match(idx, all_names)
      if (anyNA(out)) {
        stop("[ccc_step1] unknown column name(s) in param_map: ",
             paste(idx[is.na(out)], collapse = ", "))
      }
      out
    } else {
      as.integer(idx)
    }
  }

  all_names <- colnames(draws)
  fe_idx <- resolve_idx(param_map$fe_idx, all_names)
  vc_idx <- if (length(param_map$vc_idx) > 0L) {
    resolve_idx(param_map$vc_idx, all_names)
  } else integer(0)

  stopifnot(length(intersect(fe_idx, vc_idx)) == 0L)

  draws_fe <- draws[, fe_idx, drop = FALSE]
  draws_vc <- if (length(vc_idx) > 0L) draws[, vc_idx, drop = FALSE] else NULL

  # psi_hat: output center / point estimate of the FE block.
  # psi_hat_pivot: pivot for the de-/re-centering in step 6.
  #   string options -> psi_hat_pivot == psi_hat (empirical centroid).
  #   external center -> psi_hat_pivot = colMeans(draws_fe) (rotation pivot for
  #   covariance calibration) while psi_hat = ext_center (target output mean),
  #   so the calibrated cloud has cov = Sigma_target AND mean = ext_center.
  fe_names_local <- all_names[fe_idx]
  if (external_center) {
    psi_hat       <- .swl_resolve_external_center(center, fe_names_local)
    psi_hat_pivot <- colMeans(draws_fe)
    center_label  <- "external"
  } else {
    psi_hat <- colMeans(draws_fe)
    psi_hat_pivot <- psi_hat
    center_label  <- center
  }

  list(
    psi_hat       = psi_hat,
    psi_hat_pivot = psi_hat_pivot,
    draws_fe      = draws_fe,
    draws_vc      = draws_vc,
    S             = nrow(draws),
    p_fe          = length(fe_idx),
    p_vc          = length(vc_idx),
    fe_names      = fe_names_local,
    vc_names      = if (length(vc_idx) > 0L) all_names[vc_idx] else character(0),
    fe_idx        = fe_idx,
    vc_idx        = vc_idx,
    center        = center_label
  )
}

# ---------------------------------------------------------------------------
# Step 2 — Empirical raw covariance + symmetrization
# ---------------------------------------------------------------------------
ccc_step2_cov_raw <- function(step1) {
  Sigma_raw <- stats::cov(step1$draws_fe)
  Sigma_raw <- .ccc_symmetrize(Sigma_raw)
  Sigma_raw
}

# ---------------------------------------------------------------------------
# Step 3 — Cholesky of Sigma_raw with nearest-PD fallback
# Returns list(U_raw, kappa_raw, nearpd_applied)
# ---------------------------------------------------------------------------
ccc_step3_chol_raw <- function(Sigma_raw,
                               eps = 1e-10,
                               cond_warn = 1e8,
                               cond_abort = 1e12) {
	  res <- .ccc_safe_chol(Sigma_raw, eps = eps,
	                        cond_warn = cond_warn,
	                        cond_abort = cond_abort,
	                        label = "Sigma_raw",
	                        allow_nearpd = TRUE)
  list(
    U_raw          = res$U,         # upper-triangular: t(U) %*% U = Sigma
    kappa_raw      = res$kappa,
    nearpd_applied = res$nearpd_applied,
    Sigma_raw_pd   = res$M_pd
  )
}

# ---------------------------------------------------------------------------
# Step 4 — Receive Sigma_target + symmetrize + nearest-PD + Cholesky
# Sigma_target must be a p_fe x p_fe symmetric PSD matrix (external).
# In SWL Paper 2 it is the Rubin-pooled BRR-Fay sandwich:
#   Sigma_target = U_bar + (1 + 1/M) * B
# computed externally by assemble_target() — see helper below.
# ---------------------------------------------------------------------------
ccc_step4_chol_target <- function(Sigma_target, p_fe,
                                  eps = 1e-10,
                                  cond_warn = 1e8,
                                  cond_abort = 1e12,
                                  allow_target_nearpd = FALSE) {
  stopifnot(is.matrix(Sigma_target),
            nrow(Sigma_target) == p_fe,
            ncol(Sigma_target) == p_fe)
	  res <- .ccc_safe_chol(Sigma_target, eps = eps,
	                        cond_warn = cond_warn,
	                        cond_abort = cond_abort,
	                        label = "Sigma_target",
	                        allow_nearpd = allow_target_nearpd)
  list(
    U_target          = res$U,
    kappa_target      = res$kappa,
    nearpd_applied    = res$nearpd_applied,
    Sigma_target_pd   = res$M_pd
  )
}

# ---------------------------------------------------------------------------
# External assembly helper: Rubin-pooled BRR-Fay sandwich
# per_pv_results : list of length M, each element with $beta (p_fe vector)
#                  and $U (p_fe x p_fe BRR-Fay sandwich for that PV)
# Returns Sigma_target = U_bar + (1 + 1/M) * B
# ---------------------------------------------------------------------------
assemble_rubin_brrfay_target <- function(per_pv_results) {
  M <- length(per_pv_results)
  stopifnot(M >= 2L)

  beta_mat <- sapply(per_pv_results, `[[`, "beta")   # p_fe x M
  stopifnot(is.matrix(beta_mat) || length(beta_mat) >= M)
  if (!is.matrix(beta_mat)) beta_mat <- matrix(beta_mat, ncol = M)

  U_array  <- simplify2array(lapply(per_pv_results, `[[`, "U"))
  # dim: p_fe x p_fe x M

  U_bar  <- apply(U_array, c(1, 2), mean)
  # Between-imputation variance
  beta_centered <- beta_mat - rowMeans(beta_mat)
  B <- (beta_centered %*% t(beta_centered)) / (M - 1L)

  Sigma_target <- U_bar + (1 + 1/M) * B
  .ccc_symmetrize(Sigma_target)
}

# ---------------------------------------------------------------------------
# Step 5 — Calibration matrix A = L_target L_raw^{-1}
# R's chol() returns upper U with t(U) U = Sigma. Lower-triangular L = t(U).
# Thus L_target L_raw^{-1} = t(U_target) %*% solve(t(U_raw))
# We use backsolve / forwardsolve to avoid explicit inversion.
# Returns A (p_fe x p_fe).
# ---------------------------------------------------------------------------
ccc_step5_calibration_matrix <- function(U_raw, U_target) {
  # U_raw, U_target are upper-triangular: t(U) %*% U = Sigma.
  # Lower-triangular L = t(U) with L %*% t(L) = Sigma.
  L_raw <- t(U_raw)
  L_tgt <- t(U_target)

  # Goal: A = L_tgt %*% solve(L_raw)
  # Verify: A %*% Sigma_raw %*% t(A) = L_tgt L_raw^{-1} (L_raw L_raw^T) L_raw^{-T} L_tgt^T
  #       = L_tgt L_tgt^T = Sigma_target  ✓
  #
  # forwardsolve(L_raw, b) solves L_raw %*% x = b  =>  x = L_raw^{-1} %*% b
  # So L_raw^{-1} = forwardsolve(L_raw, diag(p))
  p <- nrow(L_raw)
  L_raw_inv <- forwardsolve(L_raw, diag(p))
  A <- L_tgt %*% L_raw_inv
  A
}

# ---------------------------------------------------------------------------
# Helper: Block-diagonal expansion of A (FE-only calibration enforcement).
# Returns A_full of size (p_fe + p_vc) x (p_fe + p_vc) with identity on VC.
# Caller uses this when needing the full-parameter representation; for the
# core calibrate-draws operation we use A directly on FE columns only.
# ---------------------------------------------------------------------------
.ccc_block_expand <- function(A_fe, p_vc) {
  p_fe <- nrow(A_fe)
  if (p_vc == 0L) return(A_fe)
  A_full <- matrix(0, p_fe + p_vc, p_fe + p_vc)
  A_full[seq_len(p_fe), seq_len(p_fe)] <- A_fe
  A_full[(p_fe + 1L):(p_fe + p_vc), (p_fe + 1L):(p_fe + p_vc)] <- diag(p_vc)
  A_full
}

# ---------------------------------------------------------------------------
# Step 6 — Apply calibration to FE draws + passthrough VC
#   psi^(s, cal) = psi_hat + A (psi^(s) - psi_hat)
# Returns list(draws_fe_cal, draws_cal_full)
# ---------------------------------------------------------------------------
ccc_step6_apply_calibration <- function(step1, A) {
  draws_fe <- step1$draws_fe
  psi_hat  <- step1$psi_hat
  # Two-pivot: rotate about psi_hat_pivot (empirical centroid), recenter at
  # psi_hat. Equal for string centers; differ only for external numeric center.
  psi_hat_pivot <- if (!is.null(step1$psi_hat_pivot)) step1$psi_hat_pivot else psi_hat

  centered <- sweep(draws_fe, 2L, psi_hat_pivot, FUN = "-")
  rotated  <- centered %*% t(A)
  draws_fe_cal <- sweep(rotated, 2L, psi_hat, FUN = "+")
  colnames(draws_fe_cal) <- colnames(draws_fe)

  if (step1$p_vc > 0L) {
    draws_cal_full <- cbind(draws_fe_cal, step1$draws_vc)
  } else {
    draws_cal_full <- draws_fe_cal
  }

  list(
    draws_fe_cal   = draws_fe_cal,
    draws_cal_full = draws_cal_full
  )
}

# ---------------------------------------------------------------------------
# Step 7 — Output stabilization
# Symmetrize + (optional) nearest-PD on the empirical covariance of calibrated
# FE draws. In practice (p_fe ~ 2-5, S >> 1000) repair is rarely triggered.
#
# A4 fix (056 Tier 0): the *raw* empirical covariance of the calibrated draws is
# always returned alongside the (possibly) PD-repaired matrix, so that downstream
# uncertainty (e.g. posterior SD / SE) can be read off the ACTUAL draws rather
# than a silently repaired matrix. Repair on the calibrated output is allowed by
# default (it only matters for a degenerate, rank-deficient draw matrix and is
# always reported), in contrast to the *target* covariance where repair is
# refused by default (a non-PD target signals a construction error; see Step 4).
#
# Returns list(Sigma_cal_emp, Sigma_cal_emp_raw, nearpd_applied,
#              min_eigenvalue, repair_resid)
# ---------------------------------------------------------------------------
ccc_step7_stabilize <- function(draws_fe_cal, eps = 1e-10, allow_nearpd = TRUE) {
  Sigma_cal_emp_raw <- .ccc_symmetrize(stats::cov(draws_fe_cal))
  min_ev <- min(eigen(Sigma_cal_emp_raw, symmetric = TRUE,
                       only.values = TRUE)$values)
  nearpd_applied <- FALSE
  Sigma_cal_emp <- Sigma_cal_emp_raw
  # A4 fix (056 Tier 0): an empirical covariance is PSD by construction, so a
  # rank-deficient draw matrix yields a smallest eigenvalue at the floating-
  # point floor (~+/-1e-16). Detect near-singularity with a positive tolerance
  # `eps` rather than the brittle `<= 0`, otherwise repair fires only on the
  # coin-flip sign of a ~1e-16 eigenvalue.
  if (min_ev <= eps) {
    if (!isTRUE(allow_nearpd)) {
      stop(sprintf(
        paste0("[ccc_step7_stabilize] calibrated-output covariance is non-PD ",
               "(min eigenvalue = %.3e); set allow_nearpd = TRUE to apply ",
               "nearest-PD repair, and inspect why the calibrated draws are ",
               "rank-deficient."),
        min_ev))
    }
    Sigma_cal_emp <- .ccc_nearpd(Sigma_cal_emp_raw, eps = eps)
    nearpd_applied <- TRUE
  }
  repair_resid <- if (nearpd_applied) {
    sqrt(sum((Sigma_cal_emp - Sigma_cal_emp_raw)^2))
  } else {
    0
  }
  list(
    Sigma_cal_emp     = Sigma_cal_emp,       # PD-safe (repaired only if needed)
    Sigma_cal_emp_raw = Sigma_cal_emp_raw,   # actual empirical cov of calibrated draws
    nearpd_applied    = nearpd_applied,
    min_eigenvalue    = min_ev,
    repair_resid      = repair_resid
  )
}

# ---------------------------------------------------------------------------
# Step 8 — Diagnostic ladder (Schema G2 fields)
# Returns list with:
#   delta_c_rel     : |A - I|_F / sqrt(p)
#   rho1            : |A Sigma_raw A^T - Sigma_target|_F / |Sigma_target|_F
#   rho2            : max_q |[A Sigma A^T - Sigma_T]_qq| / |[Sigma_T]_qq|
#   matrix_residual : raw A Sigma_raw A^T - Sigma_target (p x p)
#   kappa_raw, kappa_target, kappa_A
# ---------------------------------------------------------------------------
ccc_step8_diagnostics <- function(A, Sigma_raw, Sigma_target,
                                  kappa_raw = NA_real_,
                                  kappa_target = NA_real_) {
  p <- nrow(A)
  I_p <- diag(p)

  Sigma_post <- A %*% Sigma_raw %*% t(A)
  Sigma_post <- .ccc_symmetrize(Sigma_post)

  matrix_residual <- Sigma_post - Sigma_target
  fro_resid       <- sqrt(sum(matrix_residual^2))
  fro_target      <- sqrt(sum(Sigma_target^2))

  delta_c_rel <- sqrt(sum((A - I_p)^2)) / sqrt(p)
  rho1 <- if (fro_target > 0) fro_resid / fro_target else fro_resid
  rho2 <- max(abs(diag(matrix_residual)) /
              pmax(abs(diag(Sigma_target)), .Machine$double.eps))

  sv <- svd(A)$d
  kappa_A <- if (min(sv) > 0) max(sv) / min(sv) else Inf

  list(
    delta_c_rel     = delta_c_rel,
    rho1            = rho1,
    rho2            = rho2,
    matrix_residual = matrix_residual,
    kappa_raw       = kappa_raw,
    kappa_target    = kappa_target,
    kappa_A         = kappa_A
  )
}

# ---------------------------------------------------------------------------
# Integrated wrapper: ccc_twolevel()
# Combines Steps 1-8. Returns S3 object of class c("ccc_twolevel", "ccc_fit").
# ---------------------------------------------------------------------------
ccc_twolevel <- function(posterior_stacked,
                         Sigma_target,
                         param_map,
                         center = "posterior_mean",
                         control = list()) {
  # Allow numeric external center; skip match.arg for that case.
  if (!is.numeric(center)) {
    center <- match.arg(center, "posterior_mean")
  }
  ctl <- modifyList(list(
	    nearpd_eps   = 1e-10,
	    cond_warn    = 1e8,
	    cond_abort   = 1e12,
	    allow_target_nearpd = FALSE,   # non-PD target = construction error -> refuse
	    allow_cal_nearpd    = TRUE,    # non-PD calibrated output -> repair but report
	    production = FALSE,
	    raw_failure = "status",
	    diag_verbose = TRUE
  ), control)
  if (isTRUE(ctl$production)) ctl$raw_failure <- "error"
  ctl$raw_failure <- match.arg(as.character(ctl$raw_failure),
                               c("status", "error"))

  # Step 1: Assemble
  step1 <- ccc_step1_assemble(
    posterior_stacked = posterior_stacked,
    param_map         = param_map,
    center            = center
  )

  # Step 2: Raw covariance
  Sigma_raw <- ccc_step2_cov_raw(step1)

  # Step 3: Cholesky raw
  raw_error <- NULL
  step3 <- tryCatch(
    ccc_step3_chol_raw(Sigma_raw, eps = ctl$nearpd_eps,
                       cond_warn = ctl$cond_warn,
                       cond_abort = ctl$cond_abort),
    error = function(e) {
      raw_error <<- e
      NULL
    }
  )
  if (is.null(step3)) {
    raw_failure_message <- paste0(
      "Sigma_raw ill-conditioned; uncalibrated fallback draws are not ",
      "confirmatory estimates"
    )
    if (identical(ctl$raw_failure, "error")) {
      stop(
        "[ccc_twolevel] ", raw_failure_message,
        if (!is.null(raw_error)) paste0(": ", conditionMessage(raw_error)) else "",
        call. = FALSE
      )
    }
    return(structure(list(
      ccc_status   = "ill_conditioned",
      draws_cal    = step1$draws_fe,    # uncalibrated fallback
      step1        = step1,
      Sigma_raw    = Sigma_raw,
      Sigma_target = Sigma_target,
      message      = raw_failure_message,
      error_message = if (!is.null(raw_error)) conditionMessage(raw_error) else NA_character_
    ), class = c("ccc_twolevel", "ccc_fit")))
  }

  # Step 4: Cholesky target
	  step4 <- ccc_step4_chol_target(Sigma_target, p_fe = step1$p_fe,
	                                  eps = ctl$nearpd_eps,
	                                  cond_warn = ctl$cond_warn,
	                                  cond_abort = ctl$cond_abort,
	                                  allow_target_nearpd = ctl$allow_target_nearpd)

  # Step 5: Calibration matrix A
  A <- ccc_step5_calibration_matrix(step3$U_raw, step4$U_target)
  A_full <- .ccc_block_expand(A, step1$p_vc)

  # Step 6: Apply calibration
  step6 <- ccc_step6_apply_calibration(step1, A)

  # Step 7: Stabilization
  step7 <- ccc_step7_stabilize(step6$draws_fe_cal, eps = ctl$nearpd_eps,
                               allow_nearpd = ctl$allow_cal_nearpd)
  if (isTRUE(step7$nearpd_applied)) {
    warning(sprintf(
      paste0("[ccc_twolevel] calibrated-output covariance was non-PD ",
             "(min eigenvalue = %.3e) and was nearest-PD repaired ",
             "(Frobenius repair residual = %.3e). Reported SE should be read ",
             "from Sigma_cal_emp_raw (the actual draws' covariance)."),
      step7$min_eigenvalue, step7$repair_resid))
  }

  # Step 8: Diagnostics
  diag_out <- ccc_step8_diagnostics(
    A = A,
    Sigma_raw = step3$Sigma_raw_pd,
    Sigma_target = step4$Sigma_target_pd,
    kappa_raw = step3$kappa_raw,
    kappa_target = step4$kappa_target
  )

  # Flags
  flags <- list(
    nearpd_raw    = step3$nearpd_applied,
    nearpd_target = step4$nearpd_applied,
    nearpd_cal    = step7$nearpd_applied
  )

  structure(
    list(
      draws_calibrated = step6$draws_cal_full,
      draws_fe_cal     = step6$draws_fe_cal,
      A                = A,
      A_full           = A_full,
      psi_hat          = step1$psi_hat,
      Sigma_raw        = step3$Sigma_raw_pd,
      Sigma_target     = step4$Sigma_target_pd,
      Sigma_cal_emp    = step7$Sigma_cal_emp,
      Sigma_cal_emp_raw = step7$Sigma_cal_emp_raw,
      nearpd_cal_resid = step7$repair_resid,
      diagnostics      = diag_out,
      flags            = flags,
      param_map        = param_map,
      control          = ctl,
      ccc_status       = "ok"
    ),
    class = c("ccc_twolevel", "ccc_fit")
  )
}

# Print method
print.ccc_twolevel <- function(x, ...) {
  cat("<ccc_twolevel>\n")
  cat(sprintf("  status        : %s\n", x$ccc_status))
  if (x$ccc_status != "ok") {
    cat(sprintf("  message       : %s\n", x$message))
    return(invisible(x))
  }
  cat(sprintf("  p_fe          : %d  | p_vc : %d\n",
              length(x$param_map$fe_idx),
              length(x$param_map$vc_idx)))
  cat(sprintf("  S draws       : %d\n", nrow(x$draws_calibrated)))
  cat(sprintf("  delta_c_rel   : %.4e\n", x$diagnostics$delta_c_rel))
  cat(sprintf("  rho1          : %.4e\n", x$diagnostics$rho1))
  cat(sprintf("  rho2          : %.4e\n", x$diagnostics$rho2))
  cat(sprintf("  kappa(A)      : %.4e\n", x$diagnostics$kappa_A))
  if (any(unlist(x$flags))) {
    cat(sprintf("  nearPD flags  : raw=%s, target=%s, cal=%s\n",
                x$flags$nearpd_raw,
                x$flags$nearpd_target,
                x$flags$nearpd_cal))
    if (isTRUE(x$flags$nearpd_cal)) {
      cat(sprintf("  cal repair    : Frobenius residual = %.3e (SE read from raw draws cov)\n",
                  x$nearpd_cal_resid))
    }
  }
  invisible(x)
}

.ccc_contract_scalar <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(default)
  unname(as.numeric(x[[1L]]))
}

.ccc_contract_or <- function(x, y) {
  if (is.null(x)) y else x
}

.ccc_contract_term_value <- function(values, term, label) {
  if (is.null(values) || is.null(names(values)) || !term %in% names(values)) {
    stop(sprintf("[swl_ccc_confirmatory_result_row] missing %s for term %s",
                 label, term), call. = FALSE)
  }
  out <- unname(values[[term]])
  if (!is.finite(out)) {
    stop(sprintf("[swl_ccc_confirmatory_result_row] non-finite %s for term %s",
                 label, term), call. = FALSE)
  }
  out
}

.ccc_contract_diagnostics_json <- function(ccc_result,
                                           estimate_emitted = NA) {
  diagnostics <- .ccc_contract_or(ccc_result$diagnostics, list())
  flags <- .ccc_contract_or(ccc_result$flags, list())
  payload <- list(
    ccc_status = .ccc_contract_or(ccc_result$ccc_status, "missing"),
    message = .ccc_contract_or(ccc_result$message, NA_character_),
    estimate_emitted = estimate_emitted,
    rho1 = .ccc_contract_scalar(diagnostics$rho1),
    rho2 = .ccc_contract_scalar(diagnostics$rho2),
    kappa_raw = .ccc_contract_scalar(diagnostics$kappa_raw),
    kappa_target = .ccc_contract_scalar(diagnostics$kappa_target),
    kappa_A = .ccc_contract_scalar(diagnostics$kappa_A),
    nearpd_raw = isTRUE(flags$nearpd_raw),
    nearpd_target = isTRUE(flags$nearpd_target),
    nearpd_cal = isTRUE(flags$nearpd_cal)
  )
  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = 17)
}

# Emit a schema row from a CCC result without allowing non-ok CCC output to
# masquerade as a confirmatory estimate.
swl_ccc_confirmatory_result_row <- function(ccc_result,
                                            run_id,
                                            result_id,
                                            stage,
                                            dataset_id,
                                            estimand_id,
                                            term,
                                            seed,
                                            analysis_role = "confirmatory",
                                            estimand_label = NA_character_,
                                            n_obs = NA_integer_,
                                            pv_count = NA_integer_,
                                            runtime_sec = NA_real_,
                                            method_id = "c_direct",
                                            estimator_id = "long_single_fit",
                                            target_id = "ccc_brr_fay",
                                            engine_id = "brms_cmdstanr_long",
                                            provenance_json = NULL,
                                            notes = NA_character_,
                                            z_value = 1.96) {
  if (!exists("swl_schema_row", mode = "function", inherits = TRUE)) {
    stop(
      "[swl_ccc_confirmatory_result_row] source R/schema.R before emitting schema rows",
      call. = FALSE
    )
  }
  if (is.null(provenance_json)) {
    provenance_json <- jsonlite::toJSON(
      list(producer = "swl_ccc_confirmatory_result_row"),
      auto_unbox = TRUE
    )
  }

  common <- list(
    run_id = run_id,
    result_id = result_id,
    stage = stage,
    domain = "estimate",
    analysis_role = analysis_role,
    method_id = method_id,
    estimator_id = estimator_id,
    target_id = target_id,
    engine_id = engine_id,
    dataset_id = dataset_id,
    estimand_id = estimand_id,
    estimand_label = estimand_label,
    term = term,
    quantity = "estimate",
    n_obs = n_obs,
    pv_count = pv_count,
    seed = seed,
    runtime_sec = runtime_sec,
    provenance_json = provenance_json,
    notes = notes
  )

  target_repaired <- isTRUE(.ccc_contract_or(ccc_result$flags, list())$nearpd_target)
  if (!identical(ccc_result$ccc_status, "ok") || target_repaired) {
    note_bits <- c(
      if (target_repaired) {
        "No confirmatory estimate emitted because CCC target nearest-PD repair was applied."
      } else {
        "No confirmatory estimate emitted because CCC status is not ok."
      },
      as.character(notes)
    )
    note_bits <- note_bits[!is.na(note_bits) & nzchar(trimws(note_bits))]
    fail_notes <- paste(note_bits, collapse = " ")
    common$diagnostics_json <- .ccc_contract_diagnostics_json(
      ccc_result,
      estimate_emitted = FALSE
    )
    common$notes <- fail_notes
    return(do.call(swl_schema_row, c(common, list(
      value = NA_real_,
      std_error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      status = "fail"
    ))))
  }

  estimate <- .ccc_contract_term_value(ccc_result$psi_hat, term, "psi_hat")
  if (is.null(ccc_result$Sigma_cal_emp_raw) ||
      is.null(rownames(ccc_result$Sigma_cal_emp_raw)) ||
      is.null(colnames(ccc_result$Sigma_cal_emp_raw)) ||
      !term %in% rownames(ccc_result$Sigma_cal_emp_raw) ||
      !term %in% colnames(ccc_result$Sigma_cal_emp_raw)) {
    stop(sprintf(
      "[swl_ccc_confirmatory_result_row] missing Sigma_cal_emp_raw entry for term %s",
      term
    ), call. = FALSE)
  }
  variance <- unname(ccc_result$Sigma_cal_emp_raw[term, term])
  if (!is.finite(variance) || variance < 0) {
    stop(sprintf(
      "[swl_ccc_confirmatory_result_row] invalid variance for term %s",
      term
    ), call. = FALSE)
  }
  std_error <- sqrt(variance)
  common$diagnostics_json <- .ccc_contract_diagnostics_json(
    ccc_result,
    estimate_emitted = TRUE
  )

  do.call(swl_schema_row, c(common, list(
    value = estimate,
    std_error = std_error,
    ci_low = estimate - z_value * std_error,
    ci_high = estimate + z_value * std_error,
    status = "ok"
  )))
}
