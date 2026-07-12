# =============================================================================
# targets.R: PISA-style BRR-Fay design-based target
# =============================================================================
#
# Purpose : Build the external design-based covariance target that CCC
#           (ccc.R) calibrates the stacked posterior toward. For each
#           plausible value it refits the model under the full-sample weight
#           and every Fay-adjusted BRR replicate weight, forms the replicate
#           design variance U, then pools across plausible values by Rubin's
#           rules to give T_MI = U_bar + (1 + 1/M) B. Helpers detect PISA PV
#           and replicate-weight columns and hash the design fixture for
#           provenance. Output: the pooled target (beta, U_bar, B, T_MI, df)
#           and supporting metadata.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   assemble_pisa_brr_fay_target: Rubin-pooled BRR-Fay target across PVs;
#     returns T_MI, the external covariance target used by CCC calibration.
#   brr_fay_U_one_pv: Fay-adjusted BRR design variance for one PV outcome.
#   detect_pisa_pv_columns: detect PV columns by contiguous numeric suffix.
#   detect_pisa_brr_replicate_weights: detect BRR replicate-weight columns.
#   pisa_brr_fay_design_metadata: summarize + SHA-256 hash the design fixture.
#   validate_pisa_brr_fay_metadata: enforce the design-metadata contract.
# =============================================================================

.pisa_brr_missing_cols <- function(data, cols) {
  cols[!cols %in% names(data)]
}

.pisa_brr_check_cols <- function(data, cols, label) {
  missing <- .pisa_brr_missing_cols(data, cols)
  if (length(missing)) {
    stop(sprintf("[pisa_brr_fay] missing %s columns: %s",
                 label, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

.pisa_brr_check_weight <- function(w, label, n) {
  if (!is.numeric(w) || !is.null(dim(w)) || length(w) != n ||
      anyNA(w) || any(!is.finite(w))) {
    stop(sprintf("[pisa_brr_fay] %s must be a finite numeric vector of length n",
                 label), call. = FALSE)
  }
  if (any(w <= 0)) {
    stop(sprintf("[pisa_brr_fay] %s must be strictly positive", label),
         call. = FALSE)
  }
  as.numeric(w)
}

.pisa_brr_check_fay_k <- function(fay_k) {
  if (!is.numeric(fay_k) || length(fay_k) != 1L ||
      !is.finite(fay_k) || fay_k < 0 || fay_k >= 1) {
    stop("[pisa_brr_fay] fay_k must be a finite scalar with 0 <= fay_k < 1",
         call. = FALSE)
  }
  fay_k
}

.pisa_brr_check_unique <- function(cols, label) {
  if (anyDuplicated(cols)) {
    stop(sprintf("[pisa_brr_fay] %s names must be unique", label),
         call. = FALSE)
  }
  invisible(cols)
}

.pisa_brr_check_pv <- function(x, label, n) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != n ||
      anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("[pisa_brr_fay] %s must be a finite numeric vector of length n",
                 label), call. = FALSE)
  }
  as.numeric(x)
}

.pisa_brr_hash_payload <- function(payload) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(payload, algo = "sha256")
  } else {
    paste(utils::capture.output(str(payload)), collapse = "\n")
  }
}

.pisa_brr_natural_prefixed_cols <- function(data, prefix, suffix, label) {
  if (!is.data.frame(data)) {
    stop("[pisa_brr_fay] data must be a data.frame", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L ||
      is.na(prefix) || !nzchar(prefix)) {
    stop(sprintf("[pisa_brr_fay] %s prefix must be a non-empty string", label),
         call. = FALSE)
  }
  if (!is.character(suffix) || length(suffix) != 1L || is.na(suffix)) {
    stop(sprintf("[pisa_brr_fay] %s suffix must be a string", label),
         call. = FALSE)
  }
  escape_rx <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  rx <- paste0("^", escape_rx(prefix), "([0-9]+)", escape_rx(suffix), "$")
  hits <- grep(rx, names(data), value = TRUE)
  if (!length(hits)) {
    stop(sprintf("[pisa_brr_fay] no %s columns detected with prefix %s",
                 label, prefix), call. = FALSE)
  }
  suffix <- as.integer(sub(rx, "\\1", hits))
  if (anyDuplicated(suffix)) {
    stop(sprintf("[pisa_brr_fay] duplicate %s numeric suffixes detected",
                 label), call. = FALSE)
  }
  ordered_suffix <- sort(suffix)
  if (!identical(ordered_suffix, seq_len(length(ordered_suffix)))) {
    stop(sprintf("[pisa_brr_fay] %s numeric suffixes must be contiguous from 1",
                 label), call. = FALSE)
  }
  hits[order(suffix)]
}

#' Detect PISA-style plausible-value columns by numeric suffix
#'
#' @param data data.frame containing plausible-value columns.
#' @param prefix Column prefix. The default detects `PV1`, `PV2`, ...
#' @param suffix Optional subject suffix, for example `READ` in `PV1READ`.
#' @param expected_M Optional required plausible-value count.
#' @return Character vector in natural numeric order.
detect_pisa_pv_columns <- function(data,
                                   prefix = "PV",
                                   suffix = "",
                                   expected_M = NULL) {
  cols <- .pisa_brr_natural_prefixed_cols(data, prefix, suffix, "PV")
  if (!is.null(expected_M) && length(cols) != expected_M) {
    stop(sprintf("[pisa_brr_fay] expected %d PV columns, detected %d",
                 expected_M, length(cols)), call. = FALSE)
  }
  cols
}

#' Detect PISA-style BRR replicate-weight columns by numeric suffix
#'
#' @param data data.frame containing replicate-weight columns.
#' @param prefix Column prefix. The default detects `W_FSTURWT1`, ...
#' @param expected_R Optional required replicate count.
#' @return Character vector in natural numeric order.
detect_pisa_brr_replicate_weights <- function(data,
                                              prefix = "W_FSTURWT",
                                              expected_R = NULL) {
  cols <- .pisa_brr_natural_prefixed_cols(data, prefix, "", "replicate weight")
  if (!is.null(expected_R) && length(cols) != expected_R) {
    stop(sprintf("[pisa_brr_fay] expected %d replicate weights, detected %d",
                 expected_R, length(cols)), call. = FALSE)
  }
  cols
}

#' Summarize and hash a PISA-style BRR-Fay design fixture
#'
#' This helper is deliberately metadata-only: it verifies that the declared
#' PVs, full-sample weight, replicate weights, and row support are present and
#' hashable before model fitting starts.
#'
#' @return List with counts, declared columns, Fay coefficient, and deterministic
#'   SHA-256 hashes for row support, PV values, and weight design.
pisa_brr_fay_design_metadata <- function(data,
                                         pv_cols = NULL,
                                         base_weight = "W_FSTUWT",
                                         rep_weight_cols = NULL,
                                         fay_k = 0.5,
                                         expected_M = NULL,
                                         expected_R = NULL,
                                         pv_prefix = "PV",
                                         pv_suffix = "",
                                         rep_weight_prefix = "W_FSTURWT",
                                         id_cols = NULL,
                                         data_origin = "synthetic",
                                         claims_scope = "synthetic_pisa_shape_only",
                                         replicate_scheme = "BRR-Fay",
                                         generator_seed = NULL,
                                         claims_not_supported = c(
                                           "real PISA design validity",
                                           "OECD/EdSurvey agreement",
                                           "country estimates"
                                         )) {
  if (!is.data.frame(data)) {
    stop("[pisa_brr_fay] data must be a data.frame", call. = FALSE)
  }
  fay_k <- .pisa_brr_check_fay_k(fay_k)
  if (is.null(pv_cols)) {
    pv_cols <- detect_pisa_pv_columns(data, prefix = pv_prefix,
                                      suffix = pv_suffix,
                                      expected_M = expected_M)
  }
  if (is.null(rep_weight_cols)) {
    rep_weight_cols <- detect_pisa_brr_replicate_weights(
      data,
      prefix = rep_weight_prefix,
      expected_R = expected_R
    )
  }
  if (!is.character(pv_cols) || !length(pv_cols) || anyNA(pv_cols)) {
    stop("[pisa_brr_fay] pv_cols must be a non-empty character vector",
         call. = FALSE)
  }
  if (!is.character(rep_weight_cols) || !length(rep_weight_cols) ||
      anyNA(rep_weight_cols)) {
    stop("[pisa_brr_fay] rep_weight_cols must be a non-empty character vector",
         call. = FALSE)
  }
  if (!is.character(base_weight) || length(base_weight) != 1L ||
      is.na(base_weight) || !nzchar(base_weight)) {
    stop("[pisa_brr_fay] base_weight must be a non-empty string",
         call. = FALSE)
  }
  if (!is.null(expected_M) && length(pv_cols) != expected_M) {
    stop(sprintf("[pisa_brr_fay] expected %d PV columns, received %d",
                 expected_M, length(pv_cols)), call. = FALSE)
  }
  if (!is.null(expected_R) && length(rep_weight_cols) != expected_R) {
    stop(sprintf("[pisa_brr_fay] expected %d replicate weights, received %d",
                 expected_R, length(rep_weight_cols)), call. = FALSE)
  }
  if (!is.null(id_cols) && (!is.character(id_cols) || anyNA(id_cols))) {
    stop("[pisa_brr_fay] id_cols must be character column names", call. = FALSE)
  }
  .pisa_brr_check_unique(pv_cols, "PV")
  .pisa_brr_check_unique(rep_weight_cols, "replicate weight")
  if (!is.null(id_cols)) .pisa_brr_check_unique(id_cols, "row id")

  .pisa_brr_check_cols(data, c(id_cols, pv_cols, base_weight, rep_weight_cols),
                       "metadata")
  n <- nrow(data)
  for (pv in pv_cols) .pisa_brr_check_pv(data[[pv]], pv, n)
  .pisa_brr_check_weight(data[[base_weight]], base_weight, n)
  for (rw in rep_weight_cols) .pisa_brr_check_weight(data[[rw]], rw, n)

  row_key <- if (is.null(id_cols)) {
    data.frame(.row = seq_len(n))
  } else {
    data[id_cols]
  }
  if (anyNA(row_key)) {
    stop("[pisa_brr_fay] row id columns must not contain missing values",
         call. = FALSE)
  }
  if (anyDuplicated(row_key)) {
    stop("[pisa_brr_fay] row id columns must uniquely identify rows",
         call. = FALSE)
  }
  row_set_key <- row_key[do.call(order, row_key), , drop = FALSE]
  row_support <- data.frame(
    row_key,
    pv_complete = stats::complete.cases(data[pv_cols]),
    weight_complete = stats::complete.cases(data[c(base_weight, rep_weight_cols)]),
    stringsAsFactors = FALSE
  )
  list(
    n = n,
    M = length(pv_cols),
    R = length(rep_weight_cols),
    fay_k = fay_k,
    fay_variance_multiplier = 1 / (length(rep_weight_cols) * (1 - fay_k)^2),
    replicate_weight_role = "external_design_variance_only",
    pipeline_b_final_replicates_allowed = FALSE,
    synthetic_fixture = TRUE,
    data_origin = data_origin,
    claims_scope = claims_scope,
    replicate_scheme = replicate_scheme,
    generator_seed = generator_seed,
    claims_not_supported = claims_not_supported,
    pv_cols = pv_cols,
    base_weight = base_weight,
    rep_weight_cols = rep_weight_cols,
    id_cols = if (is.null(id_cols)) character(0) else id_cols,
    row_order_hash = .pisa_brr_hash_payload(row_key),
    row_set_hash = .pisa_brr_hash_payload(row_set_key),
    row_support_hash = .pisa_brr_hash_payload(row_support),
    pv_value_hash = .pisa_brr_hash_payload(data[pv_cols]),
    weight_design_hash = .pisa_brr_hash_payload(data[c(base_weight, rep_weight_cols)]),
    weight_metadata_hash = .pisa_brr_hash_payload(list(
      base_weight = base_weight,
      rep_weight_cols = rep_weight_cols,
      R = length(rep_weight_cols),
      fay_k = fay_k,
      fay_variance_multiplier = 1 / (length(rep_weight_cols) * (1 - fay_k)^2),
      replicate_scheme = replicate_scheme,
      row_order_hash = .pisa_brr_hash_payload(row_key),
      row_set_hash = .pisa_brr_hash_payload(row_set_key),
      generator_seed = generator_seed
    ))
  )
}

validate_pisa_brr_fay_metadata <- function(metadata,
                                           expected_M = NULL,
                                           expected_R = NULL,
                                           fay_k = NULL) {
  if (!is.list(metadata)) {
    stop("[pisa_brr_fay] metadata must be a list", call. = FALSE)
  }
  required <- c(
    "n", "M", "R", "fay_k", "fay_variance_multiplier",
    "replicate_weight_role", "pipeline_b_final_replicates_allowed",
    "synthetic_fixture", "data_origin", "claims_scope", "replicate_scheme",
    "claims_not_supported", "pv_cols", "base_weight", "rep_weight_cols",
    "id_cols", "row_order_hash", "row_set_hash", "row_support_hash",
    "pv_value_hash", "weight_design_hash", "weight_metadata_hash"
  )
  missing <- setdiff(required, names(metadata))
  if (length(missing)) {
    stop("[pisa_brr_fay] missing metadata fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.null(expected_M) && !identical(as.integer(metadata$M), as.integer(expected_M))) {
    stop(sprintf("[pisa_brr_fay] expected metadata M=%d, found %s",
                 expected_M, metadata$M), call. = FALSE)
  }
  if (!is.null(expected_R) && !identical(as.integer(metadata$R), as.integer(expected_R))) {
    stop(sprintf("[pisa_brr_fay] expected metadata R=%d, found %s",
                 expected_R, metadata$R), call. = FALSE)
  }
  if (!is.null(fay_k) && !isTRUE(all.equal(as.numeric(metadata$fay_k), fay_k,
                                          tolerance = 0))) {
    stop(sprintf("[pisa_brr_fay] expected metadata fay_k=%s, found %s",
                 fay_k, metadata$fay_k), call. = FALSE)
  }
  expected_mult <- 1 / (as.numeric(metadata$R) * (1 - as.numeric(metadata$fay_k))^2)
  if (!isTRUE(all.equal(as.numeric(metadata$fay_variance_multiplier),
                        expected_mult, tolerance = 1e-15))) {
    stop("[pisa_brr_fay] metadata Fay variance multiplier is inconsistent",
         call. = FALSE)
  }
  if (!identical(metadata$replicate_weight_role,
                 "external_design_variance_only")) {
    stop("[pisa_brr_fay] replicate weights must be external design variance only",
         call. = FALSE)
  }
  if (!identical(metadata$pipeline_b_final_replicates_allowed, FALSE)) {
    stop("[pisa_brr_fay] BRR replicate weights are not allowed in Pipeline B final likelihood",
         call. = FALSE)
  }
  if (!identical(metadata$synthetic_fixture, TRUE) ||
      !identical(metadata$data_origin, "synthetic") ||
      !identical(metadata$claims_scope, "synthetic_pisa_shape_only")) {
    stop("[pisa_brr_fay] metadata must be labelled synthetic_pisa_shape_only",
         call. = FALSE)
  }
  if (!identical(metadata$replicate_scheme, "BRR-Fay")) {
    stop("[pisa_brr_fay] metadata replicate_scheme must be BRR-Fay",
         call. = FALSE)
  }
  hashes <- c(metadata$row_order_hash, metadata$row_set_hash,
              metadata$row_support_hash, metadata$pv_value_hash,
              metadata$weight_design_hash, metadata$weight_metadata_hash)
  if (!all(grepl("^[a-f0-9]{64}$", hashes))) {
    stop("[pisa_brr_fay] metadata hashes must be SHA-256 hex strings",
         call. = FALSE)
  }
  TRUE
}

.pisa_brr_formula_chr <- function(base_formula, pv_col) {
  formula_chr <- paste(deparse(base_formula, width.cutoff = 500L), collapse = "")
  matches <- gregexpr("OUTCOME", formula_chr, fixed = TRUE)[[1L]]
  n_matches <- if (length(matches) == 1L && matches[[1L]] == -1L) 0L else length(matches)
  if (n_matches != 1L) {
    stop("[pisa_brr_fay] base_formula must contain exactly one OUTCOME placeholder",
         call. = FALSE)
  }
  sub("OUTCOME", pv_col, formula_chr, fixed = TRUE)
}

# Internal: one lme4 weighted REML fit -> fixed-effect vector.
.brrfay_fixef <- function(data, formula_chr, weight_vec) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("[pisa_brr_fay] lme4 is required for BRR-Fay replicate estimation",
         call. = FALSE)
  }
  d <- data
  d$.brr_w <- .pisa_brr_check_weight(weight_vec, "weight_vec", nrow(d))
  fit <- lme4::lmer(
    stats::as.formula(formula_chr),
    data = d,
    weights = d$.brr_w,
    REML = TRUE,
    control = lme4::lmerControl(calc.derivs = FALSE,
                                check.conv.singular = "ignore",
                                check.conv.grad = "ignore")
  )
  lme4::fixef(fit)
}

#' BRR-Fay design variance for one plausible-value outcome
#'
#' @param data analytic data.frame.
#' @param base_formula formula with an `OUTCOME` placeholder, e.g.
#'   `OUTCOME ~ x + (1 | school)`.
#' @param pv_col plausible-value outcome column to substitute for `OUTCOME`.
#' @param base_weight full-sample weight column.
#' @param rep_weight_cols replicate-weight columns.
#' @param fay_k Fay coefficient. `0` gives standard BRR; PISA uses `0.5`.
#' @return list with fixed-effect point estimate `beta`, design variance `U`,
#'   fixed-effect names, replicate count, and Fay coefficient.
brr_fay_U_one_pv <- function(data,
                             base_formula,
                             pv_col,
                             base_weight,
                             rep_weight_cols,
                             fay_k = 0.5) {
  if (!is.data.frame(data)) {
    stop("[pisa_brr_fay] data must be a data.frame", call. = FALSE)
  }
  fay_k <- .pisa_brr_check_fay_k(fay_k)
  R <- length(rep_weight_cols)
  if (R < 2L) stop("[pisa_brr_fay] need >= 2 replicate weights", call. = FALSE)
  .pisa_brr_check_unique(c(pv_col, base_weight, rep_weight_cols),
                         "PV/base/replicate input")
  .pisa_brr_check_cols(data, c(pv_col, base_weight, rep_weight_cols), "input")

  n <- nrow(data)
  base_w <- .pisa_brr_check_weight(data[[base_weight]], base_weight, n)
  rep_weights <- lapply(rep_weight_cols, function(col) {
    .pisa_brr_check_weight(data[[col]], col, n)
  })

  formula_chr <- .pisa_brr_formula_chr(base_formula, pv_col)
  beta0 <- .brrfay_fixef(data, formula_chr, base_w)
  p <- length(beta0)

  D <- matrix(NA_real_, nrow = p, ncol = R)
  for (r in seq_len(R)) {
    br <- .brrfay_fixef(data, formula_chr, rep_weights[[r]])
    if (!identical(names(br), names(beta0))) {
      stop("[pisa_brr_fay] fixed-effect names differ across replicate fits",
           call. = FALSE)
    }
    D[, r] <- br - beta0
  }
  mult <- 1 / (R * (1 - fay_k)^2)
  U <- mult * tcrossprod(D)
  U <- 0.5 * (U + t(U))

  fe_names <- paste0("b_", names(beta0))
  fe_names[fe_names == "b_(Intercept)"] <- "b_Intercept"
  names(beta0) <- fe_names
  dimnames(D) <- list(fe_names, rep_weight_cols)
  replicate_beta <- sweep(D, 1L, beta0, FUN = "+")
  dimnames(U) <- list(fe_names, fe_names)

  list(
    beta = beta0,
    U = U,
    fe_names = fe_names,
    R = R,
    fay_k = fay_k,
    replicate_beta = replicate_beta,
    replicate_diff = D
  )
}

#' Assemble Rubin-pooled BRR-Fay target across plausible values
#'
#' @return list with `beta`, `U_bar`, `B`, `T_MI`, `df`, `fe_names`, `per_pv`,
#'   `M`, `R`, and `fay_k`. `T_MI` is the external design-based covariance
#'   target used by CCC-style calibration.
assemble_pisa_brr_fay_target <- function(data,
                                         base_formula,
                                         pv_cols,
                                         base_weight,
                                         rep_weight_cols,
                                         fay_k = 0.5,
                                         verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("[pisa_brr_fay] data must be a data.frame", call. = FALSE)
  }
  fay_k <- .pisa_brr_check_fay_k(fay_k)
  M <- length(pv_cols)
  if (M < 1L) stop("[pisa_brr_fay] need >= 1 PV column", call. = FALSE)
  if (length(rep_weight_cols) < 2L) {
    stop("[pisa_brr_fay] need >= 2 replicate weights", call. = FALSE)
  }
  .pisa_brr_check_unique(pv_cols, "PV")
  .pisa_brr_check_unique(c(base_weight, rep_weight_cols),
                         "base/replicate weight")
  .pisa_brr_check_cols(data, c(pv_cols, base_weight, rep_weight_cols), "input")

  per_pv <- vector("list", M)
  for (m in seq_len(M)) {
    if (isTRUE(verbose)) {
      message(sprintf("[pisa_brr_fay] PV %d/%d (%s) : %d replicates",
                      m, M, pv_cols[m], length(rep_weight_cols)))
    }
    per_pv[[m]] <- brr_fay_U_one_pv(data, base_formula, pv_cols[m],
                                    base_weight, rep_weight_cols, fay_k)
  }

  fe_names <- per_pv[[1L]]$fe_names
  if (!all(vapply(per_pv, function(x) identical(x$fe_names, fe_names),
                  logical(1)))) {
    stop("[pisa_brr_fay] fixed-effect names differ across plausible values",
         call. = FALSE)
  }
  p <- length(fe_names)
  betas <- vapply(per_pv, function(x) x$beta, numeric(p))
  if (is.null(dim(betas))) betas <- matrix(betas, nrow = p)

  beta_bar <- rowMeans(betas)
  U_bar <- Reduce(`+`, lapply(per_pv, `[[`, "U")) / M

  if (M > 1L) {
    Bc <- betas - beta_bar
    B <- (Bc %*% t(Bc)) / (M - 1L)
  } else {
    B <- matrix(0, p, p)
  }
  T_MI <- U_bar + (1 + 1 / M) * B
  T_MI <- 0.5 * (T_MI + t(T_MI))

  dimnames(U_bar) <- list(fe_names, fe_names)
  dimnames(B) <- list(fe_names, fe_names)
  dimnames(T_MI) <- list(fe_names, fe_names)

  eps <- .Machine$double.eps
  rho <- (1 + 1 / M) * diag(B) / pmax(diag(T_MI), eps)
  df_mi <- (M - 1L) / pmax(rho^2, eps)

  names(beta_bar) <- fe_names
  names(df_mi) <- fe_names
  list(beta = beta_bar, U_bar = U_bar, B = B, T_MI = T_MI,
       df = df_mi, fe_names = fe_names, per_pv = per_pv,
       M = M, R = length(rep_weight_cols), fay_k = fay_k)
}
