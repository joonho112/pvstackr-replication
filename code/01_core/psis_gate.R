# =============================================================================
# psis_gate.R: PSIS reporting gate (Pareto-k reportability)
# =============================================================================
#
# Purpose : Decide which reweighted (Pipeline B) estimates may be reported.
#           psis_reportability groups the PSIS diagnostics by country and
#           marks a country reportable only when every plausible value has a
#           Pareto-k below the threshold (default 0.7), otherwise
#           "computed_but_withheld". paper_facing_reweighted joins that gate
#           onto an estimate table and blanks the numeric columns for
#           non-reportable countries so withheld results cannot reach displays.
#           Inputs: a diagnostics data frame with country and Pareto-k columns,
#           and an estimate table. Output: a per-country gate table, or the
#           gated estimate table.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

psis_reportability <- function(diagnostics, threshold = 0.7,
                               country_col = "country",
                               khat_col = "pareto_k") {
  if (!is.data.frame(diagnostics)) stop("diagnostics must be a data frame", call. = FALSE)
  needed <- c(country_col, khat_col)
  if (!all(needed %in% names(diagnostics))) {
    stop("diagnostics lacks required country or Pareto-k columns", call. = FALSE)
  }
  k <- diagnostics[[khat_col]]
  if (!is.numeric(k) || anyNA(k) || any(!is.finite(k))) {
    stop("Pareto-k values must be finite numeric values", call. = FALSE)
  }
  by_country <- split(diagnostics, diagnostics[[country_col]])
  do.call(rbind, lapply(names(by_country), function(country) {
    x <- by_country[[country]]
    pass <- all(x[[khat_col]] < threshold)
    data.frame(
      country = country,
      n_pv = nrow(x),
      max_pareto_k = max(x[[khat_col]]),
      threshold = threshold,
      reportable = pass,
      status = if (pass) "reportable" else "computed_but_withheld",
      stringsAsFactors = FALSE
    )
  }))
}

paper_facing_reweighted <- function(estimates, diagnostics,
                                    threshold = 0.7) {
  gate <- psis_reportability(diagnostics, threshold = threshold)
  out <- merge(estimates, gate[, c("country", "reportable", "status")],
               by = "country", all.x = TRUE, sort = FALSE)
  if (anyNA(out$reportable)) stop("Every estimate country needs a gate result", call. = FALSE)
  numeric_result <- intersect(c("estimate", "se", "ci_low", "ci_high",
                                "difference", "se_ratio"), names(out))
  out[!out$reportable, numeric_result] <- NA_real_
  out
}
