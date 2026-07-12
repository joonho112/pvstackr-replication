# =============================================================================
# 07_psis_reporting_gate.R: Pipeline B PSIS diagnostics and reporting gate
# =============================================================================
#
# Purpose : Pipeline B. Turns the 20 archived per-PV PSIS log-ratio
#           diagnostics into the hard reporting gate: a country's numeric
#           estimates are releasable only if all 10 plausible values return
#           finite Pareto-k below 0.7, else the numeric columns are blanked
#           and marked computed_but_withheld (which withholds Korea). Also
#           recomputes Pareto-k from the archived log-ratios to confirm match.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_load_psis <- function() {
  x <- readRDS(file.path(PISA_PRECOMPUTED_DIR, "pipeline_b_psis_gate.rds"))
  pisa_assert(identical(x$schema_version, "pisa_pipeline_b_public_diagnostics_v1"),
              "Unexpected Pipeline B public schema.")
  pisa_assert(nrow(x$khat) == 20L, "PSIS evidence must have 20 rows.")
  counts <- table(factor(x$khat$country, levels = PISA_COUNTRIES))
  pisa_assert(all(counts == 10L), "Each country must have 10 PSIS diagnostics.")
  pisa_assert(all(x$khat$log_r_emitted) && all(x$khat$psis_called),
              "Both countries must have completed PSIS computation.")
  x
}

pisa_recompute_psis <- function(cache = pisa_load_psis()) {
  if (!requireNamespace("loo", quietly = TRUE)) stop("Package 'loo' is required.", call. = FALSE)
  rows <- list()
  at <- 1L
  for (country in PISA_COUNTRIES) {
    for (m in seq_along(cache$log_ratios[[country]])) {
      # High Pareto-k warnings are the evidence consumed by the gate, not an
      # unexpected runtime fault; retain the values and report them explicitly.
      fit <- suppressWarnings(loo::psis(cache$log_ratios[[country]][[m]]))
      k <- as.numeric(loo::pareto_k_values(fit))
      neff <- as.numeric(loo::psis_n_eff_values(fit))
      rows[[at]] <- data.frame(country = country, pv_index = m,
                               pareto_khat = k, psis_n_eff = neff,
                               psis_ok = is.finite(k) && k < PISA_KHAT_THRESHOLD,
                               stringsAsFactors = FALSE)
      at <- at + 1L
    }
  }
  do.call(rbind, rows)
}

pisa_apply_reporting_gate <- function(rows = NULL, khat = NULL,
                                      threshold = PISA_KHAT_THRESHOLD) {
  cache <- pisa_load_psis()
  if (is.null(rows)) rows <- cache$paper_rows
  if (is.null(khat)) khat <- cache$khat
  numeric_cols <- intersect(c("estimate", "std_error", "ci_low", "ci_high", "df"),
                            names(rows))
  for (country in unique(rows$country)) {
    z <- khat[khat$country == country, , drop = FALSE]
    allowed <- nrow(z) == 10L && all(z$psis_ok) && all(z$pareto_khat < threshold)
    rows$reporting_status[rows$country == country] <-
      if (allowed) "reportable" else "computed_but_withheld"
    if (!allowed) rows[rows$country == country, numeric_cols] <- NA_real_
  }
  rows
}

if (sys.nframe() == 0L) {
  x <- pisa_load_psis()
  recomputed <- pisa_recompute_psis(x)
  pisa_assert(max(abs(recomputed$pareto_khat - x$khat$pareto_khat)) < 1e-8,
              "Recomputed Pareto-k values differ from the archive.")
  print(x$gate)
  print(pisa_apply_reporting_gate()[, c("country", "term", "estimate",
                                        "std_error", "reporting_status")])
}
