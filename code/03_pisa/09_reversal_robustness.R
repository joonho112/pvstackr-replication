# =============================================================================
# 09_reversal_robustness.R: BRR-Fay, reversal, and robustness evidence
# =============================================================================
#
# Purpose : Secondary sensitivity track. Loads the deterministic public
#           tables (BRR-Fay design targets, the legacy reversal contrast, and
#           the 100-cell weighting-robustness grid) and reconstructs the
#           KOR-minus-USA between-ESCS reversal contrast from the country rows.
#           Documents that this country-gap reversal is secondary to the
#           primary within/between estimates and warrants no strong claim.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_load_sensitivity <- function() {
  read_public <- function(name) utils::read.csv(file.path(PISA_PRECOMPUTED_DIR, name),
                                                 stringsAsFactors = FALSE,
                                                 check.names = FALSE)
  out <- list(
    brr_target = read_public("brr_fay_target.csv"),
    brr_per_pv = read_public("brr_fay_per_pv.csv"),
    reversal = read_public("reversal_results.csv"),
    reversal_country = read_public("reversal_country_estimates.csv"),
    robustness = read_public("reversal_robustness_grid.csv"),
    robustness_country = read_public("reversal_robustness_country_estimates.csv"),
    robustness_summary = read_public("reversal_robustness_summary.csv"),
    secondary_summary = read_public("reversal_secondary_summary.csv")
  )
  pisa_assert(nrow(out$brr_target) == 6L && nrow(out$brr_per_pv) == 60L,
              "BRR-Fay target evidence is incomplete.")
  pisa_assert(nrow(out$reversal) == 2L, "Expected two legacy reversal rows.")
  pisa_assert(nrow(out$robustness) == 100L && nrow(out$robustness_country) == 342L,
              "Robustness grid is incomplete.")
  out
}

pisa_reversal_target_note <- function() {
  paste(
    "23.5817 is the legacy level-1 final-weight KOR-minus-USA contrast.",
    "The nearby 23.20 value belongs to a different target and must not be substituted.",
    "The reversal contrast is secondary; country-specific within/between estimates are primary."
  )
}

pisa_recompute_reversal <- function(country_rows = pisa_load_sensitivity()$reversal_country) {
  d <- country_rows[country_rows$term == "b_ESCS_between", , drop = FALSE]
  out <- lapply(unique(d$weighting), function(weighting) {
    z <- d[d$weighting == weighting, , drop = FALSE]
    usa <- z[z$country == "USA", , drop = FALSE]
    kor <- z[z$country == "KOR", , drop = FALSE]
    pisa_assert(nrow(usa) == 1L && nrow(kor) == 1L,
                paste("Expected one country row for", weighting))
    estimate <- kor$estimate - usa$estimate
    se <- sqrt(kor$se^2 + usa$se^2)
    df <- se^4 / (kor$se^4 / kor$df + usa$se^4 / usa$df)
    data.frame(weighting = weighting, country_pair = "KOR_minus_USA",
               gap_pct = 100 * estimate / usa$estimate,
               estimate = estimate, se = se, df = df,
               p_value = 2 * stats::pt(-abs(estimate / se), df = df),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

if (sys.nframe() == 0L) {
  x <- pisa_load_sensitivity()
  print(pisa_recompute_reversal(x$reversal_country))
  cat(pisa_reversal_target_note(), "\n")
}
