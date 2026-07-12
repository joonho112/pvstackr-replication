#!/usr/bin/env Rscript
# =============================================================================
# verify_claims.R: Verify registered numeric anchors to tolerances
# =============================================================================
#
# Purpose : Recomputes headline quantities from the precomputed simulation,
#           method, and PISA tables under data/, then checks each registered
#           anchor in verification/expected/*-numeric-claims.csv against its
#           declared value and tolerance. Passes one disclosed unverified
#           manuscript statement, marks declared external-data contracts as not
#           observed, writes per-register reports, and fails closed on mismatch.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   - Simulation: condition, replication, and fit-row counts; max |bias|
#     over non-oracle strata; max R-hat; divergence total; MCMC-vs-frozen
#     archive parity for both slopes.
#   - Method and diagnostics: theorem-grid rows and max residual; OSM row
#     count; diagnostic reps, fit count, and minimum ESS; June reproduction
#     residual; PSIS pass counts; secondary reversal summaries.
#   - PISA: released and analytic sample cascade; slice column count;
#     Pipeline A and C R-hat, ESS, and divergences; Rubin df; PSIS k-hat.
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), ".."), mustWork = TRUE)
read_csv <- function(path) utils::read.csv(file.path(root, path), stringsAsFactors = FALSE, check.names = FALSE)

sim_design <- read_csv("data/precomputed/simulation/design.csv")
sim_rep <- read_csv("data/precomputed/simulation/parity_per_rep.csv")
sim_stratum <- read_csv("data/precomputed/simulation/parity_by_stratum.csv")
sim_diag <- read_csv("data/precomputed/simulation/diagnostics.csv")
cascade <- read_csv("data/precomputed/pisa/sample_cascade.csv")
slice_contract <- read_csv("data/pisa/slice-contract.csv")
khat <- read_csv("data/precomputed/pisa/psis_diagnostics.csv")
abc <- read_csv("data/precomputed/pisa/abc_estimates.csv")
theorem <- read_csv("data/precomputed/method/theorem_grid.csv")
june <- read_csv("data/precomputed/simulation/betaW_reproduction_check.csv")
reversal <- read_csv("data/precomputed/pisa/reversal_secondary_summary.csv")
paired <- merge(
  sim_rep[sim_rep$leg == "bayes_a", c("design_condition_id", "rep_id", "bW", "bB")],
  sim_rep[sim_rep$leg == "freq_per_pv", c("design_condition_id", "rep_id", "bW", "bB")],
  by = c("design_condition_id", "rep_id"), suffixes = c("_mcmc", "_freq")
)

main_actual <- c(
  sim_conditions = nrow(sim_design),
  sim_replications = nrow(unique(sim_rep[c("design_condition_id", "rep_id")])),
  sim_fit_rows = nrow(sim_rep),
  sim_max_abs_bias = max(abs(c(sim_stratum$biasW[sim_stratum$leg != "oracle"],
                               sim_stratum$biasB[sim_stratum$leg != "oracle"]))),
  sim_max_rhat = max(c(sim_diag$bayes_a_rhat_max, sim_diag$cdirect_rhat_max)),
  sim_divergences = sum(sim_diag$bayes_a_div) + sum(sim_diag$cdirect_div),
  mcmc_freq_archive_max_betaW = max(abs(paired$bW_mcmc - paired$bW_freq)),
  mcmc_freq_archive_max_betaB = max(abs(paired$bB_mcmc - paired$bB_freq)),
  pisa_rows = sum(cascade$released_students),
  pisa_columns = slice_contract$columns[[1L]],
  pisa_usa_released = cascade$released_students[cascade$country == "USA"],
  pisa_kor_released = cascade$released_students[cascade$country == "KOR"],
  pisa_usa_analytic = cascade$analytic_students[cascade$country == "USA"],
  pisa_kor_analytic = cascade$analytic_students[cascade$country == "KOR"],
  pisa_usa_khat_min = min(khat$pareto_khat[khat$country == "USA"]),
  pisa_usa_khat_max = max(khat$pareto_khat[khat$country == "USA"]),
  pisa_kor_khat_min = min(khat$pareto_khat[khat$country == "KOR"]),
  pisa_kor_khat_max = max(khat$pareto_khat[khat$country == "KOR"]),
  pisa_a_rhat_max = max(abc$rhat_max[abc$pipeline == "A"], na.rm = TRUE),
  pisa_a_ess_bulk_min = min(abc$ess_bulk_min[abc$pipeline == "A"], na.rm = TRUE),
  pisa_a_divergences = max(abc$divergences_total[abc$pipeline == "A"], na.rm = TRUE),
  pisa_usa_c_rhat_max = max(abc$rhat_max[abc$country == "USA" & abc$pipeline == "C"], na.rm = TRUE),
  pisa_usa_c_ess_bulk_min = min(abc$ess_bulk_min[abc$country == "USA" & abc$pipeline == "C"], na.rm = TRUE),
  pisa_kor_c_rhat_max = max(abc$rhat_max[abc$country == "KOR" & abc$pipeline == "C"], na.rm = TRUE),
  pisa_kor_c_ess_bulk_min = min(abc$ess_bulk_min[abc$country == "KOR" & abc$pipeline == "C"], na.rm = TRUE),
  pisa_c_divergences = max(abc$divergences[abc$pipeline == "C"], na.rm = TRUE),
  pisa_usa_a_df_within = abc$df[abc$country == "USA" & abc$pipeline == "A" & abc$term == "b_ESCS_within"],
  pisa_usa_a_df_between = abc$df[abc$country == "USA" & abc$pipeline == "A" & abc$term == "b_ESCS_between"],
  pisa_kor_a_df_within = abc$df[abc$country == "KOR" & abc$pipeline == "A" & abc$term == "b_ESCS_within"],
  pisa_kor_a_df_between = abc$df[abc$country == "KOR" & abc$pipeline == "A" & abc$term == "b_ESCS_between"]
)
osm_actual <- c(
  theorem_grid_rows = nrow(theorem),
  theorem_grid_residual = max(theorem$delta),
  osm_simulation_rows = nrow(read_csv("output/tables/osm_simulation_results.csv")),
  diagnostic_reps = nrow(sim_diag),
  diagnostic_fits = 2L * sum(sim_design$M + 1L),
  diagnostic_min_ess = min(sim_diag$bayes_a_ess_min),
  june_reproduction = max(abs(june$dMeanW)),
  usa_psis_pass = sum(khat$country == "USA" & khat$pareto_khat < .7),
  kor_psis_pass = sum(khat$country == "KOR" & khat$pareto_khat < .7),
  reversal_level1 = reversal$estimate[reversal$summary_row_id == "REV-LEVEL1-FINAL-WEIGHT"],
  reversal_staged = reversal$estimate[reversal$summary_row_id == "REV-DECLARED-SNS01"]
)

evaluate_claims <- function(file, actuals) {
  x <- read_csv(file)
  x$actual <- NA_real_
  x$pass <- FALSE
  x$verification_status <- ""
  for (i in seq_len(nrow(x))) {
    id <- x$claim_id[[i]]
    if (x$status[[i]] == "unverified manuscript statement") {
      x$pass[[i]] <- TRUE
      x$verification_status[[i]] <- "disclosed_source_issue"
      next
    }
    if (!id %in% names(actuals)) stop("No evaluator for claim: ", id, call. = FALSE)
    actual <- unname(actuals[[id]])
    expected <- suppressWarnings(as.numeric(x$expected[[i]]))
    tolerance <- suppressWarnings(as.numeric(x$tolerance[[i]]))
    if (!is.finite(tolerance)) tolerance <- 0
    x$actual[[i]] <- actual
    x$pass[[i]] <- is.finite(actual) && is.finite(expected) && abs(actual - expected) <= tolerance
    x$verification_status[[i]] <- if (x$pass[[i]]) {
      if (x$status[[i]] == "declared_external_data_contract") {
        "declared_external_contract_not_observed"
      } else {
        "verified_numeric"
      }
    } else "numeric_mismatch"
  }
  x
}

main <- evaluate_claims("verification/expected/main-numeric-claims.csv", main_actual)
osm <- evaluate_claims("verification/expected/osm-numeric-claims.csv", osm_actual)
dir.create(file.path(root, "verification", "reports"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(main, file.path(root, "verification", "reports", "main-claims.csv"), row.names = FALSE)
utils::write.csv(osm, file.path(root, "verification", "reports", "osm-claims.csv"), row.names = FALSE)
if (any(!main$pass) || any(!osm$pass)) {
  stop("Claim verification failed: ", paste(c(main$claim_id[!main$pass], osm$claim_id[!osm$pass]), collapse = ", "), call. = FALSE)
}
cat(sprintf("Claim verification PASS: %d main and %d OSM anchors; one disclosed unverified manuscript claim.\n", nrow(main), nrow(osm)))
