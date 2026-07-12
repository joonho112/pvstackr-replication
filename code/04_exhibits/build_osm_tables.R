# =============================================================================
# build_osm_tables.R: emit the online-supplement tables from shipped evidence
# =============================================================================
#
# Purpose : Build the online supplement (OSM) CSV tables from shipped
#           precomputed evidence: the full simulation design, the per-condition
#           parity results (72 rows across three estimators), the PISA PSIS
#           Pareto k-hat diagnostics, and, when present, the PISA sample
#           cascade and the weighting-reversal summary.
#           Inputs : data/precomputed/{simulation,pisa}/*.csv.
#           Outputs: output/tables/osm_*.csv.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
build_osm_tables <- function() {
  out <- project_path("output", "tables")
  d <- .read_first(c(project_path("data", "precomputed", "simulation", "design.csv")))
  if ("route" %in% names(d)) d <- d[d$route == "gaussian", , drop = FALSE]
  utils::write.csv(d, file.path(out, "osm_simulation_design.csv"), row.names = FALSE)

  pc <- .read_first(c(project_path("data", "precomputed", "simulation", "parity_by_condition.csv")))
  pc <- pc[pc$leg %in% c("bayes_a", "freq_per_pv", "c_direct"), ]
  stopifnot(nrow(pc) == 72L)
  utils::write.csv(pc, file.path(out, "osm_simulation_results.csv"), row.names = FALSE)

  kh <- .read_first(c(project_path("data", "precomputed", "pisa", "pisa_psis_diagnostics.csv"),
                      project_path("data", "precomputed", "pisa", "pisa_pipeline_b_pareto_k_phase7r.csv"),
                      project_path("data", "precomputed", "pisa", "psis_diagnostics.csv")))
  utils::write.csv(kh, file.path(out, "osm_psis_diagnostics.csv"), row.names = FALSE)

  cascade_file <- project_path("data", "precomputed", "pisa", "pisa_sample_cascade.csv")
  if (!file.exists(cascade_file)) cascade_file <- project_path("data", "precomputed", "pisa", "sample_cascade.csv")
  if (file.exists(cascade_file)) {
    utils::write.csv(utils::read.csv(cascade_file, check.names = FALSE),
                     file.path(out, "osm_pisa_cascade.csv"), row.names = FALSE)
  }

  reversal_file <- project_path("data", "precomputed", "pisa", "pisa_reversal_summary.csv")
  if (!file.exists(reversal_file)) reversal_file <- project_path("data", "precomputed", "pisa", "reversal_secondary_summary.csv")
  if (file.exists(reversal_file)) {
    rev <- utils::read.csv(reversal_file, check.names = FALSE)
    utils::write.csv(rev, file.path(out, "osm_pisa_reversal.csv"), row.names = FALSE)
  }
  invisible(TRUE)
}
