#!/usr/bin/env Rscript
# =============================================================================
# build_simulation_inputs.R: Numeric inputs for the manuscript exhibits
# =============================================================================
#
# Purpose : Turn the precomputed evidence bundle into the numeric input CSVs
#           for the manuscript exhibits: Table 2 (design), Table 3 (headline
#           stratum results), Figure 2 (per-condition plot data for both
#           slopes), and OSM Section E (per-condition results). Recomputes the
#           aggregates, writes each CSV atomically, and checks the row counts.
#           Outputs: table2_design_input.csv, table3_headline_input.csv,
#           figure2_plot_data.csv, and osm_e_condition_results.csv.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.exhibit_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(path, "code", "02_simulation",
                              "simulation_utils.R"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  stop("Run this script from inside the replication package.", call. = FALSE)
}
root <- .exhibit_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(root, "code", "02_simulation", "aggregate.R"), local = FALSE)
args <- sim_parse_named_args()
output <- args$out %||% file.path(root, "output", "results", "simulation-inputs")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
bundle <- readRDS(file.path(root, "data", "precomputed", "simulation",
                            "simulation_evidence.rds"))
fresh <- sim_aggregate(bundle$per_rep, bundle$design)

table2 <- data.frame(
  stratum = c("Mainstream", "Small-school stress", "PV sensitivity"),
  conditions = c(12L, 2L, 10L), replications_per_condition = c(100L, 400L, 200L),
  fits_per_estimator = c(1200L, 800L, 2000L),
  varied_factors = c(
    "ICC profiles; schools J; mean school size; covariate ICC",
    "Both ICC profiles at J=50 and mean school size=20",
    "PV reliability; PV count; Korea-profile small-school variants"
  ),
  stringsAsFactors = FALSE
)

workflow_label <- c(
  bayes_a = "Per-PV (MCMC)", freq_per_pv = "Per-PV (REML)",
  c_direct = "Calibrated stack"
)
table3 <- fresh$by_stratum[fresh$by_stratum$leg %in% names(workflow_label), ]
table3$estimator <- unname(workflow_label[table3$leg])
table3 <- table3[, c("stratum", "leg", "estimator", "fits", "biasW",
                     "mcseW", "covW", "biasB", "mcseB", "covB")]

condition <- fresh$by_condition[
  fresh$by_condition$leg %in% names(workflow_label), , drop = FALSE
]
condition$estimator <- unname(workflow_label[condition$leg])
figure2 <- rbind(
  data.frame(condition[, c("design_condition_id", "rep_tier", "profile",
                           "ICC_y", "J", "nbar", "ICC_x", "rho_PV", "M",
                           "leg", "estimator", "R")],
             slope = "beta_W", truth = SIM_TRUTH_W,
             bias = condition$biasW, coverage = condition$covW,
             coverage_low = condition$covW_lo,
             coverage_high = condition$covW_hi),
  data.frame(condition[, c("design_condition_id", "rep_tier", "profile",
                           "ICC_y", "J", "nbar", "ICC_x", "rho_PV", "M",
                           "leg", "estimator", "R")],
             slope = "beta_B", truth = SIM_TRUTH_B,
             bias = condition$biasB, coverage = condition$covB,
             coverage_low = condition$covB_lo,
             coverage_high = condition$covB_hi)
)
osm <- condition[, c("design_condition_id", "rep_tier", "profile", "ICC_y",
                     "J", "nbar", "ICC_x", "rho_PV", "M", "leg",
                     "estimator", "R", "biasW", "covW", "covW_lo",
                     "covW_hi", "biasB", "covB", "covB_lo", "covB_hi")]

outputs <- list(
  table2_design_input.csv = table2,
  table3_headline_input.csv = table3,
  figure2_plot_data.csv = figure2,
  osm_e_condition_results.csv = osm
)
for (name in names(outputs)) {
  path <- file.path(output, name)
  staged <- paste0(path, ".tmp")
  utils::write.csv(outputs[[name]], staged, row.names = FALSE, na = "")
  if (!file.rename(staged, path)) stop("Could not publish ", name,
                                      call. = FALSE)
}
stopifnot(nrow(table2) == 3L, nrow(table3) == 9L,
          nrow(figure2) == 144L, nrow(osm) == 72L)
message("Built simulation exhibit inputs in ", output)
