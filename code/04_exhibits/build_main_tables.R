# =============================================================================
# build_main_tables.R: emit the five main-text tables from shipped evidence
# =============================================================================
#
# Purpose : Build the CSV tables for the main text from shipped precomputed
#           evidence. Reads the simulation design and parity-by-stratum
#           summaries and the PISA A/B/C estimates, then writes one CSV per
#           table into output/tables.
#           Inputs : data/precomputed/simulation/{design,parity_by_stratum};
#           data/precomputed/pisa/ A/B/C estimate-and-SE CSVs.
#           Outputs: output/tables/table1..table5 CSV files.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   .read_first()  Read the first existing CSV among candidate paths.
#   build_main_tables()  Emit the five main tables:
#     table1_workflows.csv  The three PV workflows compared.
#     table2_simulation_design.csv  Simulation strata, counts, and factors.
#     table3_simulation_headline.csv  Headline bias/MCSE/coverage by stratum.
#     table4_pisa_main.csv  PISA pipelines A and C estimates.
#     table5_pisa_reweighting.csv  USA reweighted stack (B) vs per-PV (A).
# =============================================================================
.read_first <- function(paths) {
  hit <- paths[file.exists(paths)][1L]
  if (is.na(hit)) stop("Missing exhibit input: ", paste(paths, collapse = " or "), call. = FALSE)
  utils::read.csv(hit, check.names = FALSE, stringsAsFactors = FALSE)
}

build_main_tables <- function() {
  out <- project_path("output", "tables")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  table1 <- data.frame(
    attribute = c("Role in this article", "MCMC fits per model", "After the fit",
                  "External input", "Convergence audits", "Reportable when"),
    per_PV_workflow = c(
      "Orthodox reference", "M", "Combining rules across M posteriors", "None", "M",
      "Sampler diagnostics pass"
    ),
    calibrated_stack = c(
      "Proposal", "1", "One affine calibration to (beta_bar_MI, T_MI)",
      "Target assembled from deterministic per-PV refits", "1",
      "Sampler diagnostics pass; target covariances positive definite"
    ),
    reweighted_stack = c(
      "Reliability-gated variant", "1 (reuses the stacked fit)",
      "M importance reweightings, then combining rules", "None", "1",
      "Sampler diagnostics pass and Pareto k-hat < 0.7 for all M PVs"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(table1, file.path(out, "table1_workflows.csv"), row.names = FALSE)

  design <- .read_first(c(project_path("data", "precomputed", "simulation", "design.csv"),
                          project_path("data", "precomputed", "simulation", "simulation_design.csv")))
  if ("route" %in% names(design)) design <- design[design$route == "gaussian", , drop = FALSE]
  table2 <- data.frame(
    stratum = c("Mainstream", "Small-school stress", "PV sensitivity"),
    conditions = c(12L, 2L, 10L),
    replications_per_condition = c(100L, 400L, 200L),
    varied_factors = c(
      paste0("Schools J in {50, 100, 200} and mean school size n-bar in {10, 20, 30}, ",
             "over eight cells at the 0.20 profile; the 0.45 profile at J in {100, 200}, ",
             "n-bar = 20; one covariate-ICC variation at each profile's anchor. ",
             "Profiles (outcome/covariate ICC): 0.20/0.15 (United States reading) and 0.45/0.30 (Korea)."),
      "Both profiles at J = 50, n-bar = 20, at high replication.",
      paste0("PV reliability rho_PV in {0.85, 0.90, 0.95} and PV count M in {5, 10, 20} ",
             "around the 0.20 anchor (eight conditions); the 0.45 anchor and an n-bar = 10 variant of it.")
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(table2, file.path(out, "table2_simulation_design.csv"), row.names = FALSE)

  s <- .read_first(c(project_path("data", "precomputed", "simulation", "parity_by_stratum.csv")))
  keep <- s$leg %in% c("bayes_a", "freq_per_pv", "c_direct")
  s <- s[keep, c("stratum", "leg", "fits", "biasW", "mcseW", "covW", "biasB", "mcseB", "covB")]
  labels <- c(bayes_a = "Per-PV MCMC", freq_per_pv = "Per-PV REML", c_direct = "Calibrated stack")
  s$estimator <- unname(labels[s$leg])
  s <- s[, c("stratum", "estimator", "fits", "biasW", "mcseW", "covW", "biasB", "mcseB", "covB")]
  utils::write.csv(s, file.path(out, "table3_simulation_headline.csv"), row.names = FALSE)

  p <- .read_first(c(project_path("data", "precomputed", "pisa", "pisa_final_abc.csv"),
                     project_path("data", "precomputed", "pisa", "pisa_phase7r_final_abc_estimate_se.csv"),
                     project_path("data", "precomputed", "pisa", "abc_estimates.csv")))
  se_col <- if ("std_error" %in% names(p)) "std_error" else "se"
  p$se_value <- p[[se_col]]
  p$effect <- ifelse(grepl("within", p$term), "Within-school", "Between-school")
  table4 <- p[p$pipeline %in% c("A", "C"), c("country", "pipeline", "effect", "estimate", "se_value", "ci_low", "ci_high", "df")]
  names(table4)[names(table4) == "se_value"] <- "std_error"
  utils::write.csv(table4, file.path(out, "table4_pisa_main.csv"), row.names = FALSE)

  pa <- p[p$country == "USA" & p$pipeline == "A", c("term", "estimate", "se_value")]
  pb <- p[p$country == "USA" & p$pipeline == "B", c("term", "estimate", "se_value")]
  m <- merge(pa, pb, by = "term", suffixes = c("_A", "_B"), sort = FALSE)
  table5 <- data.frame(country = "USA", effect = ifelse(grepl("within", m$term), "Within-school", "Between-school"),
                       estimate = m$estimate_B, std_error = m$se_value_B,
                       difference_from_A = m$estimate_B - m$estimate_A,
                       se_ratio_to_A = m$se_value_B / m$se_value_A)
  utils::write.csv(table5, file.path(out, "table5_pisa_reweighting.csv"), row.names = FALSE)
  invisible(TRUE)
}
