# =============================================================================
# 08_build_public_evidence.R: curate private archives into public evidence
# =============================================================================
#
# Purpose : Stage 08. Reads the private working-codebase archives (located via
#           PVSTACKR_SOURCE_CODEBASE) and distills them into the path-free
#           public objects the cached track consumes: the Pipeline A/C caches,
#           the Pipeline B PSIS gate, combined A/B/C estimates, diagnostics,
#           reportability status, and BRR-Fay/reversal tables. Enforces Korea's
#           withheld status and scans every value for unsafe paths first.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   keep_columns(), bind_fill(), read_archive() - column/rbind/load helpers
#   object a - public Pipeline A per-PV reference cache
#   object c - Pipeline C raw + calibrated draws, matrices, safe target fields
#   object b - Pipeline B per-PV log ratios, khat, gate, paper rows (KOR held)
#   safety scan + saveRDS - reject unsafe values; write three public caches
#   CSV exports - abc_estimates, psis_diagnostics, reportability_status
#   copy_tables loop - sanitize/copy BRR-Fay, reversal, robustness tables
#   field_map - per-object retained/removed provenance summary
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

source_root <- Sys.getenv("PVSTACKR_SOURCE_CODEBASE", unset = "")
if (!nzchar(source_root) || !dir.exists(source_root)) {
  stop("Set PVSTACKR_SOURCE_CODEBASE to the source codebase directory.", call. = FALSE)
}
cache <- file.path(source_root, "data-cache")
tables <- file.path(source_root, "tables")

keep_columns <- function(x, columns) x[, intersect(columns, names(x)), drop = FALSE]
bind_fill <- function(...) {
  xs <- list(...)
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(columns, names(x))
    for (column in missing) x[[column]] <- NA
    x[, columns, drop = FALSE]
  })
  do.call(rbind, xs)
}
read_archive <- function(name) readRDS(file.path(cache, name))

a0 <- read_archive("pisa_pipeline_a_draws_est01.rds")
c0 <- read_archive("pisa_cdirect_draws_est01.rds")
b0 <- read_archive("pisa_pipeline_b_estimates_est01.rds")

result_columns <- c("support_id", "country", "pipeline", "term", "estimate",
                    "std_error", "ci_low", "ci_high", "df", "analysis_role",
                    "reporting_status", "parameter_family", "pv_count", "n_rows",
                    "n_schools", "n_draws", "n_draws_per_pv_min",
                    "n_draws_per_pv_max", "rhat_max", "ess_bulk_min",
                    "ess_tail_min", "divergences", "divergences_total",
                    "max_treedepth_hits", "max_treedepth_hits_total",
                    "row_support_hash", "target_hash", "Sigma_target_hash")
fit_columns <- c("country", "support_id", "pv_index", "pv_col", "n_rows",
                 "n_schools", "row_support_hash", "model_data_hash", "seed",
                 "n_draws", "n_parameters", "draw_hash", "fit_rds_sha256",
                 "runtime_sec", "rhat_max", "ess_bulk_min", "ess_tail_min",
                 "divergences", "max_treedepth_hits", "mcmc_pass",
                 "production_fit", "injected_smoke", "fake_or_synthetic_draws",
                 "fit_engine")

a <- list(
  schema_version = "pisa_pipeline_a_public_cache_v1",
  source_id = "archived-production-pipeline-a",
  support_id = "EST-01",
  runtime_config = a0$runtime_config,
  task_schedule = keep_columns(a0$fit_meta, fit_columns),
  draws = a0$draws,
  targets = a0$targets,
  pooled = a0$pooled,
  rows = keep_columns(a0$rows, result_columns)
)

safe_target <- function(x) x[intersect(names(x), c(
  "country", "support_id", "terms", "beta", "U_bar", "B", "T_MI", "df", "M",
  "row_support_hash", "n_rows", "n_schools", "target_hash", "fe_names",
  "Sigma_target_hash", "sigma_check"
))]
c_country <- lapply(PISA_COUNTRIES, function(country) {
  x <- c0$country_results[[country]]
  list(
    country = country,
    seed = x$seed,
    runtime_sec = x$runtime_sec,
    raw_stacked_draws = x$stack$stacked_draws,
    calibrated_draws = x$ccc_result$draws_calibrated,
    calibration_matrix = x$ccc_result$A,
    Sigma_raw = x$ccc_result$Sigma_raw,
    Sigma_target = x$ccc_result$Sigma_target,
    Sigma_calibrated_empirical = x$ccc_result$Sigma_cal_emp,
    ccc_status = x$ccc_result$ccc_status,
    target = safe_target(x$target),
    rows = keep_columns(x$rows, result_columns)
  )
})
names(c_country) <- PISA_COUNTRIES
c_public <- list(
  schema_version = "pisa_pipeline_c_public_cache_v1",
  source_id = "archived-production-pipeline-c",
  support_id = "EST-01",
  runtime_config = c0$runtime_config,
  countries = c_country,
  rows = keep_columns(c0$rows, result_columns)
)

khat_columns <- c("country", "support_id", "pv_index", "pv_col", "n_rows",
                  "n_schools", "n_draws", "pareto_khat", "khat_threshold",
                  "psis_status", "psis_ok", "psis_n_eff", "log_r_emitted",
                  "psis_called", "log_r_hash", "psis_weight_hash", "q_draws_hash",
                  "proposal_rng_seed", "proposal_sampler_id", "q_fit_id",
                  "draw_source_id", "row_support_hash", "pipeline_b_numerator_source",
                  "pipeline_b_uses_replicate_weights",
                  "pipeline_b_final_replicates_allowed",
                  "pipeline_b_replicate_firewall_pass", "numerator_weight_columns",
                  "forbidden_final_weight_used_as_numerator",
                  "forbidden_replicate_weight_used_as_numerator", "firewall_status")
b_rows <- keep_columns(b0$rows, c(result_columns, "parameter_reporting_status",
                                  "khat_max", "psis_ok_count", "psis_fail_count"))
b_rows$reporting_status[b_rows$country == "KOR"] <- "computed_but_withheld"
khat <- keep_columns(b0$khat_rows, khat_columns)
gate <- do.call(rbind, lapply(PISA_COUNTRIES, function(country) {
  z <- khat[khat$country == country, , drop = FALSE]
  passed <- nrow(z) == 10L && all(z$psis_ok) && all(z$pareto_khat < PISA_KHAT_THRESHOLD)
  data.frame(
    country = country,
    computation_completed = nrow(z) == 10L && all(z$log_r_emitted) && all(z$psis_called),
    n_pv = nrow(z),
    n_pv_pass = sum(z$psis_ok),
    max_pareto_k = max(z$pareto_khat),
    threshold = PISA_KHAT_THRESHOLD,
    numeric_reporting_allowed = passed,
    reporting_status = if (passed) "reportable" else "computed_but_withheld",
    stringsAsFactors = FALSE
  )
}))
b <- list(
  schema_version = "pisa_pipeline_b_public_diagnostics_v1",
  source_id = "archived-production-pipeline-b",
  support_id = "EST-01",
  policy = list(threshold = PISA_KHAT_THRESHOLD, require_all_pv = TRUE,
                blocked_status = "computed_but_withheld"),
  log_ratios = stats::setNames(lapply(PISA_COUNTRIES, function(country) {
    lapply(b0$country_results[[country]]$psis_by_pv, function(z) as.numeric(z$log_r))
  }), PISA_COUNTRIES),
  khat = khat,
  gate = gate,
  paper_rows = b_rows
)

public_objects <- list(a = a, c = c_public, b = b)
for (object_name in names(public_objects)) {
  bad <- pisa_scan_character_values(public_objects[[object_name]], object_name)
  pisa_assert(!length(bad), paste("Unsafe value in curated object:", paste(bad, collapse = "\n")))
}
saveRDS(a, file.path(PISA_PRECOMPUTED_DIR, "pipeline_a_cached.rds"),
        compress = "xz", version = 3)
saveRDS(c_public, file.path(PISA_PRECOMPUTED_DIR, "pipeline_c_cached.rds"),
        compress = "xz", version = 3)
saveRDS(b, file.path(PISA_PRECOMPUTED_DIR, "pipeline_b_psis_gate.rds"),
        compress = "xz", version = 3)

abc <- bind_fill(keep_columns(a0$rows, result_columns),
                 keep_columns(b0$rows, result_columns),
                 keep_columns(c0$rows, result_columns))
abc$reporting_status[abc$pipeline == "B" & abc$country == "KOR"] <-
  "computed_but_withheld"
abc <- abc[order(match(abc$country, PISA_COUNTRIES), match(abc$pipeline, c("A", "B", "C")),
                 match(abc$term, c("b_ESCS_within", "b_ESCS_between"))), , drop = FALSE]
utils::write.csv(abc, file.path(PISA_PRECOMPUTED_DIR, "abc_estimates.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(khat, file.path(PISA_PRECOMPUTED_DIR, "psis_diagnostics.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(gate, file.path(PISA_PRECOMPUTED_DIR, "reportability_status.csv"),
                 row.names = FALSE, na = "")

copy_tables <- c(
  pisa_brrfay_target_est01 = "brr_fay_target.csv",
  pisa_brrfay_target_est01_per_pv = "brr_fay_per_pv.csv",
  pisa_reversal_results = "reversal_results.csv",
  pisa_reversal_country_estimates = "reversal_country_estimates.csv",
  pisa_reversal_robustness_grid = "reversal_robustness_grid.csv",
  pisa_reversal_robustness_country_estimates = "reversal_robustness_country_estimates.csv",
  pisa_reversal_robustness_summary = "reversal_robustness_summary.csv",
  pisa_reversal_secondary_summary = "reversal_secondary_summary.csv",
  pisa_abc_difference_diagnostics = "abc_difference_diagnostics.csv"
)
for (stem in names(copy_tables)) {
  tab <- utils::read.csv(file.path(tables, paste0(stem, ".csv")),
                         stringsAsFactors = FALSE, check.names = FALSE)
  tab$generated_at <- NULL
  bad <- pisa_scan_character_values(tab, stem)
  pisa_assert(!length(bad), paste("Unsafe value in", stem))
  utils::write.csv(tab, file.path(PISA_PRECOMPUTED_DIR, copy_tables[[stem]]),
                   row.names = FALSE, na = "")
}

field_map <- data.frame(
  public_object = c("pipeline_a_cached.rds", "pipeline_c_cached.rds",
                    "pipeline_b_psis_gate.rds"),
  retained = c("draws; targets; pooled results; task seeds; sampler diagnostics",
               "raw and calibrated draws; calibration matrices; target; diagnostics",
               "per-PV log ratios and PSIS diagnostics; gate state; paper-facing rows"),
  removed = c("timestamps; absolute fit paths; embedded provenance JSON",
              "timestamps; fit handles; long likelihood arrays; provenance JSON",
              "timestamps; analytic data duplicate; raw unstable country estimates"),
  stringsAsFactors = FALSE
)
utils::write.csv(field_map, file.path(PISA_PRECOMPUTED_DIR, "curation_field_map.csv"),
                 row.names = FALSE)
cat("Curated PISA public evidence: PASS\n")
