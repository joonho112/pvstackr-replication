# =============================================================================
# build_precomputed_bundle.R: Rebuild public evidence bundles from CSVs
# =============================================================================
#
# Purpose : Rebuild the compact public evidence bundles from the inspectable
#           canonical CSVs. Verifies each source CSV against its recorded
#           SHA-256, re-aggregates the per-replication rows, assembles the
#           public simulation_evidence.rds and diagnostic_bundle.rds (xz), and
#           writes artifact_manifest.csv listing bytes, hashes, and roles for
#           every published file under data/precomputed/simulation.
#
# Contents:
#   .bundle_find_root()  find the replication-package root
#   Hash gate            verify canonical CSV SHA-256 against expectations
#   Re-aggregation       recompute aggregates from the per-rep CSV
#   Evidence bundle      assemble simulation_evidence.rds (+ anchors)
#   Diagnostic bundle    assemble diagnostic_bundle.rds anchors
#   Serialization        write xz-compressed RDS and artifact_manifest.csv
#
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.bundle_find_root <- function(start = getwd()) {
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
root <- .bundle_find_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(root, "code", "02_simulation", "aggregate.R"), local = FALSE)

data_dir <- file.path(root, "data", "precomputed", "simulation")
expected_hashes <- c(
  parity_per_rep.csv = "24d5dfe2a11c5a3bb0d5d96ea39ab0873216bb6091a82e2928c653d0b17590ab",
  parity_by_condition.csv = "151b11c5e93203c1685242bc8de9bed52f7f5b193d694dc4c7415635164b3081",
  parity_by_stratum.csv = "3d65821de98d5849de293709e859939dd2554a943b21ae9cbeb81d9a03dce6e7",
  diagnostics.csv = "47399cc45a993b5dbd42c227a4ab3f39c94806079dc65591a570691aafb3e328",
  betaW_reproduction_check.csv = "1c5dc8db05f48cf67e4485178f974f9e96a4853603652c00d73320da2ac530a3",
  design.csv = "08085b1e05e42fce81a8163a417e15464156210d2e0dee0a41deaa0d3f5e4752"
)

observed_hashes <- vapply(names(expected_hashes), function(name) {
  sim_file_sha256(file.path(data_dir, name))
}, character(1L))
if (!identical(unname(observed_hashes), unname(expected_hashes))) {
  bad <- names(expected_hashes)[observed_hashes != expected_hashes]
  stop("Canonical CSV hash mismatch: ", paste(bad, collapse = ", "),
       call. = FALSE)
}

per_rep <- utils::read.csv(file.path(data_dir, "parity_per_rep.csv"),
                           stringsAsFactors = FALSE)
aggregated <- sim_aggregate(per_rep)
diagnostics <- utils::read.csv(file.path(data_dir, "diagnostics.csv"),
                               stringsAsFactors = FALSE)
reproduction <- utils::read.csv(
  file.path(data_dir, "betaW_reproduction_check.csv"),
  stringsAsFactors = FALSE
)
design <- sim_read_design()

evidence <- list(
  schema_version = "pvstackr_simulation_evidence_v1",
  paper_anchor = list(
    conditions = 24L, replications = 4000L, estimator_rows = 16000L,
    truth = c(beta_W = SIM_TRUTH_W, beta_B = SIM_TRUTH_B),
    dgm_seed_root = SIM_DGM_SEED_ROOT,
    mcmc_seed_root = SIM_MCMC_SEED_ROOT
  ),
  design = design,
  per_rep = aggregated$per_rep,
  by_condition = aggregated$by_condition,
  by_stratum = aggregated$by_stratum,
  diagnostics = diagnostics,
  june_within_slope_reproduction = reproduction,
  canonical_csv_sha256 = observed_hashes,
  provenance = "July 8-11, 2026 both-gradient parity run; see SOURCE_MANIFEST.md"
)

diagnostic_bundle <- list(
  schema_version = "pvstackr_simulation_diagnostics_v1",
  expected_keys = expand.grid(
    design_condition_id = sort(design$design_condition_id),
    rep_id = 1:2, stringsAsFactors = FALSE
  ),
  summary = diagnostics,
  implied_mcmc_fits = 2L * sum(design$M + 1L),
  anchors = list(
    maximum_rhat_exact = max(diagnostics$bayes_a_rhat_max,
                             diagnostics$cdirect_rhat_max),
    minimum_bayes_a_ess = min(diagnostics$bayes_a_ess_min),
    divergences = sum(diagnostics$bayes_a_div) +
      sum(diagnostics$cdirect_div)
  )
)

saveRDS(evidence, file.path(data_dir, "simulation_evidence.rds"),
        compress = "xz", version = 3)
saveRDS(diagnostic_bundle, file.path(data_dir, "diagnostic_bundle.rds"),
        compress = "xz", version = 3)

manifest_files <- c(
  names(expected_hashes), "simulation_evidence.rds", "diagnostic_bundle.rds",
  "SOURCE_MANIFEST.md", "source_hashes.csv"
)
manifest <- data.frame(
  file = manifest_files,
  bytes = file.info(file.path(data_dir, manifest_files))$size,
  sha256 = vapply(manifest_files, function(name) {
    sim_file_sha256(file.path(data_dir, name))
  }, character(1L)),
  role = c(
    "inspectable per-replication evidence", "archived condition aggregate",
    "archived stratum aggregate", "diagnostic anchors",
    "June within-slope reproduction anchor", "frozen source design",
    "combined public evidence",
    "combined diagnostic anchors", "source provenance", "source checksums"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(data_dir, "artifact_manifest.csv"),
                 row.names = FALSE)
message("Built public simulation evidence bundles in ", data_dir)
