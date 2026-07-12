# =============================================================================
# verify_simulation.R: Archive verification and paper numerical anchors
# =============================================================================
#
# Purpose : Standalone check that the compact simulation archive still supports
#           the paper's claims. Asserts the seed contract; verifies SHA-256 of
#           the active DGM, sampling, seed, and source-design files; confirms
#           the curated design equals the frozen source; re-aggregates the
#           evidence bundle and compares it to the archived condition and
#           stratum CSVs; checks the diagnostic, parity, reproduction, and
#           headline anchors; and prints a PASS report.
#
# Contents:
#   .verify_find_root()   find the replication-package root
#   Seed and hash gates   assert seed contract; hash active source files
#   Design cross-check    curated design equals the frozen source design
#   compare_numeric()     key-aligned numeric comparison within tolerance
#   Bundle re-aggregation validate evidence and recompute aggregates
#   Archive comparison    condition and stratum aggregates vs archived CSVs
#   Diagnostic checks     48 anchors, 558 implied fits, zero divergences
#   Anchor checks         parity, June reproduction, and headline bounds
#   PASS report           print the verification summary lines
#
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.verify_find_root <- function(start = getwd()) {
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
root <- .verify_find_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(root, "code", "02_simulation", "aggregate.R"), local = FALSE)
source(file.path(root, "code", "02_simulation", "seed_protocol.R"),
       local = FALSE)

assert_simulation_seed_contract()
active_source_contract <- c(
  "code/02_simulation/dgm/dgm_gaussian.R" =
    "71267fad398d700e002f015dd2d54e20762f2136011c2930fc55115166869a98",
  "code/02_simulation/dgm/dgm_sampling_weights.R" =
    "15cd8c746d9b1c2a419f0991ec3f56cc2220d0d34498f8f70aead1792346cd02",
  "code/02_simulation/seeds.R" =
    "6db223512233767d8a744ba07424fe733b53319826365f8ff54d624139ca9ae4",
  "code/02_simulation/design/source_design.csv" =
    "08085b1e05e42fce81a8163a417e15464156210d2e0dee0a41deaa0d3f5e4752"
)
source_hashes <- vapply(names(active_source_contract), function(path) {
  sim_file_sha256(file.path(root, path))
}, character(1L))
if (!identical(unname(source_hashes), unname(active_source_contract))) {
  stop("Active DGM/design/seed source hash mismatch.", call. = FALSE)
}
source_design <- utils::read.csv(
  file.path(root, "code", "02_simulation", "design", "source_design.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
curated_design <- sim_read_design()
expected_design <- unique(source_design[
  source_design$route == "gaussian" & !is.na(source_design$n_rep) &
    source_design$n_rep > 0, , drop = FALSE
])
expected_design <- expected_design[
  !duplicated(expected_design$design_condition_id), , drop = FALSE
]
rownames(expected_design) <- NULL
rownames(curated_design) <- NULL
common <- c("design_condition_id", "rep_tier", "n_rep", "profile", "ICC_y",
            "J", "nbar", "ICC_x", "rho_PV", "M", "route", "weight_info",
            "sampling_design", "nonresponse_model", "trim_rule", "brr_R",
            "fay_k", "weight_scaling_rule")
if (!identical(expected_design[, common, drop = FALSE],
               curated_design[, common, drop = FALSE])) {
  stop("Curated paper design differs from the frozen source design.",
       call. = FALSE)
}
data_dir <- file.path(root, "data", "precomputed", "simulation")
bundle <- readRDS(file.path(data_dir, "simulation_evidence.rds"))
if (!identical(bundle$schema_version, "pvstackr_simulation_evidence_v1")) {
  stop("Unexpected simulation evidence schema.", call. = FALSE)
}
sim_validate_complete(bundle$per_rep, bundle$design)
fresh <- sim_aggregate(bundle$per_rep, bundle$design)

compare_numeric <- function(actual, expected, keys, tolerance = 1e-12) {
  a <- actual[do.call(order, actual[keys]), , drop = FALSE]
  e <- expected[do.call(order, expected[keys]), , drop = FALSE]
  rownames(a) <- NULL
  rownames(e) <- NULL
  if (!identical(a[keys], e[keys])) stop("Aggregate keys differ.", call. = FALSE)
  numeric <- intersect(names(a)[vapply(a, is.numeric, logical(1L))], names(e))
  differences <- vapply(numeric, function(name) {
    max(abs(a[[name]] - e[[name]]), na.rm = TRUE)
  }, numeric(1L))
  differences[!is.finite(differences)] <- 0
  if (any(differences > tolerance)) {
    stop("Aggregate mismatch above tolerance: ",
         paste(names(differences)[differences > tolerance], collapse = ", "),
         call. = FALSE)
  }
  invisible(differences)
}

archived_condition <- utils::read.csv(
  file.path(data_dir, "parity_by_condition.csv"), stringsAsFactors = FALSE
)
archived_stratum <- utils::read.csv(
  file.path(data_dir, "parity_by_stratum.csv"), stringsAsFactors = FALSE
)
compare_numeric(fresh$by_condition, archived_condition,
                c("design_condition_id", "leg"))
compare_numeric(fresh$by_stratum, archived_stratum, c("stratum", "leg"))

diagnostics <- bundle$diagnostics
if (nrow(diagnostics) != 48L ||
    anyDuplicated(paste(diagnostics$design_condition_id, diagnostics$rep_id)) ||
    !identical(sort(unique(diagnostics$rep_id)), 1:2) ||
    sum(diagnostics$bayes_a_div) + sum(diagnostics$cdirect_div) != 0L ||
    any(diagnostics[, c("ident_dA_W", "ident_dA_B", "ident_dC_W",
                        "ident_dC_B")] != 0)) {
  stop("Diagnostic completeness or identity checks failed.", call. = FALSE)
}
implied_fits <- 2L * sum(bundle$design$M + 1L)
if (implied_fits != 558L) stop("Expected 558 diagnostic MCMC fits.",
                               call. = FALSE)

paired <- merge(
  bundle$per_rep[bundle$per_rep$leg == "bayes_a",
                 c("design_condition_id", "rep_id", "bW", "bB")],
  bundle$per_rep[bundle$per_rep$leg == "freq_per_pv",
                 c("design_condition_id", "rep_id", "bW", "bB")],
  by = c("design_condition_id", "rep_id"), suffixes = c("_mcmc", "_freq")
)
max_w <- max(abs(paired$bW_mcmc - paired$bW_freq))
max_b <- max(abs(paired$bB_mcmc - paired$bB_freq))
if (abs(max_w - 0.0006818915) > 1e-10 ||
    abs(max_b - 0.01063868) > 1e-8) {
  stop("MCMC--archived-frequency parity anchors changed.", call. = FALSE)
}

max_june <- max(abs(bundle$june_within_slope_reproduction$dMeanW))
if (max_june > 0.0087 || max_june < 0.0085) {
  stop("June reproduction anchor is outside the recorded 0.0086 range.",
       call. = FALSE)
}

headline <- fresh$by_stratum[fresh$by_stratum$leg != "oracle", ]
if (max(abs(headline$biasW)) > 0.0014 ||
    max(abs(headline$biasB)) > 0.0052 ||
    min(c(headline$covW, headline$covB)) < 0.945 ||
    max(c(headline$covW, headline$covB)) > 0.98625) {
  stop("Paper headline anchors are outside their frozen bounds.",
       call. = FALSE)
}

cat("PASS: 24 conditions; 4,000 replications; 16,000 estimator rows\n")
cat("PASS: active DGM, sampling, curated seed, and source-design hashes\n")
cat(sprintf("PASS: diagnostics 48 anchors / 558 MCMC fits; max R-hat %.9f; divergences 0\n",
            max(diagnostics$bayes_a_rhat_max,
                diagnostics$cdirect_rhat_max)))
cat(sprintf("PASS: max |MCMC - archived frequency comparator| beta-W %.10f; beta-B %.10f\n",
            max_w, max_b))
cat(sprintf("PASS: June beta-W reproduction max condition-mean difference %.10f\n",
            max_june))
