# =============================================================================
# simulation_utils.R: Shared constants, validation, and provenance contracts
# =============================================================================
#
# Purpose : Shared building blocks sourced by every simulation script. Defines
#           the truth slopes, estimator legs, and seed roots; discovers the
#           replication-package root; computes SHA-256 file digests, run
#           signatures, and content hashes for signed provenance; reads and
#           checks the 24-condition paper design; enumerates the expected tasks
#           and estimator keys; validates evidence rows; and provides the
#           Wilson interval and the --name=value argument parser.
#
# Contents:
#   SIM_TRUTH_W/B, SIM_LEGS, seed roots  frozen study constants
#   %||%                          null-coalescing helper
#   sim_find_repo_root()          locate the replication-package root
#   sim_path()                    build a path under the repo root
#   sim_file_sha256()             SHA-256 digest of a file (via digest)
#   sim_signature_source_files()  files that define a run signature
#   sim_run_signature()           hash seeds, source, and toolchain versions
#   sim_rows_content_hash()       content hash of the estimator rows
#   sim_attach_provenance()       attach run-signature and content-hash columns
#   sim_validate_provenance()     verify signed provenance columns
#   sim_read_design()             read and validate the 24-condition design
#   sim_expected_tasks()          enumerate the condition/replication tasks
#   sim_expected_keys()           expand tasks to per-estimator keys
#   sim_key()                     compose a condition/rep/leg key string
#   sim_validate_rows_basic()     schema, finiteness, and interval checks
#   sim_validate_complete()       completeness plus oracle-identity checks
#   sim_sort_rows()               canonical ordering of evidence rows
#   sim_wilson()                  Wilson score interval for coverage
#   sim_parse_named_args()        parse --name=value command-line arguments
#
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

SIM_DGM_SEED_ROOT <- 20260514L
SIM_MCMC_SEED_ROOT <- 20260601L
SIM_TRUTH_W <- 0.40
SIM_TRUTH_B <- 0.60
SIM_LEGS <- c("oracle", "freq_per_pv", "bayes_a", "c_direct")

`%||%` <- function(x, y) if (is.null(x)) y else x

sim_find_repo_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    marker <- file.path(path, "code", "02_simulation")
    if (dir.exists(marker)) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  stop("Could not locate the replication-package root from: ", start,
       call. = FALSE)
}

sim_path <- function(...) file.path(sim_find_repo_root(), ...)

sim_file_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required for SHA-256 checks.", call. = FALSE)
  }
  digest::digest(file = path, algo = "sha256")
}

sim_signature_source_files <- function() {
  config_rel <- Sys.getenv("PVSTACKR_CONFIG", unset = "config/defaults.yml")
  c(
    "code/02_simulation/design/paper_design.csv",
    "code/02_simulation/seed_protocol.R",
    "code/02_simulation/seeds.R",
    "code/02_simulation/dgm/build_substrate.R",
    "code/02_simulation/dgm/dgm_gaussian.R",
    "code/02_simulation/dgm/dgm_sampling_weights.R",
    "code/02_simulation/simulation_utils.R",
    "code/02_simulation/runner/run_one.R",
    "code/01_core/rubin_pool.R",
    "renv.lock",
    config_rel
  )
}

sim_run_signature <- function(mode = c("full", "quick")) {
  mode <- match.arg(mode)
  root <- sim_find_repo_root()
  files <- sim_signature_source_files()
  paths <- file.path(root, files)
  if (any(!file.exists(paths))) {
    stop("Cannot compute simulation run signature; missing: ",
         paste(files[!file.exists(paths)], collapse = ", "), call. = FALSE)
  }
  runtime_packages <- c("R" = as.character(getRversion()),
                        stats::setNames(vapply(
                          c("lme4", "brms", "posterior", "cmdstanr"),
                          function(pkg) if (requireNamespace(pkg, quietly = TRUE)) {
                            as.character(utils::packageVersion(pkg))
                          } else "missing",
                          character(1L)
                        ), c("lme4", "brms", "posterior", "cmdstanr")))
  cmdstan_version <- if (requireNamespace("cmdstanr", quietly = TRUE)) {
    tryCatch(as.character(cmdstanr::cmdstan_version()),
             error = function(e) "missing")
  } else "missing"
  payload <- list(
    schema = "pvstackr-simulation-run-signature-v1",
    mode = mode,
    dgm_seed_root = SIM_DGM_SEED_ROOT,
    mcmc_seed_root = SIM_MCMC_SEED_ROOT,
    truth = c(beta_W = SIM_TRUTH_W, beta_B = SIM_TRUTH_B),
    legs = SIM_LEGS,
    runtime_packages = runtime_packages,
    cmdstan_version = cmdstan_version,
    source_hashes = stats::setNames(vapply(paths, sim_file_sha256, character(1L)),
                                    files)
  )
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

sim_rows_content_hash <- function(rows) {
  columns <- c("design_condition_id", "rep_id", "leg", "bW", "seW",
               "loW", "hiW", "bB", "seB", "loB", "hiB",
               "dgm_seed_root", "mcmc_seed_root", "replay_mode")
  if (any(!columns %in% names(rows))) {
    stop("Cannot hash shard content; provenance columns are missing.",
         call. = FALSE)
  }
  value <- rows[order(rows$design_condition_id, rows$rep_id,
                      match(rows$leg, SIM_LEGS)), columns, drop = FALSE]
  rownames(value) <- NULL
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

sim_attach_provenance <- function(rows, mode = c("full", "quick")) {
  mode <- match.arg(mode)
  rows$run_signature <- sim_run_signature(mode)
  rows$content_hash <- sim_rows_content_hash(rows)
  rows
}

sim_validate_provenance <- function(rows, expected_mode = c("full", "quick")) {
  expected_mode <- match.arg(expected_mode)
  required <- c("replay_mode", "dgm_seed_root", "mcmc_seed_root",
                "run_signature", "content_hash")
  if (any(!required %in% names(rows))) {
    stop("Simulation shard lacks signed provenance columns.", call. = FALSE)
  }
  if (!all(rows$replay_mode == expected_mode) ||
      !all(rows$dgm_seed_root == SIM_DGM_SEED_ROOT) ||
      !all(rows$mcmc_seed_root == SIM_MCMC_SEED_ROOT) ||
      !all(rows$run_signature == sim_run_signature(expected_mode)) ||
      length(unique(rows$content_hash)) != 1L ||
      !identical(unique(rows$content_hash), sim_rows_content_hash(rows))) {
    stop("Simulation shard run signature or content hash does not match.",
         call. = FALSE)
  }
  invisible(TRUE)
}

sim_read_design <- function(path = sim_path(
                              "code", "02_simulation", "design",
                              "paper_design.csv")) {
  design <- utils::read.csv(path, stringsAsFactors = FALSE,
                            na.strings = c("", "NA"), check.names = FALSE)
  required <- c("design_condition_id", "rep_tier", "n_rep", "profile",
                "ICC_y", "J", "nbar", "ICC_x", "rho_PV", "M", "route")
  missing <- setdiff(required, names(design))
  if (length(missing)) {
    stop("Simulation design is missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  design <- design[design$route == "gaussian" & !is.na(design$n_rep) &
                     design$n_rep > 0, , drop = FALSE]
  design <- design[!duplicated(design$design_condition_id), , drop = FALSE]
  if (nrow(design) != 24L || sum(design$n_rep) != 4000L) {
    stop("Paper design must contain 24 conditions and 4,000 replications.",
         call. = FALSE)
  }
  if (!setequal(unique(design$rep_tier),
                c("mainstream", "F-tail", "sensitivity"))) {
    stop("Unexpected replication strata in the paper design.", call. = FALSE)
  }
  design
}

sim_expected_tasks <- function(design = sim_read_design()) {
  rows <- lapply(seq_len(nrow(design)), function(i) {
    data.frame(
      design_condition_id = design$design_condition_id[[i]],
      rep_id = seq_len(as.integer(design$n_rep[[i]])),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

sim_expected_keys <- function(design = sim_read_design()) {
  tasks <- sim_expected_tasks(design)
  out <- tasks[rep(seq_len(nrow(tasks)), each = length(SIM_LEGS)), , drop = FALSE]
  out$leg <- rep(SIM_LEGS, times = nrow(tasks))
  rownames(out) <- NULL
  out
}

sim_key <- function(x) paste(x$design_condition_id, x$rep_id, x$leg,
                             sep = "::")

sim_validate_rows_basic <- function(rows) {
  required <- c("design_condition_id", "rep_id", "leg", "bW", "seW",
                "loW", "hiW", "bB", "seB", "loB", "hiB")
  missing <- setdiff(required, names(rows))
  if (!is.data.frame(rows) || length(missing)) {
    stop("Per-replication evidence is not a data frame with the required schema",
         if (length(missing)) paste0(": ", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  numeric_columns <- setdiff(required, c("design_condition_id", "leg"))
  finite <- vapply(rows[numeric_columns],
                   function(x) is.numeric(x) && all(is.finite(x)), logical(1L))
  if (!all(finite)) {
    stop("Per-replication evidence contains non-finite or non-numeric values.",
         call. = FALSE)
  }
  if (any(rows$rep_id < 1L) || any(rows$rep_id != as.integer(rows$rep_id))) {
    stop("rep_id must contain positive integers.", call. = FALSE)
  }
  if (any(rows$loW > rows$hiW) || any(rows$loB > rows$hiB) ||
      any(rows$seW < 0) || any(rows$seB < 0)) {
    stop("Invalid interval ordering or negative standard error.", call. = FALSE)
  }
  if (!all(rows$leg %in% SIM_LEGS)) {
    stop("Unknown estimator leg in per-replication evidence.", call. = FALSE)
  }
  invisible(TRUE)
}

sim_validate_complete <- function(rows, design = sim_read_design()) {
  sim_validate_rows_basic(rows)
  observed <- sim_key(rows)
  if (anyDuplicated(observed)) {
    stop("Duplicate condition/replication/estimator keys detected.", call. = FALSE)
  }
  expected_rows <- sim_expected_keys(design)
  expected <- sim_key(expected_rows)
  missing <- setdiff(expected, observed)
  unexpected <- setdiff(observed, expected)
  if (length(missing) || length(unexpected)) {
    stop(sprintf(
      "Incomplete simulation evidence: %d missing and %d unexpected keys.",
      length(missing), length(unexpected)), call. = FALSE)
  }
  oracle <- rows$leg == "oracle"
  if (any(rows$bW[oracle] != SIM_TRUTH_W) ||
      any(rows$bB[oracle] != SIM_TRUTH_B) ||
      any(rows$seW[oracle] != 0) || any(rows$seB[oracle] != 0)) {
    stop("Oracle rows do not equal the frozen generating values.", call. = FALSE)
  }
  invisible(TRUE)
}

sim_sort_rows <- function(rows, design = sim_read_design()) {
  condition_order <- sort(design$design_condition_id)
  order_id <- order(match(rows$design_condition_id, condition_order), rows$rep_id,
                    match(rows$leg, SIM_LEGS))
  out <- rows[order_id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

sim_wilson <- function(k, n, z = 1.959964) {
  p <- k / n
  denominator <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denominator
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator
  c(low = center - half, high = center + half)
}

sim_parse_named_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) {
      stop("Arguments must use --name=value syntax: ", arg, call. = FALSE)
    }
    pair <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    out[[pair[[1L]]]] <- paste(pair[-1L], collapse = "=")
  }
  out
}
