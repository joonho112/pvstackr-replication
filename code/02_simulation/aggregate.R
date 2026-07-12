# =============================================================================
# aggregate.R: Shard aggregation and frozen-archive comparison
# =============================================================================
#
# Purpose : Reduce the per-replication estimator evidence of the 24-condition
#           Gaussian random-intercept parity study into the paper's aggregate
#           tables. Reads a shard directory, CSV, or RDS bundle; validates
#           schema, completeness, and signed provenance; computes bias, Wilson
#           coverage, and MCSE by condition and by stratum; writes the parity
#           CSVs atomically; and can compare a replay to the frozen archive.
#
# Contents:
#   .aggregate_find_root()  find the replication-package root
#   sim_read_shards()       read + validate a complete shard directory
#   sim_read_evidence()     load evidence: shard dir, CSV, or RDS bundle
#   sim_aggregate()         condition/stratum bias, coverage, and MCSE
#   sim_write_aggregates()  atomically publish the three parity CSVs
#   sim_compare_reference() compare a fresh result to the frozen archive
#   (script entrypoint)     aggregate, optionally compare, then write outputs
#
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.aggregate_find_root <- function(start = getwd()) {
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
source(file.path(.aggregate_find_root(), "code", "02_simulation",
                 "simulation_utils.R"), local = FALSE)

sim_read_shards <- function(shard_dir, design = sim_read_design()) {
  shard_dir <- normalizePath(shard_dir, winslash = "/", mustWork = TRUE)
  tasks <- sim_expected_tasks(design)
  expected_rel <- file.path(tasks$design_condition_id,
                            sprintf("rep_%03d.rds", tasks$rep_id))
  observed_abs <- list.files(shard_dir, pattern = "^rep_[0-9]{3}[.]rds$",
                             recursive = TRUE, full.names = TRUE)
  observed_rel <- substring(normalizePath(observed_abs, winslash = "/"),
                            nchar(shard_dir) + 2L)
  missing <- setdiff(expected_rel, observed_rel)
  unexpected <- setdiff(observed_rel, expected_rel)
  if (length(missing) || length(unexpected)) {
    stop(sprintf("Partial shard archive: %d missing and %d unexpected shard files.",
                 length(missing), length(unexpected)), call. = FALSE)
  }
  pieces <- lapply(seq_along(expected_rel), function(i) {
    path <- file.path(shard_dir, expected_rel[[i]])
    value <- tryCatch(readRDS(path), error = function(e) {
      stop("Unreadable shard ", expected_rel[[i]], ": ", conditionMessage(e),
           call. = FALSE)
    })
    sim_validate_rows_basic(value)
    sim_validate_provenance(value, "full")
    expected_condition <- tasks$design_condition_id[[i]]
    expected_rep <- tasks$rep_id[[i]]
    if (nrow(value) != 4L ||
        !all(value$design_condition_id == expected_condition) ||
        !all(value$rep_id == expected_rep) ||
        !setequal(value$leg, SIM_LEGS)) {
      stop("Shard content does not match its path: ", expected_rel[[i]],
           call. = FALSE)
    }
    value
  })
  rows <- do.call(rbind, pieces)
  rownames(rows) <- NULL
  if ("replay_mode" %in% names(rows) &&
      any(rows$replay_mode != "full")) {
    stop("Quick-mode shards cannot be aggregated as paper evidence.",
         call. = FALSE)
  }
  sim_validate_complete(rows, design)
  sim_sort_rows(rows, design)
}

sim_read_evidence <- function(path, design = sim_read_design()) {
  if (dir.exists(path)) return(sim_read_shards(path, design))
  if (!file.exists(path)) stop("Evidence input does not exist: ", path,
                               call. = FALSE)
  extension <- tolower(tools::file_ext(path))
  value <- if (extension == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else if (extension == "rds") {
    readRDS(path)
  } else {
    stop("Evidence input must be a shard directory, CSV, or RDS.", call. = FALSE)
  }
  if (is.list(value) && is.data.frame(value$per_rep)) value <- value$per_rep
  if ("replay_mode" %in% names(value) && any(value$replay_mode != "full")) {
    stop("Quick-mode rows cannot be aggregated as paper evidence.",
         call. = FALSE)
  }
  sim_validate_complete(value, design)
  sim_sort_rows(value, design)
}

sim_aggregate <- function(rows, design = sim_read_design()) {
  sim_validate_complete(rows, design)
  rows <- merge(rows,
                design[, c("design_condition_id", "rep_tier", "profile",
                           "ICC_y", "J", "nbar", "ICC_x", "rho_PV", "M")],
                by = "design_condition_id", all.x = TRUE, sort = FALSE)

  groups <- split(rows, interaction(rows$design_condition_id, rows$leg,
                                    drop = TRUE, lex.order = TRUE))
  condition <- do.call(rbind, lapply(groups, function(d) {
    n <- nrow(d)
    cover_w <- d$loW <= SIM_TRUTH_W & SIM_TRUTH_W <= d$hiW
    cover_b <- d$loB <= SIM_TRUTH_B & SIM_TRUTH_B <= d$hiB
    wilson_w <- sim_wilson(sum(cover_w), n)
    wilson_b <- sim_wilson(sum(cover_b), n)
    data.frame(
      design_condition_id = d$design_condition_id[[1L]],
      rep_tier = d$rep_tier[[1L]], profile = d$profile[[1L]],
      ICC_y = d$ICC_y[[1L]], J = d$J[[1L]], nbar = d$nbar[[1L]],
      ICC_x = d$ICC_x[[1L]], rho_PV = d$rho_PV[[1L]], M = d$M[[1L]],
      leg = d$leg[[1L]], R = n,
      meanW = mean(d$bW), biasW = mean(d$bW) - SIM_TRUTH_W,
      covW = mean(cover_w), covW_lo = wilson_w[[1L]],
      covW_hi = wilson_w[[2L]], mcseW = stats::sd(d$bW) / sqrt(n),
      meanB = mean(d$bB), biasB = mean(d$bB) - SIM_TRUTH_B,
      covB = mean(cover_b), covB_lo = wilson_b[[1L]],
      covB_hi = wilson_b[[2L]], mcseB = stats::sd(d$bB) / sqrt(n),
      stringsAsFactors = FALSE
    )
  }))
  condition <- condition[order(condition$design_condition_id,
                               match(condition$leg, SIM_LEGS)), , drop = FALSE]
  rownames(condition) <- NULL

  strata <- split(rows, interaction(rows$rep_tier, rows$leg,
                                    drop = TRUE, lex.order = TRUE))
  stratum <- do.call(rbind, lapply(strata, function(d) {
    data.frame(
      stratum = d$rep_tier[[1L]], leg = d$leg[[1L]], fits = nrow(d),
      biasW = mean(d$bW) - SIM_TRUTH_W,
      mcseW = stats::sd(d$bW) / sqrt(nrow(d)),
      covW = mean(d$loW <= SIM_TRUTH_W & SIM_TRUTH_W <= d$hiW),
      biasB = mean(d$bB) - SIM_TRUTH_B,
      mcseB = stats::sd(d$bB) / sqrt(nrow(d)),
      covB = mean(d$loB <= SIM_TRUTH_B & SIM_TRUTH_B <= d$hiB),
      stringsAsFactors = FALSE
    )
  }))
  stratum_order <- c("mainstream", "F-tail", "sensitivity")
  stratum <- stratum[order(match(stratum$stratum, stratum_order),
                           match(stratum$leg, SIM_LEGS)), , drop = FALSE]
  rownames(stratum) <- NULL
  list(per_rep = sim_sort_rows(rows[, c(
         "design_condition_id", "rep_id", "leg", "bW", "seW", "loW",
         "hiW", "bB", "seB", "loB", "hiB")], design),
       by_condition = condition, by_stratum = stratum)
}

sim_write_aggregates <- function(result, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(per_rep = "parity_per_rep.csv",
             by_condition = "parity_by_condition.csv",
             by_stratum = "parity_by_stratum.csv")
  staged <- file.path(output_dir, paste0(".", unname(files), ".tmp"))
  final <- file.path(output_dir, unname(files))
  names(staged) <- names(final) <- names(files)
  on.exit(unlink(staged), add = TRUE)
  for (name in names(files)) {
    utils::write.csv(result[[name]], staged[[name]], row.names = FALSE,
                     na = "")
  }
  for (i in seq_along(final)) {
    if (!file.rename(staged[[i]], final[[i]])) {
      stop("Could not atomically publish aggregate: ", final[[i]],
           call. = FALSE)
    }
  }
  invisible(final)
}

sim_compare_reference <- function(result, reference_dir, tolerance = 1e-6) {
  reference_dir <- normalizePath(reference_dir, winslash = "/", mustWork = TRUE)
  source(sim_path("verification", "compare_numeric.R"), local = TRUE)
  files <- c(per_rep = "parity_per_rep.csv",
             by_condition = "parity_by_condition.csv",
             by_stratum = "parity_by_stratum.csv")
  keys <- list(per_rep = c("design_condition_id", "rep_id", "leg"),
               by_condition = c("design_condition_id", "leg"),
               by_stratum = c("stratum", "leg"))
  reports <- lapply(names(files), function(name) {
    expected_path <- file.path(reference_dir, files[[name]])
    if (!file.exists(expected_path)) {
      stop("Reference aggregate is missing: ", expected_path, call. = FALSE)
    }
    expected <- utils::read.csv(expected_path, stringsAsFactors = FALSE,
                                check.names = FALSE)
    report <- compare_numeric_tables(result[[name]], expected, keys[[name]],
                                     tolerance = tolerance)
    report$artifact <- name
    if (any(!report$pass)) {
      stop("Full replay differs from the frozen archive for ", name, ": ",
           paste(report$column[!report$pass], collapse = ", "), call. = FALSE)
    }
    report
  })
  do.call(rbind, reports)
}

if (sys.nframe() == 0L) {
  args <- sim_parse_named_args()
  input <- args$input %||% sim_path("data", "precomputed", "simulation",
                                    "simulation_evidence.rds")
  output <- args$output %||% sim_path("output", "results", "simulation")
  result <- sim_aggregate(sim_read_evidence(input))
  comparison <- NULL
  if (!is.null(args$reference)) {
    tolerance <- as.numeric(args$tolerance %||% "1e-6")
    if (!is.finite(tolerance) || tolerance < 0) {
      stop("--tolerance must be a nonnegative finite number.", call. = FALSE)
    }
    comparison <- sim_compare_reference(result, args$reference, tolerance)
  }
  sim_write_aggregates(result, output)
  if (!is.null(comparison)) {
    utils::write.csv(comparison, file.path(output, "replay_comparison.csv"),
                     row.names = FALSE)
  }
  message("Simulation aggregation complete: ", normalizePath(output))
}
