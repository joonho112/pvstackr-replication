#!/usr/bin/env Rscript
# =============================================================================
# worker.R: Single batch worker over a task slice
# =============================================================================
#
# Purpose : One worker process in the batch replay. Loads the shared utilities
#           and run_one.R, selects its round-robin slice of the 4,000 tasks
#           (task index modulo worker count), runs each replication through
#           run_simulation_one(), records per-task status and timing, stops on
#           the first error, and atomically publishes a per-worker completion
#           CSV (exiting non-zero if any task failed). Flags: --worker,
#           --workers, and --out.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.worker_root <- function(start = getwd()) {
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
root <- .worker_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(root, "code", "02_simulation", "runner", "run_one.R"),
       local = FALSE)
args <- sim_parse_named_args()
worker <- as.integer(args$worker)
workers <- as.integer(args$workers)
output <- args$out
mode <- args$mode %||% "full"
if (is.na(worker) || is.na(workers) || worker < 0L || workers < 1L ||
    worker >= workers || is.null(output)) {
  stop("Use --worker=0-based-index --workers=count --out=directory",
       call. = FALSE)
}
tasks <- sim_expected_tasks()
mine <- tasks[(seq_len(nrow(tasks)) - 1L) %% workers == worker, , drop = FALSE]
status <- vector("list", nrow(mine))
for (i in seq_len(nrow(mine))) {
  started <- Sys.time()
  observed <- tryCatch(
    run_simulation_one(mine$design_condition_id[[i]], mine$rep_id[[i]],
                       output, mode = mode),
    error = function(e) list(status = "error", path = "",
                             message = conditionMessage(e))
  )
  status[[i]] <- data.frame(
    worker = worker,
    design_condition_id = mine$design_condition_id[[i]],
    rep_id = mine$rep_id[[i]], status = observed$status,
    message = observed$message %||% "", seconds = as.numeric(Sys.time() - started,
                                                               units = "secs"),
    stringsAsFactors = FALSE
  )
  if (identical(observed$status, "error")) break
}
status <- do.call(rbind, status[!vapply(status, is.null, logical(1L))])
dir.create(output, recursive = TRUE, showWarnings = FALSE)
completion <- file.path(output, sprintf("worker_%02d_completion.csv", worker))
staged <- paste0(completion, ".tmp")
utils::write.csv(status, staged, row.names = FALSE)
if (!file.rename(staged, completion)) stop("Could not publish worker completion file.",
                                           call. = FALSE)
if (any(status$status == "error")) quit(save = "no", status = 1L)
