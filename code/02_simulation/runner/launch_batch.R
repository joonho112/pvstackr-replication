#!/usr/bin/env Rscript
# =============================================================================
# launch_batch.R: Parallel worker launcher for the batch replay
# =============================================================================
#
# Purpose : Launch the full simulation batch across independent R workers.
#           Enumerates the 4,000 condition/replication tasks, assigns each to a
#           worker by round-robin, and records the run signature in a task
#           manifest; unless --dry-run, it spawns one Rscript worker per slice
#           (in parallel via mclapply on unix), collects exit statuses and
#           completion files, and fails if any worker did not finish. Flags:
#           --workers, --out, --mode, and --dry-run.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.launcher_root <- function(start = getwd()) {
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
root <- .launcher_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
args <- sim_parse_named_args()
workers <- as.integer(args$workers %||% "12")
output <- args$out %||% file.path(root, "output", "simulation-shards")
mode <- args$mode %||% "full"
dry_run <- tolower(args$`dry-run` %||% "false") %in% c("true", "1", "yes")
if (is.na(workers) || workers < 1L || !mode %in% c("full", "quick")) {
  stop("workers must be positive and mode must be full or quick.", call. = FALSE)
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
tasks <- sim_expected_tasks()
tasks$worker <- (seq_len(nrow(tasks)) - 1L) %% workers
tasks$run_signature <- sim_run_signature(mode)
utils::write.csv(tasks, file.path(output, "task_manifest.csv"), row.names = FALSE)
if (dry_run) {
  cat(sprintf("DRY RUN: %d tasks assigned to %d workers\n", nrow(tasks), workers))
  quit(save = "no", status = 0L)
}
worker_script <- file.path(root, "code", "02_simulation", "runner", "worker.R")
rscript <- file.path(R.home("bin"), "Rscript")
run_worker <- function(index) {
  command <- c("--vanilla", worker_script,
               sprintf("--worker=%d", index), sprintf("--workers=%d", workers),
               paste0("--out=", output), paste0("--mode=", mode))
  status <- system2(rscript, shQuote(command, type = "sh"))
  data.frame(worker = index, exit_status = status,
             completion_file = file.path(output,
               sprintf("worker_%02d_completion.csv", index)),
             stringsAsFactors = FALSE)
}
status <- if (.Platform$OS.type == "unix" && workers > 1L) {
  do.call(rbind, parallel::mclapply(0:(workers - 1L), run_worker,
                                    mc.cores = workers))
} else {
  do.call(rbind, lapply(0:(workers - 1L), run_worker))
}
utils::write.csv(status, file.path(output, "launcher_completion.csv"),
                 row.names = FALSE)
if (any(status$exit_status != 0L) || any(!file.exists(status$completion_file))) {
  stop("At least one worker failed or did not publish a completion file.",
       call. = FALSE)
}
message("All workers completed. Run aggregate.R; it will reject partial shards.")
