#!/usr/bin/env Rscript
# =============================================================================
# run_pisa.R: driver for the cached or full PISA empirical track
# =============================================================================
#
# Purpose : Command-line entry point for the PISA track. Default cached mode
#           runs the focused checks; full mode writes a preflight task
#           manifest and (with --execute) re-runs the 20 Pipeline A and 2
#           Pipeline C fits and re-verifies the Pipeline B gate. Resolves the
#           package root, sets the working directory, and runs each stage in a
#           clean Rscript process. Flags: --mode cached|full, --execute.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
mode <- "cached"
if ("--mode" %in% args) {
  i <- match("--mode", args); if (i == length(args)) stop("--mode needs cached or full", call. = FALSE)
  mode <- args[[i+1L]]
}
inline <- grep("^--mode=", args, value = TRUE)
if (length(inline)) mode <- sub("^--mode=", "", inline[[1L]])
if (!mode %in% c("cached", "full")) stop("--mode must be cached or full", call. = FALSE)
execute <- "--execute" %in% args

root <- Sys.getenv("PVSTACKR_REPLICATION_ROOT", unset = getwd())
old <- setwd(root); on.exit(setwd(old), add = TRUE)
source("code/03_pisa/00_config.R")
source("code/00_setup.R")
run <- function(file, extra = character()) {
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", file, extra))
  if (!identical(as.integer(status), 0L)) stop("PISA component failed: ", file, call. = FALSE)
}
pisa_write_full_preflight <- function() {
  a <- readRDS(file.path(PISA_PRECOMPUTED_DIR, "pipeline_a_cached.rds"))$task_schedule
  a <- data.frame(task_id = sprintf("A-%s-PV%02d", a$country, a$pv_index),
                  pipeline = "A", country = a$country, pv_index = a$pv_index,
                  seed = a$seed, output = sprintf("pipeline_a/%s_pv%02d.rds", a$country, a$pv_index))
  c_cache <- readRDS(file.path(PISA_PRECOMPUTED_DIR, "pipeline_c_cached.rds"))$countries
  c <- data.frame(task_id = paste0("C-", names(c_cache)), pipeline = "C",
                  country = names(c_cache), pv_index = NA_integer_,
                  seed = vapply(c_cache, function(x) x$seed, integer(1L)),
                  output = paste0("pipeline_c/", names(c_cache), "_stacked_ccc.rds"))
  tasks <- rbind(a, c)
  dir.create("output/pisa_full/preflight", recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tasks, "output/pisa_full/preflight/task_manifest.csv", row.names = FALSE)
  readiness <- data.frame(local_source = file.exists(PISA_SOURCE_FILE),
                          local_analytic = file.exists(PISA_ANALYTIC_FILE),
                          task_count = nrow(tasks), status = "preflight")
  utils::write.csv(readiness, "output/pisa_full/preflight/readiness.csv", row.names = FALSE)
  tasks
}
if (mode == "cached") {
  run("code/03_pisa/run_pisa_checks.R")
  cat("PISA cached track complete.\n")
} else if (!execute) {
  tasks <- pisa_write_full_preflight()
  pv_check_heavy_toolchain()
  if (!file.exists(PISA_ANALYTIC_FILE)) {
    stop("PISA full track needs authorized local data. Follow data/pisa/DATA_NOTICE.md.", call. = FALSE)
  }
  run("code/03_pisa/04_full_reference_runner.R")
  run("code/03_pisa/06_full_calibrated_runner.R")
  cat("PISA full preflight complete: ", nrow(tasks), " tasks persisted. Add --execute to run all 22 fits.\n", sep = "")
} else {
  run("code/03_pisa/04_full_reference_runner.R", "--run")
  run("code/03_pisa/06_full_calibrated_runner.R", "--run")
  run("code/03_pisa/verify_gate_sensitivity.R")
  cat("PISA full 22-fit A/C track complete; Pipeline B gate verified from the released log-ratio archive.\n")
}
