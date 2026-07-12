#!/usr/bin/env Rscript
# =============================================================================
# run_simulation.R: Top-level simulation entry point
# =============================================================================
#
# Purpose : Top-level entry point for the simulation study. Verifies the frozen
#           archive and control scripts and runs the test suite; with --execute
#           it performs either a reduced smoke replay (one condition and
#           replication in quick mode) or the full 4,000-task batch launch,
#           aggregation, and frozen-archive comparison. In full mode without
#           --execute it runs a preflight dry-run. Flags: --mode smoke|full
#           and --execute.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
mode <- "smoke"
if ("--mode" %in% args) {
  i <- match("--mode", args); if (i == length(args)) stop("--mode needs smoke or full", call. = FALSE)
  mode <- args[[i+1L]]
}
inline <- grep("^--mode=", args, value = TRUE)
if (length(inline)) mode <- sub("^--mode=", "", inline[[1L]])
if (!mode %in% c("smoke", "full")) stop("--mode must be smoke or full", call. = FALSE)
execute <- "--execute" %in% args
root <- Sys.getenv("PVSTACKR_REPLICATION_ROOT", unset = getwd())
old <- setwd(root); on.exit(setwd(old), add = TRUE)
run <- function(file, extra = character()) {
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", file, extra))
  if (!identical(as.integer(status), 0L)) stop("Simulation component failed: ", file, call. = FALSE)
}
run("code/02_simulation/verify_simulation.R")
run("code/02_simulation/tests/test_simulation.R")
if (mode == "smoke" && execute) {
  run("code/02_simulation/runner/run_one.R", c("--condition=T2-c1", "--rep=1", "--out=output/simulation-smoke", "--mode=quick"))
  cat("Simulation live smoke replay complete.\n")
} else if (mode == "full" && execute) {
  run("code/02_simulation/runner/launch_batch.R", c("--workers=12", "--out=output/simulation-shards", "--mode=full"))
  run("code/02_simulation/aggregate.R", c(
    "--input=output/simulation-shards",
    "--output=output/results/simulation-full",
    "--reference=data/precomputed/simulation",
    "--tolerance=1e-6"
  ))
  cat("Simulation full replay complete.\n")
} else if (mode == "full") {
  source("code/00_setup.R")
  pv_check_heavy_toolchain()
  run("code/02_simulation/runner/launch_batch.R", c("--workers=12", "--out=output/simulation-shards", "--mode=full", "--dry-run=true"))
  cat("Simulation full preflight complete. Add --execute to start 4,000 tasks.\n")
} else {
  cat("Simulation archive and smoke controls verified. Add --execute for one live reduced replay.\n")
}
