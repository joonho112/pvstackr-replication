#!/usr/bin/env Rscript
# =============================================================================
# test_simulation.R: Completeness, provenance, and negative-control tests
# =============================================================================
#
# Purpose : Fast checks and negative controls for the simulation package,
#           runnable without the heavy MCMC toolchain. Loads the evidence
#           bundle and checks row counts (16,000 per-rep; 96 by-condition; 12
#           by-stratum) and the seed contract; checks deterministic substrate
#           reproducibility; exercises completeness, duplicate, interval, and
#           partial-shard controls; validates shard provenance (tamper, quick
#           vs full, dependency-mutation); and runs the exhibit builder and the
#           launcher dry-run that enumerates all 4,000 tasks.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.test_root <- function(start = getwd()) {
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
root <- .test_root()
source(file.path(root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(root, "code", "02_simulation", "aggregate.R"), local = FALSE)
source(file.path(root, "code", "02_simulation", "seed_protocol.R"),
       local = FALSE)

expect_error <- function(code, pattern) {
  observed <- tryCatch({ force(code); NULL }, error = function(e) e)
  if (is.null(observed) || !grepl(pattern, conditionMessage(observed))) {
    stop("Expected error matching: ", pattern, call. = FALSE)
  }
}

bundle <- readRDS(file.path(root, "data", "precomputed", "simulation",
                            "simulation_evidence.rds"))
sim_validate_complete(bundle$per_rep, bundle$design)
stopifnot(nrow(bundle$per_rep) == 16000L,
          nrow(bundle$by_condition) == 96L,
          nrow(bundle$by_stratum) == 12L)
assert_simulation_seed_contract()

source(file.path(root, "code", "02_simulation", "dgm", "build_substrate.R"),
       local = FALSE)
substrate_a <- build_simulation_substrate("T2-c1", 1L)
substrate_b <- build_simulation_substrate("T2-c1", 1L)
stopifnot(identical(substrate_a$sample, substrate_b$sample),
          identical(substrate_a$seeds, substrate_b$seeds),
          substrate_a$seeds$dgm_root == 20260514L,
          substrate_a$seeds$mcmc_root == 20260601L,
          nrow(substrate_a$sample) == 2000L,
          length(substrate_a$pv_cols) == 10L)

partial <- bundle$per_rep[-1L, ]
expect_error(sim_validate_complete(partial, bundle$design), "missing")
duplicate <- rbind(bundle$per_rep, bundle$per_rep[1L, ])
expect_error(sim_validate_complete(duplicate, bundle$design), "Duplicate")
bad_interval <- bundle$per_rep
bad_interval$loW[[1L]] <- bad_interval$hiW[[1L]] + 1
expect_error(sim_validate_complete(bad_interval, bundle$design),
             "Invalid interval")

# A shard directory must be complete before any row is returned.
partial_shards <- tempfile("partial-shards-")
dir.create(file.path(partial_shards, "T1-c1"), recursive = TRUE)
saveRDS(bundle$per_rep[bundle$per_rep$design_condition_id == "T1-c1" &
                         bundle$per_rep$rep_id == 1L, ],
        file.path(partial_shards, "T1-c1", "rep_001.rds"))
expect_error(sim_read_shards(partial_shards, bundle$design), "Partial shard")
unlink(partial_shards, recursive = TRUE)

# Evidence-path smoke: one complete four-leg replication from each stratum.
representatives <- c(mainstream = "T2-c1", `F-tail` = "T1-c1",
                     sensitivity = "T4-c3")
for (condition in representatives) {
  rows <- bundle$per_rep[bundle$per_rep$design_condition_id == condition &
                           bundle$per_rep$rep_id == 1L, ]
  sim_validate_rows_basic(rows)
  stopifnot(nrow(rows) == 4L, setequal(rows$leg, SIM_LEGS))
}

# Resume validation accepts a complete shard and rejects a corrupted one.
source(file.path(root, "code", "02_simulation", "runner", "run_one.R"),
       local = FALSE)

# The live frequentist leg must reproduce the archive's ML implementation. The
# comparison uses the package's stochastic tolerance (1e-6) rather than a
# machine-precision bound: this leg is a numerical lme4 optimization, so a
# different BLAS/LAPACK build (for example, a Linux CI runner versus the macOS
# machine that produced the archive) converges to the same estimate only up to
# floating-point reproducibility, not bit for bit.
archived_frequency <- .sim_fit_archived_freq(substrate_a)
frozen_frequency <- bundle$per_rep[
  bundle$per_rep$design_condition_id == "T2-c1" &
    bundle$per_rep$rep_id == 1L & bundle$per_rep$leg == "freq_per_pv",
  c("bW", "seW", "loW", "hiW", "bB", "seB", "loB", "hiB")
]
stopifnot(nrow(frozen_frequency) == 1L,
          max(abs(archived_frequency - unlist(frozen_frequency))) < 1e-6)

shard_dir <- tempfile("shard-validation-")
dir.create(file.path(shard_dir, "T2-c1"), recursive = TRUE)
shard_path <- file.path(shard_dir, "T2-c1", "rep_001.rds")
shard_rows <- bundle$per_rep[bundle$per_rep$design_condition_id == "T2-c1" &
                              bundle$per_rep$rep_id == 1L, ]
shard_rows$dgm_seed_root <- SIM_DGM_SEED_ROOT
shard_rows$mcmc_seed_root <- SIM_MCMC_SEED_ROOT
shard_rows$replay_mode <- "full"
shard_rows <- sim_attach_provenance(shard_rows, "full")
saveRDS(shard_rows, shard_path)
stopifnot(sim_validate_shard(shard_path, "T2-c1", 1L))
stopifnot(sim_validate_shard(shard_path, "T2-c1", 1L, "full"),
          !sim_validate_shard(shard_path, "T2-c1", 1L, "quick"))
tampered_rows <- shard_rows
tampered_rows$bW[[1L]] <- 999
saveRDS(tampered_rows, shard_path)
stopifnot(!sim_validate_shard(shard_path, "T2-c1", 1L, "full"))
saveRDS(shard_rows, shard_path)
shard_rows$leg[[4L]] <- "bayes_a"
saveRDS(shard_rows, shard_path)
stopifnot(!sim_validate_shard(shard_path, "T2-c1", 1L))
unlink(shard_dir, recursive = TRUE)

# A transitive DGM dependency change must invalidate an otherwise intact shard.
signature_root <- tempfile("simulation-signature-root-")
signature_files <- sim_signature_source_files()
for (rel in signature_files) {
  target <- file.path(signature_root, rel)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file.path(root, rel), target, overwrite = TRUE))
}
signature_shard <- file.path(signature_root, "signed-shard.rds")
old_wd <- setwd(signature_root)
signed_rows <- bundle$per_rep[bundle$per_rep$design_condition_id == "T2-c1" &
                                bundle$per_rep$rep_id == 1L, ]
signed_rows$dgm_seed_root <- SIM_DGM_SEED_ROOT
signed_rows$mcmc_seed_root <- SIM_MCMC_SEED_ROOT
signed_rows$replay_mode <- "full"
signed_rows <- sim_attach_provenance(signed_rows, "full")
saveRDS(signed_rows, signature_shard)
stopifnot(sim_validate_shard(signature_shard, "T2-c1", 1L, "full"))
write("# deliberate dependency mutation",
      file.path(signature_root, "code", "02_simulation", "dgm",
                "dgm_gaussian.R"), append = TRUE)
stopifnot(!sim_validate_shard(signature_shard, "T2-c1", 1L, "full"))
setwd(old_wd)
unlink(signature_root, recursive = TRUE)

quick_rows <- bundle$per_rep
quick_rows$replay_mode <- "quick"
quick_path <- tempfile(fileext = ".rds")
saveRDS(quick_rows, quick_path)
expect_error(sim_read_evidence(quick_path, bundle$design), "Quick-mode")
unlink(quick_path)

tmp <- tempfile("simulation-inputs-")
dir.create(tmp)
status <- system2(file.path(R.home("bin"), "Rscript"),
                  shQuote(c("--vanilla",
                    file.path(root, "code", "02_simulation", "exhibits",
                              "build_simulation_inputs.R"),
                    paste0("--out=", tmp))))
stopifnot(status == 0L,
          nrow(read.csv(file.path(tmp, "table2_design_input.csv"))) == 3L,
          nrow(read.csv(file.path(tmp, "table3_headline_input.csv"))) == 9L,
          nrow(read.csv(file.path(tmp, "figure2_plot_data.csv"))) == 144L,
          nrow(read.csv(file.path(tmp, "osm_e_condition_results.csv"))) == 72L)
unlink(tmp, recursive = TRUE)

# Launcher dry-run must enumerate all 4,000 tasks without starting an estimator.
dry <- tempfile("simulation-dry-run-")
status <- system2(file.path(R.home("bin"), "Rscript"),
                  shQuote(c("--vanilla",
                    file.path(root, "code", "02_simulation", "runner",
                              "launch_batch.R"),
                    "--workers=12", paste0("--out=", dry),
                    "--mode=full", "--dry-run=true")))
stopifnot(status == 0L,
          nrow(read.csv(file.path(dry, "task_manifest.csv"))) == 4000L)
unlink(dry, recursive = TRUE)

cat("PASS: simulation completeness, negative controls, exhibit inputs, and dry-run launcher\n")
