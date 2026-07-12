#!/usr/bin/env Rscript
# =============================================================================
# run_one.R: One replication through the four estimators
# =============================================================================
#
# Purpose : Core per-replication worker for the parity study. Builds the shared
#           substrate, then fits the four estimator legs -- oracle truth,
#           per-PV frequentist (lme4, Rubin-pooled), per-PV MCMC (brms and
#           CmdStan, Rubin-pooled), and the calibrated fractional-weight stack.
#           Assembles a signed four-row shard, validates it, and publishes it
#           atomically with content-bound resume and quick or full sampler
#           settings. Writes one rep_NNN.rds shard per condition/replication.
#
# Contents:
#   .runone_find_root()           find the replication-package root
#   .sim_pick()                   pull within/between slopes and CIs from a fit
#   .sim_require_live_packages()  require lme4/brms/CmdStan and a clean TMPDIR
#   .sim_fit_archived_freq()      per-PV frequentist (lme4) with Rubin pooling
#   .sim_brms_draws()             one Gaussian multilevel MCMC fit to draws
#   .sim_fit_mcmc_per_pv()        per-PV MCMC fits with Rubin pooling
#   .sim_fit_stack()              calibrated fractional-weight stacked fit
#   .sim_result_frame()           assemble and sign the four-leg result rows
#   sim_validate_shard()          validate a shard's schema and provenance
#   run_simulation_one()          run one replication; resume + atomic write
#   (script entrypoint)           parse CLI and run one condition/replication
#
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

.runone_find_root <- function(start = getwd()) {
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
.runone_root <- .runone_find_root()
source(file.path(.runone_root, "code", "02_simulation", "simulation_utils.R"),
       local = FALSE)
source(file.path(.runone_root, "code", "02_simulation", "seed_protocol.R"),
       local = FALSE)
source(file.path(.runone_root, "code", "02_simulation", "dgm",
                 "build_substrate.R"), local = FALSE)
source(file.path(.runone_root, "code", "01_core", "rubin_pool.R"),
       local = FALSE)

.sim_pick <- function(pooled) {
  c(bW = unname(pooled$beta[["x_within"]]),
    seW = unname(pooled$se[["x_within"]]),
    loW = unname(pooled$ci_low[["x_within"]]),
    hiW = unname(pooled$ci_high[["x_within"]]),
    bB = unname(pooled$beta[["xbar_j"]]),
    seB = unname(pooled$se[["xbar_j"]]),
    loB = unname(pooled$ci_low[["xbar_j"]]),
    hiB = unname(pooled$ci_high[["xbar_j"]]))
}

.sim_require_live_packages <- function() {
  needed <- c("lme4", "brms", "posterior", "cmdstanr", "digest")
  missing <- needed[!vapply(needed, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) stop("Full replay needs R package(s): ",
                            paste(missing, collapse = ", "), call. = FALSE)
  cmdstan <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cmdstan) || !dir.exists(cmdstan)) {
    stop("Full replay needs an installed CmdStan toolchain.", call. = FALSE)
  }
  if (grepl(" ", tempdir(), fixed = TRUE)) {
    stop("CmdStan requires a space-free TMPDIR for this project path.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.sim_fit_archived_freq <- function(substrate) {
  beta <- matrix(NA_real_, nrow = length(substrate$pv_cols), ncol = 3L,
                 dimnames = list(substrate$pv_cols,
                                 c("(Intercept)", "x_within", "xbar_j")))
  covariances <- vector("list", length(substrate$pv_cols))
  for (m in seq_along(substrate$pv_cols)) {
    formula <- stats::as.formula(sprintf(
      "%s ~ x_within + xbar_j + (1 | school_id)", substrate$pv_cols[[m]]
    ))
    # The frozen July parity archive was produced by the source implementation
    # with REML = FALSE.  Keep the live replay aligned with that computational
    # authority even though the submitted manuscript labels this leg "REML".
    fit <- lme4::lmer(
      formula, data = substrate$sample, REML = FALSE,
      control = lme4::lmerControl(check.conv.singular = "ignore")
    )
    beta[m, ] <- lme4::fixef(fit)[colnames(beta)]
    covariances[[m]] <- as.matrix(stats::vcov(fit))[colnames(beta),
                                                    colnames(beta)]
  }
  .sim_pick(rubin_pool_matrix(beta, covariances, orientation = "rows_pv"))
}

.sim_brms_draws <- function(formula, data, seed, config) {
  fit <- brms::brm(
    formula, data = data, family = stats::gaussian(),
    chains = config$chains, iter = config$iter, warmup = config$warmup,
    cores = 1L, seed = as.integer(seed), refresh = 0, silent = 2,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    save_pars = brms::save_pars(all = FALSE)
  )
  draws <- as.matrix(posterior::as_draws_matrix(fit))
  columns <- c("b_Intercept", "b_x_within", "b_xbar_j")
  draws[, columns, drop = FALSE]
}

.sim_fit_mcmc_per_pv <- function(substrate, config) {
  draws <- lapply(seq_along(substrate$pv_cols), function(m) {
    formula <- stats::as.formula(sprintf(
      "%s ~ x_within + xbar_j + (1 | school_id)", substrate$pv_cols[[m]]
    ))
    .sim_brms_draws(formula, substrate$sample, SIM_MCMC_SEED_ROOT + m, config)
  })
  beta <- do.call(rbind, lapply(draws, colMeans))
  colnames(beta) <- c("(Intercept)", "x_within", "xbar_j")
  covariances <- lapply(draws, function(x) {
    out <- stats::cov(x)
    dimnames(out) <- list(colnames(beta), colnames(beta))
    out
  })
  .sim_pick(rubin_pool_matrix(beta, covariances, orientation = "rows_pv"))
}

.sim_fit_stack <- function(substrate, config) {
  M <- length(substrate$pv_cols)
  pieces <- lapply(seq_along(substrate$pv_cols), function(m) {
    data <- substrate$sample
    data$.stack_outcome <- data[[substrate$pv_cols[[m]]]]
    data$.fractional_weight <- 1 / M
    data
  })
  long <- do.call(rbind, pieces)
  formula <- .stack_outcome | weights(.fractional_weight) ~
    x_within + xbar_j + (1 | school_id)
  draws <- .sim_brms_draws(formula, long, SIM_MCMC_SEED_ROOT, config)
  center <- colMeans(draws)
  covariance <- stats::cov(draws)
  names(center) <- c("(Intercept)", "x_within", "xbar_j")
  dimnames(covariance) <- list(names(center), names(center))
  se <- sqrt(diag(covariance))
  z <- stats::qnorm(0.975)
  .sim_pick(list(beta = center, se = se,
                 ci_low = center - z * se, ci_high = center + z * se))
}

.sim_result_frame <- function(substrate, legs, mode) {
  out <- do.call(rbind, lapply(names(legs), function(leg) {
    value <- legs[[leg]]
    data.frame(
      design_condition_id = substrate$condition_id,
      rep_id = substrate$rep_id, leg = leg,
      bW = value[["bW"]], seW = value[["seW"]],
      loW = value[["loW"]], hiW = value[["hiW"]],
      bB = value[["bB"]], seB = value[["seB"]],
      loB = value[["loB"]], hiB = value[["hiB"]],
      dgm_seed_root = SIM_DGM_SEED_ROOT,
      mcmc_seed_root = SIM_MCMC_SEED_ROOT,
      replay_mode = mode, stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  sim_validate_rows_basic(out)
  if (nrow(out) != 4L || !setequal(out$leg, SIM_LEGS)) {
    stop("Single-replication output is incomplete.", call. = FALSE)
  }
  sim_attach_provenance(out, mode)
}

sim_validate_shard <- function(path, condition_id, rep_id,
                               expected_mode = NULL) {
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(value)) return(FALSE)
  tryCatch({
    sim_validate_rows_basic(value)
    mode <- expected_mode %||% if ("replay_mode" %in% names(value) &&
                                    length(unique(value$replay_mode)) == 1L) {
      unique(value$replay_mode)
    } else "full"
    sim_validate_provenance(value, mode)
    nrow(value) == 4L && setequal(value$leg, SIM_LEGS) &&
      all(value$design_condition_id == condition_id) &&
      all(value$rep_id == rep_id)
  }, error = function(e) FALSE)
}

run_simulation_one <- function(condition_id, rep_id, output_dir,
                               mode = c("full", "quick"), force = FALSE) {
  mode <- match.arg(mode)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  shard <- file.path(output_dir, condition_id, sprintf("rep_%03d.rds", rep_id))
  if (file.exists(shard) && !isTRUE(force)) {
    if (!sim_validate_shard(shard, condition_id, rep_id, mode)) {
      stop("Existing shard is invalid; use --force=true only after inspection: ",
           shard, call. = FALSE)
    }
    return(invisible(list(status = "skip", path = shard)))
  }
  .sim_require_live_packages()
  assert_simulation_seed_contract()
  config <- if (mode == "full") {
    list(chains = 4L, iter = 2000L, warmup = 1000L)
  } else {
    list(chains = 2L, iter = 400L, warmup = 200L)
  }
  substrate <- build_simulation_substrate(condition_id, rep_id)
  oracle <- c(bW = substrate$oracle$beta_W, seW = 0,
              loW = substrate$oracle$beta_W, hiW = substrate$oracle$beta_W,
              bB = substrate$oracle$beta_B, seB = 0,
              loB = substrate$oracle$beta_B, hiB = substrate$oracle$beta_B)
  legs <- list(
    oracle = oracle,
    freq_per_pv = .sim_fit_archived_freq(substrate),
    bayes_a = .sim_fit_mcmc_per_pv(substrate, config),
    c_direct = .sim_fit_stack(substrate, config)
  )
  result <- .sim_result_frame(substrate, legs, mode)
  dir.create(dirname(shard), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile(pattern = "shard-", tmpdir = dirname(shard),
                     fileext = ".rds")
  on.exit(unlink(staged), add = TRUE)
  saveRDS(result, staged, version = 3)
  if (!sim_validate_shard(staged, condition_id, rep_id, mode) ||
      !file.rename(staged, shard)) {
    stop("Could not validate and atomically publish shard: ", shard,
         call. = FALSE)
  }
  invisible(list(status = "ok", path = shard))
}

if (sys.nframe() == 0L) {
  args <- sim_parse_named_args()
  required <- c("condition", "rep", "out")
  missing <- required[!required %in% names(args)]
  if (length(missing)) stop("Missing CLI argument(s): ",
                            paste(missing, collapse = ", "), call. = FALSE)
  result <- run_simulation_one(
    args$condition, as.integer(args$rep), args$out,
    mode = args$mode %||% "full",
    force = tolower(args$force %||% "false") %in% c("true", "1", "yes")
  )
  cat(result$status, result$path, "\n")
}
