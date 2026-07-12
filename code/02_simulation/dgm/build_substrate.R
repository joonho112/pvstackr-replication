# =============================================================================
# build_substrate.R: Paired data-generating substrate builder
# =============================================================================
#
# Purpose : Provide build_simulation_substrate(), which generates the shared
#           population, plausible values, and complex-sample survey draw for
#           one condition and replication so that all four estimators see an
#           identical substrate. Sources the frozen seeds.R, dgm_gaussian.R,
#           and dgm_sampling_weights.R and uses the preregistered DGM seed
#           family. Returns a list with the sample, PV column names, oracle
#           truths, and the seeds used.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

build_simulation_substrate <- function(condition_id, rep_id,
                                       design = sim_read_design()) {
  root <- sim_find_repo_root()
  source(file.path(root, "code", "02_simulation", "seeds.R"), local = FALSE)
  source(file.path(root, "code", "02_simulation", "dgm",
                   "dgm_gaussian.R"), local = FALSE)
  source(file.path(root, "code", "02_simulation", "dgm",
                   "dgm_sampling_weights.R"), local = FALSE)
  row <- design[design$design_condition_id == condition_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown simulation condition: ", condition_id,
                            call. = FALSE)
  if (!is.numeric(rep_id) || length(rep_id) != 1L || is.na(rep_id) ||
      rep_id != as.integer(rep_id) || rep_id < 1L || rep_id > row$n_rep) {
    stop("rep_id is outside the condition's frozen replication tier.",
         call. = FALSE)
  }

  population_seed <- make_seed(condition_id, rep_id, "population",
                               pipeline = "paired_substrate",
                               root_seed = SIM_DGM_SEED_ROOT)
  pv_seed <- make_seed(condition_id, rep_id, "pv",
                       pipeline = "paired_substrate",
                       root_seed = SIM_DGM_SEED_ROOT)
  sample_seed <- make_seed(condition_id, rep_id, "school_sample",
                           pipeline = "paired_substrate",
                           root_seed = SIM_DGM_SEED_ROOT)

  population <- dgm_gaussian(
    J = 4L * as.integer(row$J), nbar = as.integer(row$nbar),
    icc_y = row$ICC_y, icc_x = row$ICC_x,
    beta_W = SIM_TRUTH_W, beta_B = SIM_TRUTH_B,
    seed = population_seed, cell_id = condition_id,
    population_id = sprintf("%s_%03d_population", condition_id, rep_id)
  )
  population <- dgm_gaussian_pv(
    population, M = as.integer(row$M), rho_PV = row$rho_PV,
    seed = pv_seed
  )
  sample <- sample_survey_design(
    population, seed = sample_seed, J = as.integer(row$J),
    nbar = as.integer(row$nbar), weight_info = row$weight_info,
    sampling_design = row$sampling_design,
    nonresponse_model = row$nonresponse_model, trim_rule = row$trim_rule,
    weight_scaling_rule = row$weight_scaling_rule
  )
  pv_cols <- grep("^PV[0-9]+$", names(sample), value = TRUE)
  if (length(pv_cols) != as.integer(row$M)) {
    stop("Generated plausible-value count does not match the design.",
         call. = FALSE)
  }
  list(
    condition_id = condition_id, rep_id = as.integer(rep_id), design_row = row,
    sample = sample, pv_cols = pv_cols,
    oracle = attr(population, "dgm_gaussian")$oracle,
    seeds = list(dgm_root = SIM_DGM_SEED_ROOT,
                 population = population_seed, pv = pv_seed,
                 school_sample = sample_seed,
                 mcmc_root = SIM_MCMC_SEED_ROOT)
  )
}
