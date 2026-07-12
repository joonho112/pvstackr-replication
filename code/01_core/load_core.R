# =============================================================================
# load_core.R: Source the frozen core engine in dependency order
# =============================================================================
#
# Purpose : Load every frozen core component in dependency order: Rubin
#           pooling, GLS solvers, the Theorem 2.2 validator, the CCC affine
#           map, the stacked fractional posterior engine, the BRR-Fay target,
#           the PSIS reporting gate, and the workflow schema. Expects
#           project_root() to be defined by the caller and sources each file
#           into the global environment. No functions are defined here; this
#           is the single entry point that makes the engine available.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

source(file.path(project_root(), "code", "01_core", "rubin_pool.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "gls_solver.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "theorem.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "ccc.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "calibrated_stack.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "targets.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "psis_gate.R"), local = FALSE)
source(file.path(project_root(), "code", "01_core", "workflow_schema.R"), local = FALSE)
