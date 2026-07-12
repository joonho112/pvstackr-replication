// stan/one_mcmc.stan
// -----------------------------------------------------------------------------
// Representative TWO-LEVEL random-intercept normal model for the SWL Paper 2
// distributed-compute fleet (Phase 5).
//
// PURPOSE (not the production estimator). This model is the canonical
// "one MCMC" precompile target named by the Phase 5 plan (stan/one_mcmc.stan).
// It exists so that:
//   (1) cloud/setup_remote.R can PRECOMPILE a Stan binary once at setup time
//       (never at job runtime — see fleet guide ch04 / ch13), giving every node
//       a cached, loadable model binary;
//   (2) cloud/verify_node.R preflight can do a short real sampling run to prove
//       the CmdStan toolchain + compiled binary actually work on that node;
//   (3) Step 5.4 can test TIER-3 within-architecture byte-identity of raw HMC
//       draws (same arch + compiler + CmdStan + model_binary_digest -> identical
//       draws), WITHOUT claiming cross-architecture byte-identity.
//
// The structure mirrors the two-level (students nested in schools) random-
// intercept form underlying the study's V1 comparator: a school-level random
// intercept plus a student-level fixed slope. Production Pipeline A/B/C fits use
// brms-generated Stan; this hand-written model is a faithful, fast, deterministic
// stand-in for the MCMC workload, not a replacement for them.
// -----------------------------------------------------------------------------
data {
  int<lower=1> N;                 // number of students (level-1 units)
  int<lower=1> J;                 // number of schools  (level-2 units)
  array[N] int<lower=1, upper=J> school;  // school index per student
  vector[N] x;                    // level-1 covariate (e.g. centred predictor)
  vector[N] y;                    // outcome (e.g. plausible value / score)
}
parameters {
  real beta0;                     // grand intercept
  real beta1;                     // level-1 slope
  real<lower=0> sigma_y;          // residual (within-school) SD
  real<lower=0> tau;              // between-school SD
  vector[J] u_raw;                // non-centred school random effects
}
transformed parameters {
  vector[J] u = tau * u_raw;      // school random intercepts
}
model {
  // weakly-informative priors (frozen for reproducibility)
  beta0   ~ normal(0, 5);
  beta1   ~ normal(0, 5);
  sigma_y ~ student_t(3, 0, 2.5);
  tau     ~ student_t(3, 0, 2.5);
  u_raw   ~ std_normal();         // non-centred parameterisation
  // likelihood
  y ~ normal(beta0 + beta1 * x + u[school], sigma_y);
}
generated quantities {
  real icc = (tau^2) / (tau^2 + sigma_y^2);   // intraclass correlation
}
