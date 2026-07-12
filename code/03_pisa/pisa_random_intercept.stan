// =============================================================================
// pisa_random_intercept.stan: weighted two-level random-intercept model
// =============================================================================
//
// Purpose : The production Stan model for the PISA 2022 application. A two-level
//           Gaussian random-intercept regression of a reading plausible value on
//           the within-school and between-school components of socioeconomic
//           status (ESCS), fit under informative sampling by a survey-weighted
//           pseudo-likelihood: each student's normal log-density is raised to the
//           power of its normalized final weight w[n] (the `target += w[n] * ...`
//           statement). School effects use a non-centred parameterisation
//           (u_school = tau_school * z_school). Fixed effects: beta[1] is the
//           within-school ESCS slope, beta[2] the between-school ESCS slope.
// Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
//           Monte Carlo Fit for Many Plausible Values. arXiv preprint.
// Author  : JoonHo Lee (jlee296@ua.edu)
// License : MIT
// =============================================================================

data {
  int<lower=1> N;
  int<lower=1> J;
  array[N] int<lower=1, upper=J> school;
  vector[N] y;
  vector[N] escs_within;
  vector[N] escs_between;
  vector<lower=0>[N] w;
}
parameters {
  real alpha;
  vector[2] beta;
  real<lower=0> sigma;
  real<lower=0> tau_school;
  vector[J] z_school;
}
transformed parameters {
  vector[J] u_school = tau_school * z_school;
}
model {
  alpha ~ normal(500, 100);
  beta ~ normal(0, 50);
  sigma ~ student_t(3, 0, 50);
  tau_school ~ student_t(3, 0, 50);
  z_school ~ normal(0, 1);

  for (n in 1:N) {
    target += w[n] * normal_lpdf(
      y[n] | alpha +
        beta[1] * escs_within[n] +
        beta[2] * escs_between[n] +
        u_school[school[n]],
      sigma
    );
  }
}
