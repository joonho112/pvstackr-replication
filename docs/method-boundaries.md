# Method and reporting boundaries

The fixed-effect identity checked here assumes common analytic rows, a common realized design matrix, a fixed common positive-definite working covariance, a flat fixed-effect prior or MLE regime, and equal plausible-value weights. It is a point-estimate identity; it is not a covariance, posterior, variance-component, or PSIS theorem.

The calibrated stack maps the empirical fixed-effect center and covariance of stacked draws to an external Rubin target. Equality of the calibrated and target moments is therefore by construction, not independent corroboration. Variance-component draws are retained for diagnostics but are not calibrated or promoted as confirmatory targets.

Reweighted-stack estimates are reportable only when every plausible value in a country has Pareto k below 0.7. Computation and reportability are separate states. The United States passes this gate; Korea does not. Korea diagnostics are included, while paper-facing reweighted estimates are returned as missing with status `computed_but_withheld`.

The PISA Pipeline A archive uses classic Rubin degrees of freedom. Barnard--Rubin computation remains available when a finite complete-data degree of freedom is supplied, but it is not used to overwrite the frozen paper result.
