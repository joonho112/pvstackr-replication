# Simulation reproduction

This directory contains the public reproduction path for the paper's
24-condition Gaussian simulation. The default path reads the complete July
2026 evidence bundle and regenerates all numerical inputs without rerunning
4,000 expensive MCMC replications. An optional full-run path is provided for
readers who want to regenerate every shard.

## What is canonical

The canonical paper evidence is the July 8--11, 2026 both-gradient parity run:

- 24 conditions in three strata;
- 4,000 replications;
- four estimator rows per replication;
- 16,000 complete estimator rows;
- both within-school and between-school slopes;
- 48 diagnostic re-extraction anchors representing 558 MCMC fits.

Earlier June aggregates, IRT pilots, placeholder backends, and the superseded
Path-1 between-slope calculation are not paper-result inputs. The retained
`betaW_reproduction_check.csv` is a validation anchor only: it checks the July
run against the earlier within-slope archive.

## Fast reproduction

From the replication-package root:

```sh
Rscript --vanilla code/02_simulation/verify_simulation.R
Rscript --vanilla code/02_simulation/exhibits/build_simulation_inputs.R \
  --out=output/results/simulation-inputs
```

The first command validates completeness, recomputes condition and stratum
aggregates, checks them against the archived values, checks the diagnostic key
set, and verifies the paper anchors. The second writes:

- `table2_design_input.csv` (3 rows),
- `table3_headline_input.csv` (9 rows),
- `figure2_plot_data.csv` (144 slope-specific rows), and
- `osm_e_condition_results.csv` (72 condition/workflow rows).

The exhibit files are numeric inputs. Formatting and plotting belong to the
separate exhibit stage.

## Evidence files

`data/precomputed/simulation/` contains:

- `simulation_evidence.rds`: compact combined bundle;
- `parity_per_rep.csv`: inspectable 16,000-row evidence table;
- `parity_by_condition.csv` and `parity_by_stratum.csv`: archived aggregates;
- `diagnostics.csv` and `diagnostic_bundle.rds`: diagnostic anchors;
- `betaW_reproduction_check.csv`: July--June validation anchor;
- `artifact_manifest.csv`, `source_hashes.csv`, and `SOURCE_MANIFEST.md`:
  provenance and checksums.

The 4,000 private working shards are replaced by the combined RDS and CSV. No
information needed for the paper aggregates is lost. `aggregate.R` can also
read a complete shard directory and refuses partial input.

Each live shard carries a run signature over the design, seed protocol, DGM,
estimator source, pooling code, and selected configuration, plus a content hash
over its result rows. Resume rejects either mismatch. After all 4,000 full
tasks complete, aggregation compares the new per-replication, condition, and
stratum results with the frozen archive at absolute tolerance `1e-6` before
publishing replay aggregates.

## Code map

- `design/paper_design.csv`: curated 24-condition design.
- `dgm/`: frozen Gaussian generator and sampling-weight source.
- `seeds.R` and `seed_protocol.R`: the curated deterministic DGM seed family
  and explicit MCMC root separation.
- `aggregate.R`: completeness-first aggregation from CSV, RDS, or shards.
- `build_precomputed_bundle.R`: rebuilds compact RDS bundles from canonical
  CSVs after verifying their hashes.
- `verify_simulation.R`: end-to-end archive verification.
- `runner/`: optional single-replication, worker, and launcher path.
- `protocol/FULL-RUN.md`: resource, command, resume, and completion procedure.
- `tests/test_simulation.R`: positive and adversarial focused tests.

## Recorded anchors

- Maximum stratum absolute bias: 0.0013 for `beta_W`, 0.0051 for `beta_B`
  after manuscript rounding.
- Coverage range across the three reported workflows: 0.945--0.986.
- Exact maximum diagnostic R-hat: 1.020238444 (reported as 1.020 after
  three-decimal rounding); zero divergences.
- Maximum per-replication absolute MCMC--archived-frequency difference:
  0.0006818915 for `beta_W` and 0.0106386784 for `beta_B`. The archived
  comparator is ML, although the submitted manuscript labels it REML.
- Maximum July--June within-slope condition-mean difference: 0.0085525693.

See `protocol/FULL-RUN.md` before starting a live replay.
