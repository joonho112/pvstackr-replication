# Full simulation replay protocol

The paper results use the archived July 2026 evidence by default. A full replay
is optional because it entails 4,000 generated datasets, ten per-PV fits in
many conditions, and one stacked fit per replication. The archived run used 12
independent workers and took approximately 63 hours on the recorded machine.

## Fixed scientific configuration

- 24 Gaussian conditions and 4,000 replications.
- Four rows per replication: oracle, the archived per-PV frequentist ML
  comparator, per-PV MCMC, and the one-fit stacked workflow used in the
  archived parity run. The submitted manuscript labels the ML comparator
  “per-PV REML”; replay code follows the archived implementation.
- Generating values: `beta_W = 0.40`, `beta_B = 0.60`.
- DGM seed-family root: `20260514`.
- MCMC root: `20260601`. Per-PV fit `m` uses `20260601 + m`; the stacked fit
  uses `20260601`.
- Four chains, 2,000 iterations, 1,000 warmup, `adapt_delta = 0.95`, and
  `max_treedepth = 12`.

The DGM and sampler seed roots are deliberately separate. Changing sampler
settings cannot change the generated population, selected sample, or plausible
values.

## Prerequisites

Install R, `digest`, `lme4`, `brms`, `posterior`, and `cmdstanr`, then install
CmdStan. The archived toolchain used R 4.6.0, brms 2.23.0, cmdstanr 0.8.0, and
CmdStan 2.38.0. A newer compatible toolchain may produce numerically equivalent
rather than byte-identical MCMC output.

CmdStan temporary paths must not contain spaces. The project directory may
contain spaces, but set a space-free temporary directory before launch:

```sh
mkdir -p "$HOME/.pvstackr-sim/tmp"
export TMPDIR="$HOME/.pvstackr-sim/tmp"
```

Each worker fits chains serially. Parallelism is across replications, avoiding
oversubscription and the fork-related CmdStan failures encountered during the
original study.

## Dry run

From the replication-package root:

```sh
Rscript --vanilla code/02_simulation/runner/launch_batch.R \
  --workers=12 \
  --out=output/simulation-shards \
  --mode=full \
  --dry-run=true
```

This writes a 4,000-row task manifest without fitting a model.

## Reduced live replay

The `quick` mode uses two chains and 400 iterations solely to exercise the live
fit path. It is not paper evidence. Run one representative replication from
each stratum:

```sh
Rscript --vanilla code/02_simulation/runner/run_one.R \
  --condition=T2-c1 --rep=1 --out=output/simulation-smoke --mode=quick
Rscript --vanilla code/02_simulation/runner/run_one.R \
  --condition=T1-c1 --rep=1 --out=output/simulation-smoke --mode=quick
Rscript --vanilla code/02_simulation/runner/run_one.R \
  --condition=T4-c3 --rep=1 --out=output/simulation-smoke --mode=quick
```

These conditions represent the mainstream, small-school stress, and PV
sensitivity strata. Do not combine `quick` shards with full-run shards.

## Full launch and monitoring

```sh
Rscript --vanilla code/02_simulation/runner/launch_batch.R \
  --workers=12 \
  --out=output/simulation-shards \
  --mode=full
```

Each worker publishes a completion CSV and returns a nonzero exit status after
its first error. The launcher records every worker exit status and fails if a
completion file is absent. Each shard is signed against the design, seeds,
source files, selected configuration, and its result content. Only a matching
existing shard is skipped; malformed or stale shards stop the worker and
require inspection before `--force=true` is used on a single replication.

Monitor disk use and worker completions rather than editing shards. The original
4,000 RDS shards occupied about 16 MB; Stan temporary output can be much larger
and should remain in the space-free scratch directory.

## Completion and aggregation

Aggregation is intentionally stricter than the workers. It requires exactly
4,000 shard files and all 16,000 condition/replication/estimator keys before it
returns any result:

```sh
Rscript --vanilla code/02_simulation/aggregate.R \
  --input=output/simulation-shards \
  --output=output/results/simulation-full \
  --reference=data/precomputed/simulation \
  --tolerance=1e-6
```

One missing, duplicate, corrupt, or unexpected shard is a hard failure. Partial
aggregates are never published. The reference comparison runs before the new
aggregates are published and writes `replay_comparison.csv` on success. MCMC
output is evaluated with the declared numerical tolerance, not byte hashes.
