# Replication package: One MCMC Fit for Many Plausible Values

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-1f65b7.svg)](https://www.r-project.org/)

This repository is the replication package for

> **One Markov Chain Monte Carlo Fit for Many Plausible Values: A Calibrated
> Stacked Posterior Workflow for Bayesian Multilevel Models of Large-Scale
> Assessment Data.**
> JoonHo Lee, Matthew R. Williams, and Terrance D. Savitsky (2026).
> arXiv preprint.

It contains the code and the shipped, audited evidence that reproduce every
table and figure in the paper's simulation study and PISA 2022 application, plus
the scripts that regenerate those results from scratch. The default path rebuilds
every reported exhibit from included evidence in minutes; separate opt-in tracks
expose the intermediate calculations, a reduced simulation replay, the full
4,000-replication simulation protocol, and the 22-fit PISA protocol.

A rendered walkthrough is the **replication guide** in [`book/`](book/) (built
with `quarto render book`; deployed to GitHub Pages by
[`.github/workflows/pages.yml`](.github/workflows/pages.yml)). Read the guide to
reproduce; read the manuscript to understand the science.

---

## Overview

Large-scale assessments such as PISA do not observe a student's proficiency
directly; they release a small set of *M* **plausible values** (PVs) per
student. The orthodox analysis treats the *M* PVs as *M* multiple imputations:
it fits the analyst's model separately to each PV and combines the estimates with
Rubin's rules — which, for a Bayesian multilevel model estimated by MCMC, means
running the sampler *M* times per analysis. This paper studies whether **one**
MCMC fit can stand in for those *M* fits, through two one-fit workflows whose
correctness is established (and, for one, *gated*) rather than assumed:

- **the calibrated stack**, which fits one stacked fractional model and maps its
  fixed-effect draws, through a single Cholesky-based affine calibration, to the
  same mean and covariance the per-PV workflow would report; and
- **the reweighted stack**, a diagnostic variant reported only when
  Pareto-smoothed importance sampling clears a per-plausible-value reliability
  gate.

## What is reproduced

Two bodies of evidence, both regenerated from shipped, audited data products.

**The simulation study.** A 24-condition Gaussian random-intercept experiment —
12 mainstream conditions at 100 replications, 2 small-school-stress conditions at
400, and 10 PV-sensitivity conditions at 200 (4,000 replications, a 16,000-row
estimator table) — scoring four estimator legs (oracle, a per-PV frequentist
comparator, per-PV MCMC, and the calibrated stack) with Monte Carlo standard
errors. At the headline, the calibrated stack recovers the per-PV fixed-effect
estimates (maximum absolute bias below 0.006) and their nominal coverage
(empirical coverage clustered near 0.95).

**The empirical illustration.** A within/between-school analysis of the effect of
socioeconomic status (ESCS) on PISA 2022 reading for the United States (4,342
students in 150 schools) and Korea (6,391 in 186). The per-PV workflow and the
calibrated stack agree by construction (e.g., the U.S. within/between ESCS
effects are 25.08 / 69.74 score points; Korea's are 27.86 / 92.94), and the
reweighted stack's gate reports the United States (10 of 10 PVs reliable) while
withholding Korea (0 of 10).

## Reproduction tracks

| Track | Recomputes | Stan? | Typical time | Purpose |
|---|---|:--:|---|---|
| `quick` *(default)* | all tables and figures from shipped evidence | No | minutes | read or check the paper |
| `intermediate` | Rubin pooling, CCC targets, aggregation, reporting gates | No | minutes | inspect the transformations |
| `simulation --mode smoke` | a small deterministic subset + negative controls | Yes | minutes | check the full simulation path |
| `simulation --mode full` | 24 conditions, 4,000 replications, from scratch | Yes | ~63 h on 12 workers | rebuild the simulation archive |
| `pisa --mode cached` | PISA evidence from sanitized cached objects | No | minutes | inspect the empirical analysis |
| `pisa --mode full` | 20 per-PV + 2 stacked fits; re-audit the B gate | Yes | hours | refit A/C and re-audit B |
| `verify` | manifests, anchors, reporting gates, hygiene scans | No | minutes | audit the package |

The full simulation is a parallel-hardware job, not a laptop check. The full PISA
path requires locally obtained OECD data (below), performs 22 MCMC fits, and
re-audits Pipeline B from the included per-draw log-ratio archive rather than
regenerating those ratios from a new proposal — a boundary the command reports
explicitly. See [Getting started](docs/getting-started.md) and
[Reproduction tracks](docs/reproduction-tracks.md) before a heavy track.

## Quick start

On a fresh R 4.3 or newer installation, from the package root (or after opening
`pvstackr-replication.Rproj` in RStudio):

```sh
# 1. Restore the pinned package environment (once).
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(prompt = FALSE)'

# 2. Rebuild every reported table and figure from shipped evidence.
Rscript run_all.R --track quick

# 3. Audit the package: manifests, anchors, gates, hygiene.
Rscript run_all.R --track verify
```

Both commands exit with status `0` on success and write only under `output/` and
`verification/reports/`. `Rscript run_all.R --help` lists every option; a dry run
prints the resolved command without executing it:

```sh
Rscript run_all.R --track simulation --dry-run -- --mode smoke
```

## Requirements

- **R >= 4.3** (`code/00_setup.R` enforces the floor; the frozen CI environment
  is R 4.6.0). Restore dependencies with `renv::restore()`.
- **`quick`, `intermediate`, `verify`, `pisa --mode cached`: CRAN only** — no Stan
  toolchain, no microdata.
- **`simulation --mode full` and `pisa --mode full`: add CmdStan** (via
  `cmdstanr`) with a C++ compiler, `make`, and a space-free `TMPDIR`. The full
  PISA path also needs authorized local OECD data.
- **Quarto** is needed only to render the guide in `book/`.

## Repository structure

```
pvstackr-replication/
|-- README.md               # this file
|-- run_all.R               # single entry point: dispatches the five tracks
|-- config/defaults.yml     # track -> entrypoint -> default-mode contract
|-- renv.lock               # pinned R 4.6.0 package environment
|-- CITATION.cff  LICENSE
|-- code/
|   |-- 00_setup.R          # shared environment setup (paths, seeds, preflight)
|   |-- 01_core/            # frozen statistical engine: Rubin pooling, CCC
|   |                       #   calibration, PSIS gate, point-identity, BRR-Fay
|   |-- 02_simulation/      # the 24-condition parity study (DGM, runners, aggregate)
|   |-- 03_pisa/            # PISA 2022 pipeline (import 00 ... manifest 10)
|   |-- 04_exhibits/        # table and figure builders
|   `-- helpers/            # portable paths and backend selection
|-- stan/                   # Stan model source
|-- data/
|   |-- pisa/               # OECD access notice, codebook, source metadata (no records)
|   `-- precomputed/        # shipped, audited simulation and PISA evidence
|-- output/                 # rebuilt tables and figures land here
|-- manifest/               # artifact manifest (SHA-256 ledger) and curation ledger
|-- verification/           # the audit suite, expected values, and reproduction map
|-- tests/                  # core-engine tests and adversarial negative controls
|-- docs/                   # getting-started, tracks, discrepancies, lineage, ...
`-- book/                   # Quarto replication guide -> GitHub Pages
```

## A critical reporting rule

Pipeline B (the reweighted stack) is diagnostic-gated. A country is reportable
only if **every** plausible value has Pareto-$\hat k$ below 0.7. The United
States passes (10 of 10); Korea fails (0 of 10). Korea's diagnostics remain
visible, but its paper-facing reweighted estimates are deliberately returned as
missing with status `computed_but_withheld`, and the tests make accidental
promotion a hard failure.

## Reproduce, then disclose: frozen-source discrepancies

The package reproduces the submitted evidence exactly and separately records six
source discrepancies rather than silently rewriting an archived value or editing
the manuscript. Four concern manuscript claims or display labels:

1. The archived Pipeline A objects use classic-Rubin degrees of freedom, while
   the manuscript describes Barnard–Rubin intervals (the two coincide to the
   printed precision).
2. The archived simulation frequentist comparator was fit by maximum likelihood,
   although the manuscript and its displays label it "per-PV REML."
3. The exact maximum diagnostic $\hat R$ is 1.02023844442964; `1.020` is the
   three-decimal display, not an exact upper bound.
4. No archived producer supports the statement that Korea's between-school
   outcome variance share is roughly twice the U.S. value; it is registered as
   `unverified`.

Two further supplement-description discrepancies concern the separate
data-generation (`20260514`) and MCMC (`20260601`) seed domains and a historical
four-versus-six reduced-footprint count. All six are described in
[Known source discrepancies](docs/known-source-discrepancies.md). The final local
QA scope, independent-review dispositions, and checks deferred until repository
publication are recorded in [Release readiness](docs/release-readiness.md).

## Reproducibility

- **Fixed seeds.** The simulation separates two deterministic seed domains —
  `20260514` for data generation and `20260601` for MCMC — so a sampler change
  cannot perturb the generated data.
- **Verify the shipped evidence.** `Rscript run_all.R --track verify` checks the
  public file set against `manifest/artifacts.csv` (SHA-256 and size), the
  registered numeric anchors, the reporting gates, and publication hygiene, and
  exits non-zero on any mismatch.
- **Byte-level determinism.** Rebuilding the exhibits with `--track quick` and
  re-checking the manifest reproduces the shipped artifacts byte for byte; figure
  PDFs are checked from plot data rather than PDF bytes.
- **Pinned environment.** `renv.lock` captures the analysis-side library (R 4.6.0
  plus the pinned packages). `renv::restore()` reinstalls those versions; treat
  it as guidance toward a matching environment, not a byte-for-byte guarantee of
  the CmdStan C++ backend.

## Software boundary

This repository ships a frozen, portable copy of the exact engine that generated
the paper's random-intercept results. The separately maintained
[**pvstackr**](https://github.com/joonho112/pvstackr) package (version 0.1.0) is
related software, but its current public API does not implement this exact
weighted random-intercept execution path and is **not** a dependency here. See
[Software lineage](docs/software-lineage.md) and
[`verification/pvstackr-compatibility.csv`](verification/pvstackr-compatibility.csv).

## Data availability

PISA 2022 unit-record files are publicly obtainable from the OECD but may not be
redistributed by a third party under the current PISA-specific terms. This
package therefore ships the analysis code, the exact input contract, and
sanitized aggregate and model-based evidence — but **not** the student and school
records. Direct OECD access, attribution, and the local import contract are
described in [`data/PISA_DATA_NOTICE.md`](data/PISA_DATA_NOTICE.md); any
authorized local records live under the git-ignored `data/pisa/local/` path.

## Citation

If you use this replication package, please cite the paper:

```bibtex
@misc{lee2026onemcmc,
  author       = {Lee, JoonHo and Williams, Matthew R. and Savitsky, Terrance D.},
  title        = {One {Markov} Chain {Monte} {Carlo} Fit for Many Plausible Values:
                  A Calibrated Stacked Posterior Workflow for {Bayesian} Multilevel
                  Models of Large-Scale Assessment Data},
  year         = {2026},
  howpublished = {arXiv preprint}
}
```

The workflow's general-purpose implementation is the companion R package:

```bibtex
@misc{pvstackr,
  author = {Lee, JoonHo and Williams, Matthew R. and Savitsky, Terrance D.},
  title  = {{pvstackr}: A Calibrated Stacked Posterior Workflow for
            Plausible-Value Analysis},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/joonho112/pvstackr}
}
```

Machine-readable metadata is in [`CITATION.cff`](CITATION.cff). Questions about
reproducing a result should include the track, the command, `sessionInfo()`, and
the relevant verification report.

## Authors

- **JoonHo Lee**, The University of Alabama — corresponding author and package
  maintainer ([jlee296@ua.edu](mailto:jlee296@ua.edu), GitHub
  [@joonho112](https://github.com/joonho112), ORCID
  [0009-0006-4019-8703](https://orcid.org/0009-0006-4019-8703))
- **Matthew R. Williams**, U.S. Bureau of Labor Statistics
- **Terrance D. Savitsky**, U.S. Bureau of Labor Statistics

## License

Released under the MIT License. Copyright (c) 2026 JoonHo Lee, Matthew R.
Williams, and Terrance D. Savitsky. See [`LICENSE`](LICENSE).
