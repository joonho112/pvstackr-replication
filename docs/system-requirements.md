# System requirements

## Minimum setup

- R 4.3.0 or newer for source-level execution. R 4.6.0 is the frozen release
  environment and is used in CI; use it when byte-identical PDF hashes matter.
- A UTF-8 locale.
- Enough disk space to retain generated tables, figures, and temporary model
  output. The shipped package is designed to remain well below GitHub's
  per-file limit.

The `quick` track uses ordinary CRAN reporting packages and does not require a
C++ compiler, CmdStan, LaTeX, or Quarto. The rendered reader guide is built from
static inputs and likewise does not trigger model fitting.

## Optional heavy-computation setup

The `simulation` and full `pisa` modes require:

- a C++17 compiler supported by the installed R toolchain;
- `cmdstanr` and CmdStan 2.36 or newer;
- `brms`, `posterior`, `Matrix`, and `lme4`;
- sufficient memory for the selected model and worker count.

Full simulation uses independent R processes rather than forked CmdStan jobs.
The reference run used 12 workers and took about 63 elapsed hours. Start with a
single smoke replication before increasing parallelism.

The PISA full path comprises 20 per-PV fits and two country-level stacked fits.
The included cached route is the normal reproduction path when refitting is not
the question of interest.

## Environment policy

The repository lockfile, once frozen for release, is authoritative for R package
versions. It must not contain a local `../` dependency. CmdStan is checked
separately because it is installed outside the R package library. The setup code
pins numerical library thread counts to one per R process to avoid accidental
oversubscription.

Run an early package check with:

```sh
Rscript run_all.R --track simulation --dry-run --check-deps
```

No cloud account, API token, private data path, or adjacent source repository is
required by any public track.
