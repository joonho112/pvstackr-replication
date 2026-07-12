# Getting started

## 1. Restore the pinned R environment

Install R 4.3 or newer. From the repository root, install `renv` once and
restore the public packages recorded in `renv.lock`:

```sh
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(prompt = FALSE)'
```

This is the required dependency bootstrap for a fresh R installation. The
lockfile contains only public packages; it has no local package path or private
repository. Quarto is needed to render the optional guide, but not to run the
`quick` or `verify` tracks.

## 2. Check the lightweight path

Run:

```sh
Rscript code/00_setup.R --check
Rscript run_all.R --track quick
Rscript run_all.R --track verify
```

The setup check reports missing packages and tools without starting a fit. It
is diagnostic and does not install anything. The quick track reads only
included, sanitized evidence. It writes to `output/`; rerunning it replaces
generated outputs deterministically.

## 3. Check a heavy track before starting

```sh
Rscript run_all.R --track simulation --dry-run -- --mode smoke
Rscript run_all.R --track pisa --dry-run -- --mode full
```

Top-level `--dry-run` prints the resolved child command only. Run a non-executing scientific preflight with:

```sh
Rscript run_all.R --track simulation -- --mode full
Rscript run_all.R --track pisa -- --mode full
```

The PISA preflight requires the authorized local files described in `data/pisa/DATA_NOTICE.md`. It persists a 22-task readiness manifest. Add `--execute` only after reviewing that manifest. Simulation smoke similarly requires an explicit `--execute` to start a live Stan replay.

Full MCMC tracks require CmdStan and a working C++ toolchain. They write compiled models to `output/cache/cmdstan/`, not to `stan/`. The source files under `stan/` are the only model binaries intended for release.

## Runtime profile

| Path | Expected scale |
|---|---|
| Quick exhibits and verification | Minutes |
| Intermediate deterministic calculations | Minutes to tens of minutes |
| Reduced simulation smoke | Tens of minutes, machine-dependent |
| Full simulation | Approximately 63 hours with 12 workers in the reference run |
| PISA cached path | Minutes |
| PISA full path | 22 MCMC fits; machine-dependent and long-running |

All full paths have a persistent task manifest and resume validation. Completion is granted only after every expected key is present, unique, finite, bound to the run signature, and error-free.
