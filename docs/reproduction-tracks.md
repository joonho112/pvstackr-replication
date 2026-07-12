# Reproduction tracks

The repository separates inexpensive artifact reproduction from optional model
refitting. Start with `quick`; use a heavier track only when its additional
scientific question matters.

| Track | Starts from | Reproduces | Default mode | Typical cost |
|---|---|---|---|---|
| `quick` | Shipped, curated evidence | Manuscript and OSM tables, figures, and headline checks | `shipped` | Minutes; no Stan |
| `intermediate` | Shipped draws, targets, and per-replication evidence | Rubin pooling, CCC, PSIS summaries, aggregation, then exhibits | `cached` | Minutes to tens of minutes |
| `simulation` | Synthetic DGM and deterministic seed schedule | A smoke subset or the complete 4,000-replication parity study | `smoke` | Smoke: tens of minutes; full: about 63 hours with 12 workers |
| `pisa` | Clean released evidence; locally obtained OECD PUF for full mode | Cached evidence checks or the 22-fit reference/stacked analysis | `cached` | Cached: minutes; full: hours and Stan |
| `verify` | Shipped or newly generated artifacts | Checksums, schemas, expected values, reportability rules, and reproduction map | `quick` | Minutes |

## Command contract

Use either the explicit or positional form:

```sh
Rscript run_all.R --track quick
Rscript run_all.R quick
```

Inspect a track without running it:

```sh
Rscript run_all.R --track simulation --dry-run -- --mode smoke
```

Arguments after `--` are passed unchanged to the selected track entrypoint.
The top-level runner accepts `--config PATH` for a root-relative configuration
file and `--check-deps` for an early dependency check. Unknown options, unknown
tracks, missing entrypoints, missing configuration, and nonzero child-process
status all stop with a nonzero exit status.

## Input and output contract

- All repository paths are relative to the project root. No track depends on
  the author's home directory or an adjacent checkout.
- Shipped inputs are read-only. Generated artifacts go under `output/figures`,
  `output/tables`, and `output/results`.
- `quick` and the static guide do not compile Stan models.
- `simulation --mode full` and `pisa --mode full` are opt-in. They must never be
  selected implicitly by a documentation build or routine verification run.
- A computed result can still be non-reportable. In particular, a PSIS result
  that fails its diagnostic gate remains recorded as computed-but-withheld.
- Successful commands exit 0. Configuration, validation, missing-input, and
  scientific-gate failures exit nonzero.

The machine-readable defaults and entrypoint names are in
`config/defaults.yml`. The runner parses and validates that file; a custom
`--config` must use the same schema, define all five tracks, and keep every path
inside the repository.
