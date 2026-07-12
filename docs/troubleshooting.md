# Troubleshooting

| Symptom | Check | Resolution |
|---|---|---|
| `Cannot locate repository root` | Current directory and `.Rproj` file | Start anywhere inside the repository or invoke the absolute `run_all.R` path |
| A package is missing | `Rscript code/00_setup.R --check` | Restore `renv.lock` or install the named public package |
| CmdStan is unavailable | `cmdstanr::cmdstan_path()` | Install CmdStan, then rerun the preflight |
| Heavy-track preflight reports `space_free_tmpdir` | `tempdir()` or the startup `TMPDIR` contains a space | Before starting R, set `TMPDIR` to an existing writable path without spaces, then restart R and rerun the preflight |
| `PISA full track needs authorized local data` | `data/pisa/local/source/pisa2022_read_usa_kor.rds` is absent or its source contract has not been completed | Obtain the files directly from OECD and follow `data/pisa/DATA_NOTICE.md`; run the documented importer and build commands, do not bypass the hash checks, and never commit unit records |
| A simulation aggregate refuses to build | Completion report | Resume missing tasks; do not bypass duplicate, error, or missing-key checks |
| Existing shard is rejected | Shard schema and completion flag | Remove only the rejected output and rerun its task; atomic valid shards are reusable |
| Korea Pipeline B estimates are missing | `reporting_status` | This is expected: Korea is computed but withheld because the PSIS gate fails |
| Table 4 df differ from a new calculation | `df_method` | Use `classic` to reproduce the archived paper result; Barnard–Rubin requires a finite complete-data df and answers a different calculation |
| A PDF is visually different but numbers match | fonts and platform metadata | Use the plot-data and numeric verifier as the scientific check; inspect the visual QA report for benign rendering differences |
| Verification reports a hash mismatch | file path in `manifest/artifacts.csv` | Restore the released artifact or regenerate it with its recorded producer; never edit evidence manually |

When reporting a problem, include the exact command, exit status, R `sessionInfo()`, operating system, and the smallest relevant report under `verification/reports/`.
