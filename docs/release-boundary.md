# Release boundary

This repository reproduces the computational evidence for the submitted manuscript from frozen source snapshots. It is assembled from an explicit allowlist. Development logs, cloud launch material, compiled Stan executables, private paths, obsolete analyses, and credentials are outside the release boundary.

## Authoritative sources

- `manuscript_v5` is the read-only authority for the set of reported claims and displays.
- `codebase_v3` is the authority for the computations and archived evidence used in the paper.
- `pvstackr` version 0.1.0 (commit `95fe8c5c3c226cb9dac7300b91b2e5fda56c6ca0`) is related software, not the execution engine for the paper's random-intercept analysis.

The replication repository contains a frozen, portable copy of the minimum analysis engine needed to reproduce the paper. It does not modify or silently substitute the separately maintained `pvstackr` package.

PISA unit-record data are outside the public release boundary because the current PISA-specific OECD terms prohibit third-party redistribution. The public tree contains the import code, expected local checksum, and aggregated/model-based evidence; authorized local files remain under ignored `data/pisa/local/`.

## Known source discrepancies

The archived Pipeline A object records `df_method = "classic"` and `df_complete = NA`. Consequently, the Table 4 intervals in the frozen evidence use the classic Rubin degrees of freedom, although the manuscript describes them as Barnard--Rubin intervals. The package reproduces the archived values and exposes this difference in its verification report.

The archived simulation's frequentist comparator used ML (`REML = FALSE`), although the manuscript and paper-facing displays label it “per-PV REML.” The full replay follows the archived ML implementation; display builders retain the submitted label for exact artifact reproduction.

The exact maximum simulation R-hat is 1.02023844442964. The value 1.020 is its three-decimal display, not an exact upper bound.

No archived producer was found for the statement that Korea's between-school outcome variance share was roughly twice the United States value. That statement is registered as unverified rather than reverse-engineered.

These items are manuscript-facing editorial issues. In accordance with the release instructions, this repository records a proposed change memo but does not edit the manuscript.

## Publication stops

Publication remains blocked if any public candidate contains a credential, private absolute path, compiled executable, unlicensed third-party material, incomplete simulation aggregate, or paper-facing Korea Pipeline B estimate. Publication also remains blocked if a registered display has no producer or verification rule.

Git initialization, commits, remotes, releases, and GitHub publication are not part of Phases 1--8 and are not performed here.
