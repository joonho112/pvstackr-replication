# Software lineage

The calculations in this repository use a frozen, curated copy of the analysis engine in the paper's `codebase-v3` archive. That engine supports the weighted Gaussian random-intercept models used in the simulation and PISA application.

The separately maintained `pvstackr` package at version 0.1.0 (commit `95fe8c5c3c226cb9dac7300b91b2e5fda56c6ca0`) is conceptually related software. Its public direct-stack and PSIS interfaces reject grouped random-effect terms, its BRR target interface is limited to linear models, and it does not provide the live CmdStan backend used for this paper. It is therefore not an execution dependency of this replication package.

This boundary prevents two misleading claims: that `pvstackr` 0.1.0 generated the archived paper results, or that the paper's random-intercept results can be reproduced by substituting its current public API. No changes are made to the separate software repository.

## Frozen engine provenance

Curated source files retain the scientific implementation used in the archive. `manifest/artifacts.csv` records their public paths and hashes, while the curation ledger records the source logical IDs. Portable wrappers add root-relative configuration, schemas, and reporting guards without changing the archived numerical targets.

Pipeline A intervals are intentionally reproduced from the archived objects. Those objects record classic Rubin degrees of freedom. See `docs/known-source-discrepancies.md` for the difference from the manuscript wording.
