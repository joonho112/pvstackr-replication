# PARITY_MANIFEST — 2026-07 both-gradient simulation evidence run

Run: 24 preregistered Gaussian conditions x full R (4,000 replications),
four estimators per replication (oracle, per-PV frequentist comparator,
per-PV MCMC, calibrated stack), BOTH fixed-effect slopes recorded per fit.
Launched 2026-07-08 17:20 CDT, completed 2026-07-11 ~08:00 CDT,
12 independent local workers, 0 errors.

Toolchain: R 4.6.0 (arm64), brms 2.23.0, cmdstanr 0.8.0, CmdStan 2.38.0,
lme4; sampler config: 4 chains x 2,000 iter (1,000 warmup),
adapt_delta 0.95, max_treedepth 12; DGM seed root 20260514 and MCMC seed
root 20260601.

The historical display label for the frequentist comparator is “per-PV
REML,” but the archived producer called `lme4::lmer()` with `REML = FALSE`.
The comparator is therefore ML; the public full replay follows that archived
implementation.

The upstream seed source hash shown below is retained as historical provenance.
The public runtime keeps the same deterministic `make_seed()` implementation
but omits unused development-only ledger and substrate-audit helpers that
depended on a package layout absent from this repository. Its curated SHA-256
is `6db223512233767d8a744ba07424fe733b53319826365f8ff54d624139ca9ae4`;
the simulation verifier fixes this hash and tests a frozen representative
substrate and frequentist archive row.

Generative sources consumed match the preregistration hashes recorded in
codebase-v2/log/evidence/step4.7/preregistration_hash_checks.csv
(dgm_gaussian.R, dgm_sampling_weights.R, seeds.R, design.csv). No signed
file was modified by this run.

Validation: (i) reproduces the June 2026 archived run's within-school
results, max |Delta condition mean| = 0.0086; (ii) seed-deterministic
diagnostics re-extraction for 2 reps/condition, identity-verified against
stored shards (diag_summary.csv); (iii) per-PV MCMC vs archived frequentist ML
rep-level agreement across all 4,000 reps, max |Delta| 0.0007 (beta^W),
0.011 (beta^B).

AUTHOR SIGN-OFF: __________________________  date: __________

| artifact | sha256 |
|---|---|
| run_beta_parity.R | 9426160136f5f257f5336c03e184c992fd1bfc3d0268e32c57ceee8c24071fee |
| parity_worker.R | ece768f3f39b337017bb8c87be57b95ef618fc1d3281eab65e0d3e3d935c9712 |
| launch_parity.sh | b7f2e3b4ee73389c52083babd30a958b444a4de29131a82092f7b916e25668cd |
| aggregate_parity.R | 3686c9d785c806f589434a0184d9a176e8d8fdf5ae110fc21818e2709c919372 |
| diag_worker.R | baacd14eb9e6fbd2970e056c07d42729a8dc58234203803ce93f00e363f56f7f |
| finalize_diag.R | 158911211c73a2e07bcaa5f4b7711fbe93b89be4c5b1daac978e4cc011c6487f |
| parity_by_condition.csv | 151b11c5e93203c1685242bc8de9bed52f7f5b193d694dc4c7415635164b3081 |
| parity_by_stratum.csv | 3d65821de98d5849de293709e859939dd2554a943b21ae9cbeb81d9a03dce6e7 |
| parity_per_rep.csv | 24d5dfe2a11c5a3bb0d5d96ea39ab0873216bb6091a82e2928c653d0b17590ab |
| betaW_reproduction_check.csv | 1c5dc8db05f48cf67e4485178f974f9e96a4853603652c00d73320da2ac530a3 |
| diag_summary.csv | 47399cc45a993b5dbd42c227a4ab3f39c94806079dc65591a570691aafb3e328 |
| shards/ (rolled hash over 4000 files, sorted path order) | fa42d78c5469ab5c4025e257a6f3dc51d1fd4144c1b03314d0c4c462e7f0d834 |
| codebase-v2/sim/dgm/dgm_gaussian.R | 71267fad398d700e002f015dd2d54e20762f2136011c2930fc55115166869a98 |
| codebase-v2/sim/dgm/dgm_sampling_weights.R | 15cd8c746d9b1c2a419f0991ec3f56cc2220d0d34498f8f70aead1792346cd02 |
| codebase-v2/sim/run/seeds.R | cd3f9f36a5f4e123a01e5531aa75c38546f80c5332e4bff2d7fd44efe0b1f00b |
| codebase-v2/sim/grid/design.csv | 08085b1e05e42fce81a8163a417e15464156210d2e0dee0a41deaa0d3f5e4752 |
