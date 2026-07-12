# Manuscript and OSM change proposal

No manuscript file was changed. For the authors' later manual revision, the replication audit supports the following minimal edits:

1. Replace the description of the archived Pipeline A intervals as finite-complete-data Barnard–Rubin intervals with wording that matches the recorded classic Rubin calculation, or rerun the analysis under a prespecified finite complete-data df and update all affected values.
2. Replace the simulation comparator's “per-PV REML” description with ML, or regenerate and separately validate a REML archive; the frozen evidence was produced with `REML = FALSE`.
3. Describe the diagnostic maximum as “1.020 after rounding” rather than “at or below 1.020.”
4. Remove, qualify, or supply a separately defined and archived producer for the statement that Korea's between-school outcome variance share is roughly twice the U.S. share.
5. State both simulation seed domains: 20260514 for the DGM and 20260601 for MCMC.
6. Clarify that four conditions had deliberately reduced 50-fit complements, while two additional F-tail conditions completed 397 and 399 of 400 fits; six were below nominal completion under the broader count.
7. Retain the statement that PISA unit records are not included, and point readers to the direct OECD access/import instructions and data notice.
8. Describe `pvstackr` 0.1.0 as related software unless its public random-intercept execution path is expanded and independently versioned.

The user explicitly reserved manuscript editing for a later manual step. Phase 6.16 is therefore recorded as “not authorized,” and Phase 6.17 is a no-op.
