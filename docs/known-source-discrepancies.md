# Known source discrepancies

This record distinguishes faithful reproduction from editorial correction. The package does not change the submitted manuscript.

## Pipeline A degrees of freedom

The submitted text calls the Table 4 intervals Barnard–Rubin intervals. The archived object that produced those endpoints records `df_method = "classic"` and `df_complete = NA`; the production function also used its default `classic` method. The displayed degrees of freedom and endpoints are therefore classic Rubin results. The verifier checks those archived values. A finite-complete-data Barnard–Rubin option is available in the core function, but it is not substituted after the fact.

## Simulation frequentist comparator

The frozen July simulation archive was produced by per-PV `lme4::lmer()` fits with `REML = FALSE`, so the frequentist comparator is an ML estimator. The submitted manuscript and its display labels call this comparator “per-PV REML.” The full replay follows the archived ML implementation so that newly generated results can be compared with the frozen archive. Paper-facing tables and figures retain the submitted label solely to reproduce the manuscript exactly.

## Diagnostic R-hat precision

The exact maximum across the 48 archived diagnostic replications is 1.02023844442964. The manuscript's 1.020 value is correct only after rounding to three decimals. Verification stores the exact value and the display rule separately.

## Between-school variance-share sentence

No archived table or producer was found for the sentence describing Korea's between-school outcome variance share as roughly twice the U.S. value. The covariate-adjusted variance-component draws do not support that wording under the most direct ICC calculation. The claim is therefore marked `unverified manuscript statement`; the package does not create a post hoc statistic to support it.

## OSM archive descriptions

The executable seed contract uses `20260514` for data generation and `20260601` for MCMC; the OSM reports only the first. The historical four-versus-six count uses two different denominators: four conditions were intentionally assigned 50-fit complements, while two additional F-tail conditions completed 397 and 399 of 400 nominal fits. Thus six cells were below nominal completion, but four had a deliberately reduced footprint. The historical June inventory establishing those counts is not a shipped public artifact, so this item is retained as a disclosure-only source discrepancy rather than presented as an independently reproducible package result.

## Data availability

The submitted OSM says the archive does not include PISA files. That remains correct for the public repository: current PISA-specific terms prohibit third-party redistribution. The package supplies a direct OECD access/import path and keeps any local unit-record files in an ignored directory.
