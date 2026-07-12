# PISA 2022 data access notice

PISA student- and school-level unit records are **not distributed in this repository**. Although the OECD describes the files as Public Use Files, the current PISA-specific access terms state that PISA data must not be distributed, disclosed, or made available to a third party. Those dataset-specific terms control this release decision.

Obtain the PISA 2022 student and school files directly through the [OECD PISA 2022 Database](https://www.oecd.org/en/data/datasets/pisa-2022-database.html) and accept the [PISA PUF Terms of Use](https://survey.oecd.org/index.php?lang=en&r=survey%2Findex&sid=197663). Do not commit the downloaded files or derived unit-record files to a public repository.

The analysis used cycle `CY08MSP`, specifically the extracted SAS files below.
OECD does not assign a semantic version number to these files, so exact content
hashes—not timestamps—define the frozen source version. The archive links are
the OECD links labelled “Student questionnaire data file” and “School
questionnaire data file.”

| Role | OECD archive | Extracted member | Bytes | SHA-256 |
|---|---|---|---:|---|
| Student | `STU_QQQ_SAS.zip` | `CY08MSP_STU_QQQ.SAS7BDAT` | 4,056,940,544 | `04be0433153c8c4195e849023de501575c8a1073b9e4812434d61c69e3b4b160` |
| School | `SCH_QQQ_SAS.zip` | `CY08MSP_SCH_QQQ.SAS7BDAT` | 39,829,504 | `ae9e654480b6da5c1b8c20a701c8e0300fafafb8b77ac2d314b4e6a8a6530868` |

The same metadata are machine-readable in `data/pisa/source-metadata.csv`.
The derived local-slice dimensions and hash are machine-readable in
`data/pisa/slice-contract.csv`; both the importer and the verifier consume
that contract rather than maintaining separate literals.
After access, build the frozen local slice from those two files:

```sh
Rscript code/03_pisa/00_import_oecd_puf.R --student PATH/TO/CY08MSP_STU_QQQ.SAS7BDAT --school PATH/TO/CY08MSP_SCH_QQQ.SAS7BDAT
Rscript code/03_pisa/01_build_data.R
```

The importer first checks both raw-file hashes. It then writes
`data/pisa/local/source/pisa2022_read_usa_kor.rds`; the second command writes the
ignored local analytic view. The expected canonical slice is 11,006 rows by 99
columns with SHA-256
`178795eeba9367ebcfda69af8036e8bcb2e6bab6190b2c657af160e29a76ff44`.
A mismatch stops the import. If OECD has revised a file, retain the new raw
file outside the repository, record its hash, and do not bypass the check:
contact the package authors so the revision can be compared and a new package
version can be issued.

The public repository contains only aggregated and model-based evidence needed for the quick reproduction. OECD should be acknowledged as: “Programme for International Student Assessment (PISA), Organisation for Economic Co-operation and Development (OECD), Paris.” Terms reviewed 2026-07-11.
