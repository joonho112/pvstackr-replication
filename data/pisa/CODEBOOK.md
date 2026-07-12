# Local PISA data contract

The non-redistributed local canonical RDS is a data frame with 11,006 rows and 99 columns. Rows retain their original order in the source extract. Its dimensions and hash are fixed in `slice-contract.csv`. Build it only after obtaining the PUF directly from OECD and accepting the PISA-specific terms.

| Variables | Role |
|---|---|
| `CNT`, `CNTSCHID`, `CNTSTUID` | Country, school, and student identifiers |
| `ESCS` | Index of economic, social, and cultural status; the only complete-case exclusion variable |
| `ST004D01T`, `IMMIG`, `HISCED` | Background variables retained in the canonical slice but unused in EST-01 |
| `W_FSTUWT` | Final student weight |
| `W_FSTURWT1`–`W_FSTURWT80` | BRR-Fay replicate weights used only for the external design-based target |
| `PV1READ`–`PV10READ` | Reading plausible values, in the fixed analysis order |
| `W_SCHGRNRABWT` | School weight used to factor the final student weight into school and conditional-student stages |

The derived RDS adds `row_index_source`, `school_id_model`, `w_school`, `w_student_cond`, `w_norm_sample`, `ESCS_school_mean`, `ESCS_within`, and `ESCS_between`. For each country, `w_norm_sample` has mean one. Unique school weights have mean one, and `w_school * w_student_cond` equals `w_norm_sample` up to floating-point precision. School means of ESCS use `w_norm_sample`.

The released-to-analytic cascade is USA 4,552 students in 154 schools to 4,342 students in 150 schools, and Korea 6,454 students in 186 schools to 6,391 students in 186 schools. Exclusions are records with missing `ESCS`.
