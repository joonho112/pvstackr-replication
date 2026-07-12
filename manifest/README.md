# Public artifact manifests

Manifests connect a public logical source ID to a repository-relative file and
its producer. They deliberately omit author-machine paths, cloud identifiers,
credentials, and internal review history.

## Artifact manifest schema

`manifest/artifacts.csv` uses the columns defined in
`manifest/schemas/artifact-fields.csv`.

Each public file has one `logical_id`. A generated file must identify its
`producer` and one or more `source_ids`. `sha256` is calculated over the bytes
distributed in this repository, not over a private precursor. For cleaned RDS
objects, the precursor hash belongs in the curation ledger and the clean public
object receives its own hash.

`path` is always repository-relative, uses `/`, and cannot contain `..`.
`rows` and `columns` are required for rectangular CSV/RDS data and blank for
non-tabular artifacts. `status` must be one of the values in
`manifest/schemas/status-values.csv`.

## RDS object schema

Every public RDS object must have a companion record containing:

- logical ID, schema version, producer, and source IDs;
- top-level class and names;
- expected dimensions or key cardinalities;
- byte size and SHA-256;
- a statement that recursive character-field scanning found no absolute path,
  private identifier, credential, or internal tool trace.

RDS objects are conveniences, not opaque evidence. Important rectangular
content also has a CSV mirror or a documented inspection command.

## Status semantics

`computed` describes whether a quantity exists. `reportable` describes whether
the paper-facing gate allows it to appear. These are independent. Code must not
turn `computed_but_withheld` into a numeric paper result, and missing rows must
not be silently converted to zero or `NA` without an explicit status.

## Curation ledger

`manifest/curation-ledger.csv` records how internal source material was handled:
`include`, `adapt`, `reference`, or `exclude`. It is provenance documentation;
it is not permission to copy unlisted files. The public release is built from an
explicit allowlist.

