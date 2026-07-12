# Release-readiness record

## Local acceptance

The public-boundary candidate passed the following checks on the frozen arm64
macOS environment with R 4.6.0:

- quick exhibit rebuild and the complete public verification suite;
- a clean-room copy containing no local PISA unit records, audit logs, Quarto
  cache, compiled model, or runtime shard;
- deterministic regeneration of tables and PDF exhibits;
- exact manifest set, byte-size, and SHA-256 checks;
- negative controls for a changed listed file, an unlisted public file,
  quoted and unquoted credentials in text/YAML, credential content in an
  allowlisted binary and serialized-object fields, values, and attributes,
  an unsupported serialized environment, an unreadable serialized object,
  non-project authoring-attribution text, corrupt simulation content,
  duplicate/missing simulation keys, and a non-reportable Korea Pipeline B
  result;
- Quarto rendering of eight guide pages and all 13 local links;
- public text, binary, and serialized-object scans for credentials, private
  keys, author-machine paths, control characters, and embedded local paths;
- full simulation and PISA preflights, enumerating 4,000 simulation tasks and
  22 PISA fits without starting the long computations.

The candidate public boundary contains fewer than 10 MB and no file approaches
GitHub's per-file limit. The unit-record PISA inputs, audit log, Quarto cache,
compiled models, and long-run scratch outputs are excluded by both ignore rules
and the canonical public-file enumerator.

## Independent review disposition

The final manuscript/evidence review confirmed exact parity for Tables 3--5
and Figures 1--4, and substantive/numeric parity for Figure S3. It prompted two
corrections before acceptance: Tables 1 and 2 now preserve the manuscript's
actual row structure and wording rather than a substitute summary, and the
main claim registry now checks the maximum MCMC--archived-frequency
differences for both slopes.

The execution/security review prompted fail-closed manifest enumeration,
simulation shard run signatures and content hashes, a real configuration-file
parser, binary credential scanning, and automatic full-replay comparison with
the frozen simulation archive. The novice review prompted an explicit `renv`
bootstrap, a pinned R setup for both CI workflows, and exact PISA source-file
metadata.

## Deferred external checks

No x86_64 container or virtual-machine runtime is installed on the release
host, and repository publication is outside the present scope. Consequently,
the Linux/x86_64 workflow has been authored but cannot run until the repository
is published. The 4,000-replication simulation and 22-fit PISA computations
were preflighted, not repeated during packaging; the package instead ships the
complete audited evidence and explicit opt-in replay protocols. These are
declared execution limits, not silent passes.

PISA unit records are deliberately not redistributed. The current OECD PISA
PUF terms prohibit making them available to a third party. Exact SAS archive
names, extracted filenames, sizes, and hashes are recorded so an authorized
reader can reconstruct the ignored local slice and detect an upstream revision.

## Source discrepancies retained

The package continues to expose, rather than overwrite, six frozen-source
issues. Four concern manuscript claims or labels: classic Rubin degrees of
freedom in the Pipeline A archive, ML implementation behind the simulation's
“REML” label, the exact maximum R-hat of 1.02023844442964 behind the displayed
1.020, and the absence of an archived producer for the Korea variance-share
sentence. Two OSM archive-description issues concern the separate DGM/MCMC
seed domains and the historical four-versus-six reduced-footprint count. See
`docs/known-source-discrepancies.md`.
