# SOMevidence 1.2.0 local validation

Validation date: 2026-08-11 (Asia/Shanghai)

This record covers the release-candidate source before public tagging. It
separates object-contract assurance, scientific-output checks, package checks
and public-release boundary review. Remote build identity is verified after
release.

## Identity and object-contract checks

- All 1,884 test expectations across 26 test files passed with no failures,
  errors, warnings or skipped expectations.
- Historical version 1.0 and 1.1 exported interfaces remain prefix-compatible;
  version 1.2 exports, arguments and defaults match the frozen machine-readable
  API contracts exactly.
- Public result constructors carry the version 1.2 structural contract.
  Deterministic migrations were tested for idempotence, nested provenance,
  explicit `NULL` fields, sample identity and required recomputation paths.
- External labels were tested for identity-based alignment, subsets, explicit
  positional compatibility, duplicate and conflicting identifiers, consensus
  coverage and mutually exclusive omission accounting.
- Two independent read-only implementation reviews reported PASS after the
  identity, migration, topology and representation boundary cases were covered
  by regression tests and re-reviewed.

## Continuous-representation checks

- Quantization, topographic, occupancy and mapping-coverage diagnostics were
  tested for analysis and assessment scopes.
- Pairwise shortest-hop topology and optional tie-preserving neighbourhood
  overlap were tested across rectangular and hexagonal maps, including
  non-toroidal and even- and odd-row toroidal grids.
- Exact fit-pair and sample-pair computation budgets, fit and split identity,
  malformed grids, disconnected graphs, warning records and `fail_fast`
  behaviour were exercised explicitly.
- A negative control retained quantization behaviour while disrupting topology;
  the representation audit detected the topological change without producing a
  score, ranking or automatic model-selection decision.

## Package and documentation checks

- Test coverage: 92.47%.
- `lintr` reported no findings.
- Roxygen documentation regeneration was idempotent, and `git diff --check`
  reported no whitespace errors.
- Installation from the candidate archive succeeded. A clean installed-package
  smoke run produced four successful SOM fits, one requested representation
  comparison and an idempotent current-object upgrade.
- The complete pkgdown site, including all five executable articles and both
  new reference pages, built successfully from an installed candidate package.
- The PDF reference manual built successfully as a non-empty 30-page file.
- `R CMD check --as-cran --no-manual` completed on macOS and R 4.5.3 with
  `Status: OK`.

## Source-archive review

- Candidate archive: `SOMevidence_1.2.0.tar.gz`.
- SHA-256: `fc1e0657eb7da9be29e71a36ea23e75fcf506b71f6c7bc4dc80d770b7973a5b2`.
- Gzip integrity, archive listing, extraction and package metadata checks
  passed. The archive contained 133 entries and 122 regular files, with no
  duplicate, absolute or parent-traversal paths and no symbolic links or
  special nodes.
- An independent read-only archive and change-scope review also reported PASS;
  the candidate remained byte-identical throughout that review.
- The candidate contained no credentials, local absolute paths, repository
  metadata, temporary files, private study materials, unpublished coordinates
  or unapproved author and contributor names.
- Package authorship remained limited to Shaowen Ye, with maintainer email
  `yeshaowen2119@gmail.com`. AI-assisted development remained a provenance
  disclosure, not authorship or contributorship.

## Remote release gate

This local record does not by itself constitute a release. The exact public
candidate must pass the GitHub Actions check matrix, PDF-manual job, coverage,
lint, pkgdown build and remote CRAN incoming preflight before an annotated
stable release tag is created. GitHub Pages and R-universe source identity are
then verified against that tag.
