# SOMevidence 1.1.2 local validation

Validation date: 2026-08-11 (Asia/Shanghai)

This record covers the release-candidate source before public tagging. Remote
build identity and R-universe synchronization are verified separately after a
tag is created.

## Scientific and interface checks

- The complete `testthat` suite passed without unexpected warnings.
- Regression tests cover missing hard-partition evidence, representation-only
  gates, a single non-comparative partition, mismatched source partitions,
  missing source-partition provenance, repeated analysis sets, GUI missing
  predictors, split-specific preprocessing failures, and zero eligible
  cross-model splits.
- Exported API and object-contract tests passed.
- Two independent read-only reviews of the scientific guards and release
  checks reported no remaining release-blocking defect.

## Package checks

- Test coverage: 92.87%.
- `lintr` reported no findings in package code or release-check scripts.
- Roxygen documentation generation was idempotent.
- The pkgdown site built successfully from the installed candidate package.
- `R CMD check --as-cran --no-manual` completed with `Status: OK` on macOS and
  R 4.5.3. The local clock-network query was disabled for this run; the remote
  CI matrix performs its own runner-level check.
- The PDF reference manual was generated successfully and was non-empty; the
  release CI performs the authoritative manual check in a managed runner.
- The check-log verifier accepted a known complete historical check artifact
  and rejected a known incomplete artifact that had previously appeared green.

## Source-archive review

- Candidate archive: `SOMevidence_1.1.2.tar.gz`.
- SHA-256: `d228b49a521930799f1e9b1abb0e28d5e66829bc6fe2c1add42897a7eb5542f9`.
- The archive contained no local absolute paths, credentials, repository
  metadata, temporary files, unpublished coordinates, or unapproved author or
  contributor names.
- Package authorship remained limited to Shaowen Ye. AI-assisted code
  development remained disclosed only as provenance, not authorship or
  contributorship.

## Remote release gate

The version is not considered released by this record alone. The exact public
candidate must still pass the complete GitHub Actions matrix, PDF-manual job,
coverage, lint, pkgdown build, and the manually triggered remote CRAN incoming
preflight before the annotated release tag is created.
