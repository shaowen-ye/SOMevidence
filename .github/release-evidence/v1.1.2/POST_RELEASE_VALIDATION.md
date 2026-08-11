# SOMevidence 1.1.2 post-release validation

Validation date: 2026-08-11 (Asia/Shanghai)

This record links the public release, repository source and R-universe build to
the same immutable source revision. It supplements the pre-release local
validation record.

## Public source identity

- GitHub release: <https://github.com/shaowen-ye/SOMevidence/releases/tag/v1.1.2>
- Release state: stable, published and not marked as a prerelease.
- Annotated tag: `v1.1.2`.
- Source commit: `cc8f833f55aab373c9d7d6d3f231a01a268a2be8`.
- Annotated tag object: `344a0e15a94fc824d1458fc56f7f3c8d91418671`.

## GitHub release checks

The complete seven-job package-check matrix, lint, coverage and pkgdown
workflows passed for the release source. Test coverage was 92.87%. The
manually triggered remote CRAN preflight also passed; its sole note identified
the package as a new submission.

## R-universe synchronization

- Package page: <https://shaowen-ye.r-universe.dev/SOMevidence>
- Published package version: `1.1.2`.
- `RemoteRef`: `v1.1.2`.
- `RemoteSha`: `cc8f833f55aab373c9d7d6d3f231a01a268a2be8`.
- Build status: `success`.
- Publication time: `2026-08-11T10:39:32.368Z`.
- Build record: <https://github.com/r-universe/shaowen-ye/actions/runs/31482597971>.

The R-universe source build and eight reported platform binaries completed
successfully. The source revision therefore matches the GitHub release and the
repository tag exactly.

