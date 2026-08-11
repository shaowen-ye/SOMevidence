# SOMevidence 1.2.0 post-release validation

Validation date: 2026-08-11 (Asia/Shanghai)

This record links the public release, its source archive, documentation and
R-universe distribution to the exact source revision that passed the release
checks. It complements, rather than changes, the pre-release evidence in
`LOCAL_VALIDATION.md`.

## Release identity

- Repository: <https://github.com/shaowen-ye/SOMevidence> (PUBLIC).
- Pull request: <https://github.com/shaowen-ye/SOMevidence/pull/7>.
- Merge commit: `dfc9330d33689c020bc6e195b06d7d8632389c23`.
- Annotated tag: `v1.2.0`; tag object
  `04c7e12e115a04a357beb6e5ecf49e4af9a24a4e`, dereferencing to the merge
  commit above.
- Stable release: <https://github.com/shaowen-ye/SOMevidence/releases/tag/v1.2.0>.
- Release publication time: 2026-08-11 14:24:01 UTC.

## Release archive

- Asset: `SOMevidence_1.2.0.tar.gz` (668,236 bytes).
- SHA-256:
  `fc1e0657eb7da9be29e71a36ea23e75fcf506b71f6c7bc4dc80d770b7973a5b2`.
- The digest reported by the GitHub Release asset matches the independently
  audited local candidate exactly.
- A fresh download from the stable release passed gzip integrity checking and
  reproduced the same SHA-256 digest.

## GitHub checks and documentation

- All pull-request checks completed successfully: six R platform/version
  combinations, the PDF reference manual, coverage, lint and pkgdown.
- The separately dispatched CRAN incoming preflight completed successfully:
  <https://github.com/shaowen-ye/SOMevidence/actions/runs/31499474951>.
- The post-merge R matrix completed all seven applicable jobs successfully:
  <https://github.com/shaowen-ye/SOMevidence/actions/runs/31500215641>.
- The pkgdown deployment was built from the exact release commit:
  <https://github.com/shaowen-ye/SOMevidence/actions/runs/31500215696>.
- The resulting `gh-pages` revision is
  `45e68dbadfa08f60831688ecba2cbedaff260a39`.
- The public site at <https://shaowen-ye.github.io/SOMevidence/> returns HTTP
  200 and identifies the released package as version 1.2.0. Both new reference
  pages, `audit_som_representation()` and `upgrade_som_object()`, return HTTP
  200.

## R-universe distribution

- Registry source selection resolved `v1.2.0` to
  `dfc9330d33689c020bc6e195b06d7d8632389c23`.
- Multi-platform build:
  <https://github.com/r-universe/shaowen-ye/actions/runs/31506452729>.
- Source, Linux release/devel, macOS oldrel/release, Windows
  oldrel/release/devel and WebAssembly checks completed with `OK` results.
- The public API reports version `1.2.0`, `RemoteRef = v1.2.0`, the exact
  release commit, and `status = success`. All nine applicable check records
  point to the build above.
- The R-universe source package is 1,229,888 bytes with SHA-256
  `7b2fc63e01bb2f9173efd99b6e67c24babc01171dd76651bcb15ef37fbe80ace`.
  The source package, eight published platform binaries, package page, PDF and
  HTML manuals, five articles and both new help topics return HTTP 200.
- Installation from R-universe into a new temporary R library selected version
  1.2.0. An installed-package smoke run produced two successful SOM fits, two
  fit-level representation records, one pairwise representation record and an
  idempotent object-contract upgrade. Both new functions were exported.

## Public-release boundary

- The repository remained PUBLIC after publication.
- The released package has one author and maintainer, Shaowen Ye, using
  `yeshaowen2119@gmail.com`.
- The release archive contains no credentials, local absolute paths, private
  study material, unpublished coordinates, manuscript files or additional
  author and contributor identities.
- AI-assisted development is disclosed only as code provenance and is not
  represented as authorship or contributorship.
