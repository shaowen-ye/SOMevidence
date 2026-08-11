# SOMevidence 1.1.3 local performance evidence

Benchmark date: 2026-08-11 (Asia/Shanghai)

The deterministic benchmark script compares selected version 1.1.2 reference
kernels with the computation-equivalent version 1.1.3 implementations. It
requires identical outputs before recording any timing. The measurements are
directional local evidence, not cross-platform performance guarantees or CI
acceptance thresholds.

## Environment

- R 4.5.3
- Platform: `aarch64-apple-darwin20`
- Three timing repetitions per implementation; the median elapsed time is
  reported.

## Results

| Operation | v1.1.2 | v1.1.3 | Before/after | Unit |
|---|---:|---:|---:|---|
| Ensemble task metadata | 199.411 | 0.191 | 1045.24x | MiB |
| Analysis-row distance matrix | 0.064 | 0.007 | 9.14x | seconds |
| Complete-data co-assignment | 1.665 | 0.093 | 17.90x | seconds |
| Aligned-vote propagation | 0.546 | 0.152 | 3.59x | seconds |
| Ward.D2 tree across `k = 2:8` | 0.091 | 0.016 | 5.69x | seconds |

Elapsed times depend on hardware, operating system, R version and background
load. The scientifically relevant release gate is output equivalence; the
timings only show the direction and practical scale of the local improvement.

## Reproduction

From the repository root, with development dependencies available, run:

```sh
Rscript .github/scripts/benchmark-v113.R
```

The script can also run against an installed SOMevidence 1.1.3 package when
`pkgload` is unavailable. It uses fixed random seeds and prints its runtime
environment with the result table.

