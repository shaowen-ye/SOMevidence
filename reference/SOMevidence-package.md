# SOMevidence: evidence-oriented auditing of self-organizing maps

`SOMevidence` wraps the training implementation in
[`kohonen::supersom()`](https://rdrr.io/pkg/kohonen/man/supersom.html)
with explicit data-design, preprocessing, resampling, ensemble and
evidence-auditing interfaces. Its central distinction is between
continuous-representation diagnostics and a defensible hard partition.

## API contract

The package ships machine-readable contracts for exported functions and
returned objects in `inst/extdata`. The Shiny interface, sensitivity
scenario list and simulation catalogue are marked `experimental`; all
other exported interfaces form the stable 1.0.0 API.

The public API is deliberately evidence-oriented. It keeps
representation, partition, cross-model, external-label and transfer
evidence separate and permits an explicit insufficient-evidence outcome.
The package does not claim to validate ecological truth or choose a
universally optimal number of clusters.

## See also

Useful links:

- <https://shaowen-ye.github.io/SOMevidence/>

- <https://shaowen-ye.r-universe.dev/SOMevidence>

- <https://github.com/shaowen-ye/SOMevidence>

- Report bugs at <https://github.com/shaowen-ye/SOMevidence/issues>

## Author

**Maintainer**: Shaowen Ye <yeshaowen2119@gmail.com>

Authors:

- Shaowen Ye <yeshaowen2119@gmail.com>
