# Package index

## Data, preprocessing and design

- [`som_data()`](https://shaowen-ye.github.io/SOMevidence/reference/som_data.md)
  : Construct a design-explicit SOM data object
- [`som_preprocess()`](https://shaowen-ye.github.io/SOMevidence/reference/som_preprocess.md)
  : Define leakage-safe SOM preprocessing
- [`som_resamples()`](https://shaowen-ye.github.io/SOMevidence/reference/som_resamples.md)
  : Construct design-aware resamples
- [`som_spec()`](https://shaowen-ye.github.io/SOMevidence/reference/som_spec.md)
  : Specify a reproducible SOM ensemble
- [`expand_som_spec()`](https://shaowen-ye.github.io/SOMevidence/reference/expand_som_spec.md)
  : Expand an ensemble specification into its model budget

## Ensembles and representation

- [`fit_som_ensemble()`](https://shaowen-ye.github.io/SOMevidence/reference/fit_som_ensemble.md)
  : Fit a reproducible ensemble of self-organizing maps
- [`audit_som()`](https://shaowen-ye.github.io/SOMevidence/reference/audit_som.md)
  : Audit representation quality across a SOM ensemble
- [`plot_som_plane()`](https://shaowen-ye.github.io/SOMevidence/reference/plot_som_plane.md)
  : Plot an interpretable plane from one SOM ensemble member
- [`pareto_candidates()`](https://shaowen-ye.github.io/SOMevidence/reference/pareto_candidates.md)
  : Identify Pareto-efficient SOM candidates

## Partitions and consensus

- [`partition_som()`](https://shaowen-ye.github.io/SOMevidence/reference/partition_som.md)
  : Form and compare candidate hard partitions of SOM units
- [`consensus_som()`](https://shaowen-ye.github.io/SOMevidence/reference/consensus_som.md)
  : Construct a sample-level consensus partition
- [`som_gate()`](https://shaowen-ye.github.io/SOMevidence/reference/som_gate.md)
  : Define explicit evidence gates for a hard SOM partition
- [`assess_defensibility()`](https://shaowen-ye.github.io/SOMevidence/reference/assess_defensibility.md)
  : Assess partition defensibility without forcing a verdict

## Triangulation and transfer

- [`fit_cross_models()`](https://shaowen-ye.github.io/SOMevidence/reference/fit_cross_models.md)
  : Fit controlled cross-model reference partitions
- [`compare_cross_models()`](https://shaowen-ye.github.io/SOMevidence/reference/compare_cross_models.md)
  : Compare SOM partitions with controlled cross-model references
- [`audit_transfer()`](https://shaowen-ye.github.io/SOMevidence/reference/audit_transfer.md)
  : Audit transfer to held-out sampling domains
- [`map_som_ensemble()`](https://shaowen-ye.github.io/SOMevidence/reference/map_som_ensemble.md)
  : Map new observations through a fitted SOM ensemble
- [`evaluate_external_labels()`](https://shaowen-ye.github.io/SOMevidence/reference/evaluate_external_labels.md)
  : Compare a consensus partition with external ecological labels

## Diagnostic plot methods

- [`plot(`*`<som_audit>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_audit.md)
  : Plot SOM audit trade-offs
- [`plot(`*`<som_partitions>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_partitions.md)
  : Plot partition reproducibility across candidate class counts
- [`plot(`*`<som_consensus>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_consensus.md)
  : Plot sample-level consensus evidence
- [`plot(`*`<som_cross_comparison>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_cross_comparison.md)
  : Plot cross-model agreement without ranking methods
- [`plot(`*`<som_transfer_audit>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_transfer_audit.md)
  : Plot mapping shift in held-out domains
- [`plot(`*`<som_newdata_mapping>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_newdata_mapping.md)
  : Plot new-data mapping shift across an ensemble
- [`plot(`*`<som_sensitivity>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/plot.som_sensitivity.md)
  : Plot sample-level sensitivity across prespecified scenarios

## Complete workflows

- [`run_som_workflow()`](https://shaowen-ye.github.io/SOMevidence/reference/run_som_workflow.md)
  : Run a complete SOM defensibility workflow
- [`summary(`*`<som_workflow>`*`)`](https://shaowen-ye.github.io/SOMevidence/reference/summary.som_workflow.md)
  : Summarize workflow quality assurance and model outcomes
- [`run_som_sensitivity()`](https://shaowen-ye.github.io/SOMevidence/reference/run_som_sensitivity.md)
  : Run prespecified SOM sensitivity scenarios

## Teaching, examples and interface

- [`simulate_som_scenario()`](https://shaowen-ye.github.io/SOMevidence/reference/simulate_som_scenario.md)
  : Simulate known-structure data for SOM validation
- [`open_data_registry()`](https://shaowen-ye.github.io/SOMevidence/reference/open_data_registry.md)
  : List governed tutorial and candidate open-data sources
- [`launch_som_app()`](https://shaowen-ye.github.io/SOMevidence/reference/launch_som_app.md)
  : Launch the optional reproducible SOM interface
