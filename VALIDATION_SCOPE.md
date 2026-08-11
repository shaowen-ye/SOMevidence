# Validation scope

`SOMevidence` is an evidence-auditing workflow. Its outputs describe
software behaviour and reproducibility under a stated data set, sampling
design, preprocessing specification, ensemble budget, candidate `k`, and
set of analyst-specified requirements. They are not certificates of
ecological truth.

## Evidence layers

| Layer | What is currently checked | Boundary |
|----|----|----|
| Software verification | Unit, integration and regression tests; API and object contracts; package checks across supported R and operating-system configurations | Reduces implementation and interface errors; does not validate a scientific design or interpretation |
| Design-conditioned reproducibility | Variation across prespecified resamples, grids and random starts, with preprocessing learned within each analysis split | Describes the stated ensemble and sampling unit; does not generalize to unexamined designs or budgets |
| Continuous-representation reproducibility | Exact cross-fit comparison of shortest-hop sample-pair distances and optional tie-preserving local neighbourhoods among jointly mapped observations | Describes topology concordance for prespecified fit pairs and scopes; fit pairs and sample pairs are dependent descriptive units and do not identify a best map or prove discreteness |
| Hard-partition evidence | Pairwise agreement, consensus coverage, clusterwise stability, assignment support and cross-model agreement for candidate `k` values | Agreement among partitions is not classification accuracy and does not prove that discrete types exist |
| Held-domain evidence | Mapping distance, unit occupancy and coverage for observations excluded from fitting | Diagnoses mapping behaviour under the stated held-domain design; does not by itself establish ecological transferability or its cause |
| External-label assessment | Post hoc agreement with labels excluded from SOM training | Describes concordance with those labels among evaluable samples; labels are not automatically ground truth |
| Conditional decision | Whether available evidence meets an analyst-specified [`som_gate()`](https://shaowen-ye.github.io/SOMevidence/reference/som_gate.md) | A `supported` result is conditional on the stated requirements; thresholds are not package-wide scientific standards |

## Meaning of a supported decision

[`assess_defensibility()`](https://shaowen-ye.github.io/SOMevidence/reference/assess_defensibility.md)
returns `supported` only when every active gate and the required
structural-completeness checks pass, the evidence objects trace to the
same candidate partitions, comparative partition evidence is evaluable,
and at least one partition-quality requirement was prespecified. The
status means that a candidate partition met the analyst’s prespecified
evidence requirements for that workflow. It does not demonstrate that
discrete ecological types exist, choose an optimal `k`, name the
resulting groups, identify a causal mechanism, or establish general
transferability.

Neither the within-fit output from
[`audit_som()`](https://shaowen-ye.github.io/SOMevidence/reference/audit_som.md)
nor the experimental cross-fit output from
[`audit_som_representation()`](https://shaowen-ye.github.io/SOMevidence/reference/audit_som_representation.md)
is candidate-partition evidence. The latter is deliberately not accepted
by
[`assess_defensibility()`](https://shaowen-ye.github.io/SOMevidence/reference/assess_defensibility.md).
If no partition, consensus or cross-model partition evidence is
supplied, comparative evidence cannot be evaluated, or the gate contains
only representation-level requirements, the required evidence is marked
unavailable and the decision is `uncertain`.

The package supplies no universal gate thresholds. Requirements should
be defined before inspecting the focal result and justified from the
study design, decision context, simulation evidence or an independently
governed protocol. Changing the data, sampling unit, preprocessing,
candidate values, ensemble budget or transfer domain changes the scope
of the decision.

## Continuous gradients are an important counterexample

A continuous ecological gradient can be divided into a fixed number of
segments that remain similar across random starts, resamples or
alternative clustering algorithms. High partition stability or
cross-model agreement can therefore occur even when the data-generating
structure contains no discrete classes.

`SOMevidence` keeps representation, partition, transfer and
external-label evidence separate so that this limitation can be
reported. The current package does not provide a calibrated test that
proves discreteness or distinguishes all continuous structures from
genuine ecological types. Analysts should retain a continuous
interpretation whenever the evidence does not independently justify a
hard partition.

The experimental representation audit asks a narrower question: whether
two fitted maps preserve similar shortest-hop relationships among the
same mapped observations. It uses exact calculations under explicit
fit-pair and sample-pair budgets and may report unevaluable comparisons
when overlap is too small or distances are constant. High concordance
does not establish that map orientation, node identities or hard classes
are equivalent.

## Current real-data and open-data status

The Palmer penguin vignette is a worked example of the complete
interface. Species labels are excluded from training and inspected only
after consensus. The example demonstrates workflow use; it is not
external validation of the package or evidence that morphology-derived
SOM classes define species.

[`open_data_registry()`](https://shaowen-ye.github.io/SOMevidence/reference/open_data_registry.md)
distinguishes implemented examples from governed candidate sources. A
candidate DOI, licence record or planned role is not a completed
benchmark. External validation additionally requires a frozen source
version, file-level integrity checks, an auditable transformation and
schema, an analysis protocol, results, and interpretation at the correct
independent sampling unit.

## Planned method evaluation

A future simulation and benchmark study should be specified before
results are examined and reported using an ADEMP structure:

- **Aims:** evaluate when the workflow supports, abstains or remains
  uncertain under scientifically distinct structures.
- **Data-generating mechanisms:** include discrete mixtures with varying
  separation, continuous and nonlinear gradients, no-structure controls,
  unequal classes, grouped dependence, missingness, multilayer conflict
  and independently varied domain and covariate shifts.
- **Estimands:** define the target state and scope for each scenario,
  including whether a discrete partition is present and which sampling
  unit is independent.
- **Methods:** compare complete, prespecified workflows rather than
  treating K-means, Ward.D2 or Gaussian mixtures only as informal
  competitors.
- **Performance measures:** report correct-support, false-support,
  abstention and uncertainty rates with Monte Carlo uncertainty,
  alongside coverage, failures, runtime and memory use.

External examples should span more than one ecological system and define
held-out units from the sampling design, such as sites, campaigns, data
sets, regions or phylogenetic clades. Until those studies and their
reproducible artifacts are complete, software checks, simulations used
for teaching, and real-data demonstrations remain separate evidence
categories.

## Reproducible reporting

Archive the package version, input provenance, stable sample
identifiers, preprocessing and resampling specifications, model budget,
all random seeds, candidate `k` values, gate settings, warnings,
failures, result objects and
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html). A
configuration snapshot records intended settings but does not replace
the input data, executed results or software environment.
