# Launch the optional reproducible SOM interface

The Shiny interface exposes a compact subset of the package workflow for
teaching and exploratory configuration. Every completed run can export
its exact R script and a YAML configuration snapshot. The GUI cannot
currently import that snapshot to restore its controls. The exported
script, not the GUI session or snapshot, is the executable analysis
record. The interface is designed for a local R session, and
`SOMevidence` sends no telemetry. In a local session, selected files
remain on the local computer. A remotely deployed Shiny application
transfers selected files to its host, whose operator is responsible for
access controls and data handling.

## Usage

``` r
launch_som_app()
```

## Value

A `shiny.appobj`. Pass it to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) or print
it in an interactive R session. The application does not return an
analysis result; users can export the executable R script for a
configured run.

## Lifecycle

Experimental. The interface and its exported configuration snapshot
schema may change after independent usability testing. The ordinary R
API remains the authoritative analysis interface.
