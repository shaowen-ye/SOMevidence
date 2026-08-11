# Launch the optional reproducible SOM interface

The Shiny interface exposes a compact subset of the package workflow for
teaching and exploratory configuration. Every completed run can export
its exact R script and a YAML configuration. The exported script, not
the GUI session, is the reproducible analysis record.

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

Experimental. The interface and its exported configuration schema may
change after independent usability testing. The ordinary R API remains
the authoritative analysis interface.
