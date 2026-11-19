# monolixSuiteTFL

Helper functions to assemble **T**ables, **F**igures, and **L**istings for
reporting with the Monolix and PKanalix applications of MonolixSuite.

The package reads a loaded Monolix or PKanalix project through
`lixoftConnectors` and turns the results into report ready tables, listings, and
plots. Tables and listings are returned as `flextable` objects, so they can be
dropped straight into Word or PowerPoint reports via `officer`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("simulations-plus/monolixSuiteTFL")
```

### MonolixSuite and lixoftConnectors

Every function reads the loaded project through `lixoftConnectors`, so a working
MonolixSuite installation is required at runtime. `lixoftConnectors` is declared
under `Suggests` rather than `Imports` because it is distributed with
MonolixSuite instead of a package repository, and so cannot be installed
automatically. Install it from your MonolixSuite installation, then initialize
the connection before calling any function:

```r
library(lixoftConnectors)
initializeLixoftConnectors(software = "monolix")   # or "pkanalix"
```

## Usage

Load a project, then build the piece you need:

```r
library(monolixSuiteTFL)
library(lixoftConnectors)

initializeLixoftConnectors(software = "monolix")
loadProject("path/to/project.mlxtran")

# Population parameter estimates with uncertainty
tablePopulationParameterEstimates()

# QQ plot of standardized random effects
plotQQStandardizedRandomEffects()
```

Every table and listing function returns a styled `flextable`. Use `styleTFL()`
to apply the same formatting to your own data frames.

## What's included

### Tables

| Function | Purpose |
| --- | --- |
| `tablePopulationParameterEstimates()` | Population parameter estimates with their uncertainties |
| `tableContinuousCovariateSummary()` | Summary statistics for continuous covariates |
| `tableCategoricalCovariateSummary()` | Summary counts for categorical covariates |
| `tableCovariateModelBuilding()` | Covariate model building results |
| `tableRunHistory()` | Run history across model iterations |
| `tableExclusions()` | Summary of excluded observations |
| `tableNCAConcentration()` | NCA concentration summary |
| `tableNCAParameters()` | NCA parameter summary |
| `tableNCARatios()` | NCA parameter ratios |
| `tableBEResults()` | Bioequivalence results |

### Listings

| Function | Purpose |
| --- | --- |
| `listingExclusions()` | Listing of excluded observations |
| `listingDosingRegimen()` | Dosing regimen listing |
| `listingAbbreviation()` | Abbreviations used in the report |
| `listingNCAConcentration()` | NCA concentration listing |
| `listingNCAParameters()` | NCA parameter listing |
| `listingNCARatios()` | NCA ratio listing |

### Figures

| Function | Purpose |
| --- | --- |
| `plotQQStandardizedRandomEffects()` | QQ plot of standardized random effects |
| `plotAbsIWRESScatterPlot()` | Scatter plot of absolute individual weighted residuals |

### Styling and helpers

| Function | Purpose |
| --- | --- |
| `styleTFL()` | Format and style a data frame as a `flextable` |
| `getDescriptiveStatsMetrics()` | Available descriptive statistics metrics for summary tables |

## Requirements

- R (>= 4.2.0)
- A MonolixSuite installation providing `lixoftConnectors` (required at runtime; see above)
- Imports: `ggplot2`, `dplyr`, `tidyr`, `rlang`, `purrr`, `stringr`, `tibble`, `tidyselect`
- Suggests: `lixoftConnectors`, `flextable`, `officer`, `haven`, `readxl`, `knitr`

## License

MIT License. See [LICENSE](LICENSE).
