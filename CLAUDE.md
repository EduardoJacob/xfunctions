# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**xfunctions** is an R package containing statistical analysis and data visualization functions. The package uses roxygen2 documentation style with `#'` comments and exports functions via a NAMESPACE file generated from roxygen2 tags.

### Architecture

- **Source files**: Located in `R/` directory, each file contains a single function (e.g., `XHistogram.R`, `XConfusionMatrix.R`)
- **Tests**: Located in `tests/testthat/` with testthat framework
- **Documentation**: Generated from roxygen2 comments; README.md is auto-generated from README.Rmd

### Key Functions by Category

**Statistical Tests:**
- `XStatsOneSampleTtest()` - One sample t-test for population mean inference
- `XStatsTwoSampleTtest()` - Two sample t-test
- `XStatsZtest()` - Z-test for population mean
- `XStatsChiSquareGOF()` - Chi-square goodness-of-fit test
- `XStatsChiSquareHomogeneity()` - Chi-square homogeneity test
- `XTtest()` - Paired t-test
- `XTwoSampleTtest()` - Two-sample t-test
- `XStatsCIproportion()` / `XStatsCIproportion2()` - Proportion confidence intervals

**Data Visualization:**
- `XHistogram()` - Histogram with superimposed normal distribution, density plot, boxplot stats
- `XHistogramDiscrete()` - Histogram for integer vectors with centered bars
- `XBarPlot()`, `XBarPlot2()`, `XBarPlot3()` - Bar plots (1-3 variables) with ggplot2
- `XScatterPlot()`, `XScatterPlot2()` - Scatter plots with correlation analysis and regression lines
- `XPlotBinomial()`, `XPlotGeometric()`, `XPlotPoisson()`, `XPlotNegativeBinomial()`, `XPlotChiSquare()`, `XPlotOjive()` - Distribution plots using plotly
- `XPlotMulti()` - Multiple functions on single graph with automatic legend
- `XThresholdPlot()` - Threshold visualization

**Statistical Summaries:**
- `XSummary()` - Enhanced summary with arithmetic/geometric/harmonic means, quartiles, outliers, skewness, kurtosis
- `XMode()` - Statistical mode (unimodal/multimodal)
- `XEntropy()` - Entropy computation for vectors
- `XDescribeRandomVariable()` - Expected value, variance, standard deviation for discrete random variables

**Classification/Machine Learning:**
- `XConfusionMatrix()` - Classification evaluation with accuracy, sensitivity, specificity, F1-score, visual confusion matrix

**Data Utilities:**
- `XLibrary()` - Automated package manager (installs missing packages, loads them, creates summary dataframes)
- `XExecSQL()` - Execute SQL queries via ODBC connection
- `XFillMissingNumbers()` - Data imputation using linear regression
- `XSplit()` - Split data/strings
- `XListEnvironments()` - List R environment chain
- `XShowInfo()`, `XShowObject()` - Display package/info summaries

**Mathematical Functions:**
- `XElectricForce()` - Electric force calculation using Coulomb's Law
- `XPowerSet()` - Generate power set of a set (all subsets)
- `XStringPermutations()` - String permutations
- `XRandomVariableContinuous()`, `XRandomVariableDiscrete()` - Random variable analysis
- `XJointCategorical()` / `XJointNumerical()` - Joint distribution analysis with covariance/correlation
- `XIsNormalyDistributed()` - Normality check using density plots, Q-Q plots

**Vector Geometry Helpers:**
- `norm()`, `unit()`, `dot()`, `angle()`, `cross()`, `rotate()` - Vector operations (defined in XFunctions.R)

### Dependencies

**Imports** (from DESCRIPTION):
- benchmarkme, caret, cvms, epiDisplay, ggplot2, gridExtra, gtools, kableExtra, magrittr, moments, numDeriv, plotly, RODBC, rootSolve, rstudioapi, scales, sessioninfo, stats, utils

**Suggests:**
- testthat (for testing)

### Development Workflow

**Build/Install:**
```r
# Install from local source
devtools::load_all()

# Or use pak
pak::pak("EduardoJacob/xfunctions")
```

**Run Tests:**
```bash
# Run all tests
Rscript -e "library(testthat); test_dir('tests/testthat')"

# Run single test file
Rscript -e "library(testthat); test_file('tests/testthat/test-XEntropy.R')"
```

**Build Documentation:**
```bash
roxygen2::roxygenise()
```

**Check Package:**
```bash
R CMD check
```

### Code Style Notes

- Functions use `@export` tags for roxygen2 export
- Each R file typically contains one function
- Functions include detailed docstrings with @description, @param, @return, @examples
- Uses `message()` for console output and `print()` for structured output
- Statistical functions often return invisible values while printing results
- Common pattern: calculate statistics → print results → plot visualizations

### Testing Pattern

Tests are in `tests/testthat/` with files named by function (e.g., `test-XEntropy.R`). Use testthat's standard structure with `test_that()` blocks.

### Important Files

- `R/` - Source files (one function per file)
- `NAMESPACE` - Generated exports (do not edit manually; use roxygen2)
- `DESCRIPTION` - Package metadata
- `README.Rmd` - Documentation source (generates README.md)
- `.Rbuildignore` - Files excluded from building
- `.gitignore` - Git ignore rules

### Common Development Tasks

**Add new function:**
1. Create file in `R/` with `<functionName>.R`
2. Add roxygen2 documentation with `#'` comments
3. Add `@export` tag
4. Run `roxygen2::roxygenise()` to update NAMESPACE
5. Add test in `tests/testthat/test-<functionName>.R`

**Update function:**
1. Edit the source file in `R/`
2. Run `roxygen2::roxygenise()` if documentation changed
3. Run tests to verify changes

### Environment Variables

Some functions require environment variables:
- `GEMINI_API_KEY` - For gemini() function via gemini.R package
- `OPENROUTER_API_KEY` - For openrouter() function via openrouter.R package
