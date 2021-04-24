# R Scripts for Branch-and-Bound Tree Size Estimation

This directory contains R scripts to transform time-series data observed during MIP branch-and-bound search into treesize estimations.

## Structure

The repository is divided into a couple of R scripts. The functionality to read in and transform the raw time-series data is provided by "functions.R", which all other scripts source.
The R Markdown file "ForecastingTheFutureProgress.Rmd" can be used to generate an html report that contains plots and error tables for every instance in the data set individually.

Some sample data is contained in the "Data/" subdirectory. The data has been generated via modified scripts of [this GitHub Repository](https://github.com/pierre-lebodic/bnb-mip-estimates), which has been used for a previous paper on this topic.
The directory structure of the "Data/" subdirectory is important: It separates the individual data files into time series based on uniform leaf weights, and nonuniform (phi) leaf weights.


## Usage

### Rendering an HTML Report

The file "ForecastingTheFutureProgress.Rmd" is a template to generate a data report. Open it in RStudio, and click on "Knit With Parameters". Tick the box "full" to generate a full report for the Data/ subdirectory contained in this repository, otherwise, only one time series is read in and depicted.
The argument "path" can be used to specify a different root directory of Data.

To generate the full report more quickly outside of RStudio, you can modify the script
"render_full_report.R" to suit your needs.

### Computing Forecast Errors

Simply execute the script

```
./do_all.sh
```

It may be necessary to edit the file "save_forecast_errors.R" first to specify a different Data root directory.
As a byproduct, CSV tables "uniform_errors.csv" and "nonuniform_errors.csv"
are produced in the root directory.
They can be used to directly call `./makeplots.R` afterwards.

### Computing Window Size Errors

Simply execute the script

```
./do_all_windowsize.sh
```

It may be necessary to edit the file "save_windowsize_errors.R" to specify a different Data root directory.


## Installation

The R scripts are ready to execute. It is necessary to install some packages from [CRAN](https://cran.r-project.org/) first. The particular packages needed are all listed by "functions.R":

```
require(magrittr)
require(ggplot2)
require(dplyr)
require(forecast)
```

In order to install those. simply execute

```
install.packages(c("magrittr", "dplyr", "forecast", "ggplot2")) from within an R or RStudio session.
```

In order to render the HTML report, you also need to install the package "rmarkdown".


## Meaning of the CSV columns

The error tables contain a lot of data. Here is the explanation of the column names:

- **X1**: Index within a single problem/method combination
- **Method**: One of "des-paper", "ets", or "w-linear"
- **rmsd**: "Root mean square deviation between the progress forecast and the actual curve
- **mae{Prog,Freq}**: Mean Absolute Error of the forecast curves for progress and leaf frequency
- **mapeRes**: "Mean Average Precision Error" of the resource forecast
- **Estim{Prog,Freq,Res}**: Tree Size Estimation of the corresponding method
- **Error{Prog,Freq,Res}**: Error measure log2(Estim/Actual)
- **Actual**: The actual tree size at termination
- **Current**: The current number of visited nodes (when the estimation was made)
- **Level**: The amount of cumulative progress when this observation has been made
- **Prob**: MIP Instance identifier
