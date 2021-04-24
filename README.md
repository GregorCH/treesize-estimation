# R Scripts for Branch-and-Bound Tree Size Estimation

This repository contains R-scripts to perform the data analysis
published in [Hendel, Anderson, Le Bodic, Pfetsch: Estimating Search Tree Size](https://opus4.kobv.de/opus4-zib/frontdoor/index/index/docId/7814).

## Overview

During Branch-and-Bound Search, a search tree is explored, whose size can be
exponential in the size (number of variables) of the input problem. It is a
difficult problem to estimate the time remaining until the search completes.

Even before the search is completed, information gathered at *leaves* (terminal
nodes) of the search tree may give valuable insights into the current progress
of the search. The scripts in this repository transform raw time series data of
actual MIP search trees into data frames that can be used to assess the
suitability of forecasting techniques on those time series.

## Structure

The repository is divided into two directories. Both directories contain helper scripts, mostly in R,
to transform and evaluate B&B Search tree data evaluated for the article.
Each directory contains a README.md with more specific information about the scripts and entry points.

* the directory `des_calibration` contains the scripts for the calibration of
  the estimates based on double exponential smoothing (Section 5 of the article).
  Please refer to the [README.md](des_calibration/README.md) within the directory for more information.
* the directory `training_evaluation` bundles the scripts for training ensemble methods from
  the atomic estimators, and a computational comparison based on SCIP runs.
  Please refer to the [README.md](training_evaluation/README.md) within the directory for more information.
