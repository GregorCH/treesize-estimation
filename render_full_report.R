#! /usr/bin/Rscript

#
# This script generates a full report out of the R Markdown template. Therefore, it requires an external path to a Data-subdirectory.
# The resulting html report will be created in a directory "full_report/"
#
rmarkdown::render("ForecastingTheFutureProgress.Rmd",
                  params=list(full=TRUE,
                              path="../MMMc_v3"
                              ),
                  output_dir = "full_report/")
