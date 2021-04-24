#! /usr/bin/Rscript

source("./functions.R")

if(! dir.exists("windowsizes") ) {
  print("Creating 'windowsizes' directory")
  dir.create("windowsizes")
}



# make 75 forecasts ranging from 1% to 75 % of the cumulative progress.
rellevels <- seq(0.01, 0.75, by = 0.01)

# summarize each forecast
datafiles <- getDataFilesInPath("../MMMc_v3")
testwindowsizes <- c(c(2,4), seq(3,100, by = 2))

library(foreach)
library(doMC)
registerDoMC(4)
foreach(d=(datafiles %>% grep("uniform", ., value = T))) %dopar% {
  print(progressPlotTitle(d))
  for (t in testwindowsizes) {
    df <- probsFile2dataFrame(d)
    if(nrow(df) < 10)
      next
    problemspecificrellevels <- getProblemSpecificRellevels(df, rellevels)
    lapply(problemspecificrellevels, function(x) summarize_one_forecast(df, x, "w-linear", windowsize = t)) %>%
      dplyr::bind_rows() %>%
      mutate(Level=problemspecificrellevels,
             Windowsize=t
             ) %>%
      as.data.frame() %>%
      write.csv("windowsizes/%s_%d.csv" %>%
                  sprintf(progressPlotTitle(d), t)
                )
  }
}

