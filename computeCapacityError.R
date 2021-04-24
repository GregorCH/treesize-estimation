#! /usr/bin/Rscript

#
# computes the average absolute error for different storage capacities
#
source("./functions.R")

# make 95 forecasts ranging from 1% to 95 % of the cumulative progress.
rellevels <- seq(0.01, 0.95, by = 0.01)

# get the data files list
datafiles <- getDataFilesInPath("../MMMc_v3") %>% grep("uniform", ., value = T)

# create a list of potential capacities
capacities <- c(2 ** (3:15),3 * 1e7)
capacities <- c(3 * 1e7)


errorsAllParams <- list()
for( c in capacities ) {
  print(sprintf("Computing results for capacity %d", c))
  # collect errors for all files
  errorsCapacityList <- list()
  # iterate through all problems and problemspecific rellevels
  for( d in datafiles ) {
    df <- probsFile2dataFrame(d, maxsize = c)

    print(progressPlotTitle(d))

    if(nrow(df) < 10)
      next

    problemspecificrellevels <- getProblemSpecificRellevels(df, rellevels)

    if( length(problemspecificrellevels) == 0 )
      next

    # use ETS as a self-adjusting, neutral estimation method
    errorsProblem <- lapply(problemspecificrellevels, function(x) summarize_one_forecast_linear(df, x, "ets")) %>%
      dplyr::bind_rows() %>%
      mutate(Level=problemspecificrellevels) %>%
      as.data.frame()

    errorsCapacityList[[length(errorsCapacityList) + 1]] <- errorsProblem
  }


  # concatenate the errors at this level of capacity/resolution and compute the median errors and mean absolute errors (MAPE) of the forecasts
  completedf <- errorsCapacityList %>% dplyr::bind_rows()

  summarizeddf <- completedf %>%  dplyr::summarize(
    medErrProg=median(ErrorProg),
    medErrFreq=median(ErrorFreq),
    medErrUns=median(ErrorUns),
    medErrSsg=median(ErrorSsg),
    meanAbsErrProg=mean(abs(ErrorProg)),
    meanAbsErrFreq=mean(abs(ErrorFreq)),
    meanAbsErrUns=mean(abs(ErrorUns)),
    meanAbsErrSsg=mean(abs(ErrorSsg)),
    n=n()
  ) %>% dplyr::mutate(capacity=c)

  errorsAllParams[[length(errorsAllParams) + 1]] <- summarizeddf
}


# concatenate the individual error measurements at all levels of resolution and save them to a csv
errorsAllParamsdf <- errorsAllParams %>% dplyr::bind_rows() %>% as.data.frame()
errorsAllParamsdf %>% write.csv(file="errors_capacity_noresolution.csv")
