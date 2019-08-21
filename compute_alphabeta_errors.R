#! /usr/bin/Rscript

#
# compute median and mean absolute errors for combinations of alpha and beta in double exponential smoothing.
#
# intermediate results are not stored currently due to space considerations
#
source("./functions.R")

# make 95 forecasts ranging from 1% to 95 % of the cumulative progress.
rellevels <- seq(0.01, 0.95, by = 0.01)

# summarize each forecast
datafiles <- getDataFilesInPath("../MMMc_v3") %>% grep("uniform", ., value = T)
stepwidth <- 0.05
testalphas <- seq(0.1,0.99, by = stepwidth)

errorsAllParams <- list()
for ( alpha in testalphas ) {
  for( beta in seq(0.1,alpha, by = stepwidth) ) {

    errorsAlphaBetaList <- list()
    print(alpha)
    print(beta)
    # iterate through all problems and problemspecific rellevels
    for( d in datafiles ) {
      df <- probsFile2dataFrame(d, maxsize = 1024)

      print(progressPlotTitle(d))

      if(nrow(df) < 10)
        next
      problemspecificrellevels <- getProblemSpecificRellevels(df, rellevels)

      if( length(problemspecificrellevels) == 0 )
        next

      errorsProblem <- lapply(problemspecificrellevels, function(x) summarize_one_forecast_linear(df, x, "des-paper", alphabeta = c(alpha, beta))) %>%
        dplyr::bind_rows() %>%
        mutate(Level=problemspecificrellevels) %>%
        as.data.frame()

      errorsAlphaBetaList[[length(errorsAlphaBetaList) + 1]] <- errorsProblem

    }

    # concatenate the errors at this level of alpha and beta and compute the median errors and mean absolute errors (MAPE) of the forecasts
    completedf <- errorsAlphaBetaList %>% dplyr::bind_rows()

    summarizeddf <- completedf %>%  dplyr::summarize(
      medErrProg=median(ErrorProg),
      medErrFreq=median(ErrorFreq),
      medErrUns=median(ErrorUns),
      medErrSsg=median(ErrorSsg),
      meanAbsErrProg=mean(abs(ErrorProg)),
      meanAbsErrFreq=mean(abs(ErrorFreq)),
      meanAbsErrUns=mean(abs(ErrorUns)),
      meanAbsErrSsg=mean(abs(ErrorSsg))
    ) %>% dplyr::mutate(alpha=alpha, beta=beta)

    errorsAllParams[[length(errorsAllParams) + 1]] <- summarizeddf
  }
}

# concatenate the individual error measurements at all levels of alpha and beta and save them to a csv
errorsAllParamsdf <- errorsAllParams %>% dplyr::bind_rows() %>% as.data.frame()
errorsAllParamsdf %>% write.csv(file="errors_alphabeta.csv")
