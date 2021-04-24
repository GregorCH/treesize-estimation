# iterate through all problems and problemspecific rellevels
errorsAlphaBetaList <- list()
for( d in datafiles) {
  df <- probsFile2dataFrame(d, maxsize = 1024)

  print(progressPlotTitle(d))

  if(nrow(df) < 10)
    next
  problemspecificrellevels <- getProblemSpecificRellevels(df, rellevels)

  if( length(problemspecificrellevels) == 0 )
    next

  errorsProblem <- lapply(problemspecificrellevels, function(x) summarize_one_forecast_linear(df, x, "des-paper", alphabeta = c(0.1,0.1))) %>%
    dplyr::bind_rows() %>%
    mutate(Level=problemspecificrellevels,
           Problem=d %>% basename()) %>%
    as.data.frame()

  errorsAlphaBetaList[[length(errorsAlphaBetaList) + 1]] <- errorsProblem
}

completedf <- errorsAlphaBetaList %>% dplyr::bind_rows()
