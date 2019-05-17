#! /usr/bin/Rscript
#
# script to compute and save forecast errors for different methods.
#
source("./functions.R")

if(! dir.exists("tables") ) {
  print("Creating 'tables' directory")
  dir.create("tables")
}

# make 75 forecasts ranging from 1% to 75 % of the cumulative progress.
rellevels <- seq(0.01, 0.75, by = 0.01)

# summarize each forecast
datafiles <- getDataFilesInPath("../MMMc_v3")

uniformdatafiles <- datafiles %>% grep("Data/uniform", ., value = T)

for(d in uniformdatafiles) {
  print(progressPlotTitle(d))
  for (method in c("ets", "w-linear", "des-paper")) {
    df <- probsFile2dataFrame(d)
    if(nrow(df) < 10)
      next
    problemspecificrellevels <- getProblemSpecificRellevels(df, rellevels)
    if( length(problemspecificrellevels) == 0 )
      next()

    if( method == "w-linear" )
      windowsizes <- c(50)
    else
      windowsizes <- c(50)

    for( w in windowsizes )
    {

      methodname <- ifelse(method == "w-linear",
                           paste(method,w, sep = " "),
                           method)
      lapply(problemspecificrellevels, function(x) summarize_one_forecast(df, x, method, windowsize = w)) %>%
        dplyr::bind_rows() %>%
        mutate(Level=problemspecificrellevels,
               Method = methodname) %>%
        as.data.frame() %>%
        write.csv("tables/%s_%s.csv" %>%
                    sprintf(progressPlotTitle(d), methodname %>% gsub(" ", "_", .))
                  )
    }
  }
}

