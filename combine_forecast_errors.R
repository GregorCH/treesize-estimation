#! /usr/bin/Rscript
#
# concatenates all individual forecast error tables created with save_forecast_errors into one data frame and saves it.
#
library(readr)
source("functions.R")

# glue all uniform tables together.
uniformtables <- dir(path="tables/uniform/", pattern = "*.csv")

writeBigTable <- function(tables, path, filename, stripexpression="\\.p_u_.*") {
  dflist <- list()
  for(t in tables){
    df <- read_csv("%s/%s" %>% sprintf(path,t))
    df$Prob <- gsub(stripexpression, "", t)
    dflist[[length(dflist) + 1]] <- df
  }
  dflist %>% bind_rows() %>% write.csv(filename)
  print("Combined %d tables to %s" %>% sprintf(length(dflist), filename))
}

writeBigTable(uniformtables,
              "tables/uniform/",
              "uniform_errors.csv")

# glue all nonuniform tables together
nonuniformtables <- dir(path="tables/nonuniform/", pattern = "*.csv")
writeBigTable(nonuniformtables, "tables/nonuniform/", "nonuniform_errors.csv", stripexpression = "\\.p_k_.*")
