#! /usr/bin/Rscript

library(readr)
require(magrittr)
require(dplyr)

# read all tables and combine them into one list.

tablefiles <- dir("./windowsizes/", pattern = "*.csv") %>% paste("./windowsizes/", ., sep = "")

bigtable <- lapply(tablefiles, read_csv) %>% do.call(rbind, .)
mysummary <- bigtable %>%
  group_by(Windowsize, Level) %>%
  summarize(ErrProg=mean(ErrorProg),
            ErrRes=mean(ErrorRes),
            ErrFreq=mean(ErrorFreq)) %>%
  as.data.frame()
write.csv(mysummary, file = "windowsizes/summary.csv")
