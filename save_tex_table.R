#! /usr/bin/Rscript

#
# plot different visualizations of the forecast errors, and the corresponding errors in tree size estimation
#
library(readr)
library(dplyr)
library(magrittr)
source("functions.R")
errors <- read.csv("uniform_errors.csv")

errors$Stage <- ifelse(errors$Level <= 0.3, "\\GrpEarly",
                          ifelse(errors$Level <= 0.6, "\\GrpIntermediate", "\\GrpLate")
                          )
my_error_summary <- errors %>% group_by(Stage, Method) %>% summarise(n=n(),
                     "\\medianErrorP"=median(ErrorProg),
                     "\\meanAbsP" = mean(abs(ErrorProg)),
                     "\\medianErrorF"=median(ErrorFreq),
                     "\\meanAbsF" = mean(abs(ErrorFreq)),
                     "\\medianErrorU"=median(ErrorUns),
                     "\\meanAbsU" = mean(abs(ErrorUns))
                     )


knitr::kable(my_error_summary, format = "latex",
             digits = 3, booktabs = TRUE, escape = FALSE,
             linesep = c("", "", "\\addlinespace")) %>%
  # kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(" " = 3, "Progress" = 2, "Leaf Frequency" = 2, "Uns. Nodes" = 2), escape = FALSE) %>%
  cat(file = "error_table.tex")

