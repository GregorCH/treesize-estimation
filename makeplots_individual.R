#! /usr/bin/Rscript

#
# plot different visualizations of the forecast errors, and the corresponding errors in tree size estimation
#
library(readr)
library(ggplot2)
source("functions.R")
errors <- read.csv("uniform_errors.csv")
interestinginstances <- c("../MMMc_v3/Data/uniform/41_danoint.p_u.probs",
                          "../MMMc_v3/Data/uniform/325_neos-827015.p_u.probs",
                          "../MMMc_v3/Data/uniform/19_bc.p_u.probs",
                          "../MMMc_v3/Data/uniform/393_neos-941698.p_u.probs"
                          )

for( i in interestinginstances ){
  t <- progressPlotTitle(i)
  print(t)
  df <- probsFile2dataFrame(i)
  errordf <- errors %>% subset(Prob == t)

  s  <- simultaneousplot(df, errordf)
  ggsave("interesting_%s.pdf" %>% sprintf(t), s)
}


