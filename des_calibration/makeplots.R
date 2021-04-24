#! /usr/bin/Rscript

#
# plot different visualizations of the forecast errors, and the corresponding errors in tree size estimation
#
library(readr)
library(ggplot2)
source("functions.R")
datafiles <- c("uniform_errors.csv")

measurements <- c("ErrorProg", "ErrorFreq", "ErrorUns")

for( d in datafiles )
{
  errors <- read_csv(d) %>%
    # subset(Actual >= 1000) %>% # restrict to larger trees
    subset(Level <= 0.5) # restrict to first half of the search.

  for (m in measurements) {
    g <- ggplot(errors, aes(x=factor(Level), y=errors[[m]], fill=Method)) +
      geom_boxplot(outlier.alpha = 0.1) +
      theme(axis.text.x=element_text(size = 8, angle = 45)) +
      scale_fill_brewer(palette = "Accent") +
      xlab("Progress") + ylab("Prediction Error log_2(pred/actual)")

    ggsave("%s_%s.pdf" %>% sprintf(gsub(".csv", "", d), m), units = "in", width = 12, height = 7)
  }
}

