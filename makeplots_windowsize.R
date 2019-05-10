#! /usr/bin/Rscript
library(magrittr)
library(ggplot2)
mysummary <- read.csv("windowsizes/summary.csv")

for (m in c("ErrRes", "ErrFreq", "ErrProg")) {
  g <- mysummary %>% dplyr::mutate(Windowsize=factor(Windowsize)) %>%
    ggplot(aes_string("Windowsize", m)) + geom_boxplot() + geom_jitter(alpha=0.1)
  ggsave("windowsize_%s.png" %>% sprintf(m), unit="in", width=10, height=7)
}
