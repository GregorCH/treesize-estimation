#! /usr/bin/Rscript

library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

errors_capacity_noresolution <- read_csv("errors_capacity_noresolution.csv")
errors_capacity <- read_csv("errors_capacity.csv")

errors.data <- bind_rows(errors_capacity, errors_capacity_noresolution) %>% select(-medErrProg, -medErrSsg, -medErrUns, -medErrFreq)

# todo rename measures to meet paper
colnames(errors.data)[2:5] <- c("tree weight",
                                "leaf frequency",
                                "open nodes",
                                "ssg")

plot.errors.data <- errors.data[c(2:5,7)] %>% melt(id.vars="capacity", variable.name="Measure", value.name = "Log.E")
# recode capacity to fit the plot
plot.errors.data$Measure <- plot.errors.data$Measure %>% as.character
plot.errors.data$capacity <- pmin(plot.errors.data$capacity, 2 ** 16)
ggplot(plot.errors.data %>% filter(capacity < 1e+6), aes(capacity, 2 ** Log.E, col=Measure,shape=Measure)) +
  geom_point(alpha=0.75, size=1.4) +
  scale_color_viridis_d(option="magma", end=0.8) +
  ylim(2,6) +
  scale_x_log10(breaks=2 ** (3:16), labels=c(sprintf("%d", 2 ** (3:15)), "Infinity")) +
  ylab("geom. mean normalized ratio E") +
  xlab("Capacity") + theme_light() + theme(axis.text.x = element_text(angle=45, size = 7), text = element_text(size=8))
  # ggtitle("Calibration of Resolution Capacity")
ggsave("resolution_capacity.pdf", width = 345 / 72, height=345 / 72 / 1.6)

# View(errors.data)
#
# errors_alphabeta %>% mutate(
#   meanAbsErrFreq = 2 ** meanAbsErrFreq,
#   meanAbsErrProg = 2 ** meanAbsErrProg,
#   meanAbsErrUns = 2 ** meanAbsErrUns,
#   meanAbsErrSsg = 2 ** meanAbsErrSsg
# ) %>% View()
