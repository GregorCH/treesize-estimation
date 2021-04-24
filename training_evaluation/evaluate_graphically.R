#! /usr/bin/Rscript

library(magrittr)
library(dplyr)
library(ggplot2)
library(ggpubr)

table3 <- read.csv("output_revision2//table3.csv")
table2 <- read.csv("output_revision2//table2.csv")

table2$Prob <- table2$Prob %>% gsub(".scip.M640.enable_treeprofile.out", "", .)

plotInstance <- function(probname) {
  subdf <- table2 %>% dplyr::filter(Prob == probname) #%>% dplyr::filter(Method != "treeprofile")
  subdf$Method <- subdf$Method %>% gsub("-", " ", .)

  new_theme <- theme(legend.position = "right",
                     legend.title = element_blank(),
                     legend.text = element_text(size = 12),
                     axis.text = element_text(size=12),
                     axis.title = element_text(size=14)
  )
  ggplot2::theme_set(theme_minimal() + new_theme)
  my_methods <- c("gap", "leaf frequency", "open nodes", "ssg", "tree weight")
  l <- list()
  scale_values <- list()
  for (m in my_methods) {
    subdf_method <- subdf %>% filter(Method == m)
    subdf_method <- subdf_method %>%
      mutate(maxValue=max(Value),
             maxCorrEstim=max(CorrEstim),
             CorrEstim2=CorrEstim / maxCorrEstim * maxValue)
    maxValue <- subdf_method$maxValue[1]
    maxCorrEstim <- subdf_method$maxCorrEstim[1]

    yInterceptActual <- subdf_method$Actual[1] / maxCorrEstim * maxValue
    print(yInterceptActual / maxValue * maxCorrEstim)
    scale_values[[m]] <- maxCorrEstim / maxValue
    l[[m]] <- subdf_method %>% ggplot(aes(Leaves, Value)) +
      geom_line(aes(col="Value", linetype="Value")) +
      geom_line(mapping = aes(y=CorrEstim2, col="Estimation", linetype="Estimation")) +
      facet_wrap(~Method) +
      geom_hline(yintercept = yInterceptActual) +
      scale_color_brewer("Curve", type="qual", palette=2) + scale_linetype_manual("Curve", labels=c("Estimation", "Value"), values=c(2,1))
  }

  # we need this post hoc secondary continuous scale because the formula is only applied lazily:
  # Only the last transformation would be active and distort all scales.
  g <- ggarrange(plotlist = lapply(names(l), function(x)l[[x]] + scale_y_continuous(name="Value", sec.axis=sec_axis(~. * scale_values[[x]], name="Estimation"))),
                 ncol=1, common.legend = T, legend="bottom")

  return(g)
}

for( probname in #c(
  # c("csched008")
  c("danoint", "neos-827015")
  # c("neos-955215", "drayage-25-23","app1-2"), # worst performance oracle early
  # c("prod1", "neos13", "eilB101"), # worst performance for oracle intermediate
  # c("neos13", "arki001", "neos-955800"), # worst performance for oracle late
  # c("ns1952667")
  # )
)
{
  g <- plotInstance(probname)
  # ggsave(sprintf("%s.pdf", probname), plot = g, width = 12.17,height = 15.83 / 3)
  ggsave(sprintf("%s.pdf", probname), plot = g, width=8.17, height = 11.8)
}

