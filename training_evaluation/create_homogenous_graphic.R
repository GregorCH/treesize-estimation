#! /usr/bin/Rscript

library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)

data.generated <- read_csv("output.homogenous/generated/train-testerrors.csv")
data.chromaticindex <- read_csv("output.homogenous/chromaticindex/train-testerrors.csv")
data.fc <- read_csv("output.homogenous/fc//train-testerrors.csv")
data.fixedCostNetworkFlow <- read_csv("output.homogenous/fixedCostNetworkFlow//train-testerrors.csv")
data.iis <- read_csv("output.homogenous/iis//train-testerrors.csv")
data.map <- read_csv("output.homogenous/map/train-testerrors.csv")
data.opm2 <- read_csv("output.homogenous/opm2//train-testerrors.csv")

datalist <- list(
  generated=data.generated,
  chromaticindex=data.chromaticindex,
  # fc=data.fc,
  "network-flow"=data.fixedCostNetworkFlow,
  iis=data.iis,
  map=data.map,
  opm2=data.opm2
)

custom.methods <- c("tree profile", "wbe", "profile")
simple.search.completion.methods <- c("gap", "ssg", "leaf frequency", "tree weight")
forecast.methods <- c(simple.search.completion.methods, "open nodes")

for (d in 1:length(datalist)) {
  datalist[[d]]$Set <- names(datalist)[d]
}
data.set <- do.call(rbind, datalist)

data.set <- data.set %>% mutate(Group=ifelse(
  Method %in% custom.methods,"Custom",
  ifelse(
    is.na(MSE), "DES",
    ifelse(Method %in% simple.search.completion.methods, "Search Completion", "Learned")
  )
))

weights.methods <- data.frame(
  Method = c("gap", "leaf frequency", "Linear.model", "linear.monotone",
  "open nodes", "random forest MIPLIB", "Random.Forest.Big", "Random.Forest.Reasonable",
  "Reg.tree", "ssg", "tree profile","tree weight","wbe"),
  Method.weight = c(2,5,10,11,1,20,14,13,12,3,7,4,8)
)

weights.groups <- data.frame(
  Group=c("DES","Search Completion","Learned","Custom"),
  Group.weight=c(300,100,400,200)
)


results <- data.set %>% filter(Training == "Test") %>% reshape2::dcast(Method+Group~Set, value.var = "meanAbsE")
results.arranged <- results %>% arrange(Group,Method) %>% reshape2::melt()
results.arranged <- merge(results.arranged, weights.groups, by = "Group")
results.arranged <- merge(results.arranged, weights.methods, by = "Method")
results.arranged <- results.arranged %>% mutate(Weight=Method.weight+Group.weight,
                                                X1=paste(Group, Method, sep = " ")) %>%
                    arrange(Weight)
subresults.arranged <- results.arranged %>% filter(variable == "iis")
results.arranged$X2 <- factor(results.arranged$X1, levels=subresults.arranged$X1[order(subresults.arranged$Weight)])


ggplot(results %>% arrange(Group,Method) %>% reshape2::melt(),
       aes(variable, paste(Group, Method, sep = " "), fill=log(value),label=sprintf("%.2f",value))) + geom_tile() + geom_text()
new_theme <- theme(legend.position = "bottom",
                   legend.title = element_blank(),
                   legend.text = element_text(size = 16),
                   axis.text = element_text(size=16),
                   axis.title = element_text(size=16),
                   strip.text = element_text(size=16),
)



ggplot2::theme_set(theme_minimal(base_size = 16) + new_theme)
ggplot(results.arranged, aes(X2, value, fill=factor(Group, levels=c("Search Completion", "Custom", "DES", "Learned")), label=sprintf("%.2f", value))) +
  geom_col() +
  theme(axis.text.x = element_text(angle=30, vjust=.8, hjust=0.8)) +
  facet_wrap(~variable, ncol=1, strip.position = "right") + geom_text(vjust=-0.3) +
  scale_fill_manual(values=viridis::magma(4, begin = 0.1, end = 0.7)) +
  scale_x_discrete(
    labels=subresults.arranged$Method %>% gsub("[.-]", " ", .) %>% tolower() %>% gsub("tree profile", "profile", .)
                   ) + scale_y_log10() + xlab("") + ylab("Normalized Ratio E") + coord_cartesian(ylim=c(1,13))

ggsave("homogeneous.pdf", width=12.17, height = 18.8)
