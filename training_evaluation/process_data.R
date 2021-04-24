#! /usr/bin/Rscript

#
# 1. Analyze accuracy of different tree size prediction methods
# 2. Train regression forests and linear regression to approximate search completion
# 3. Save the results as tables, plots, and files that SCIP can read
#

library(readr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(knitr)
library(rpart)
library(randomForest)
library(reshape2)

Random.Seed <- 123
split.method <- 'prob' # train/test split by 'prob'lem, 'record', or nondefault 'seeds' (test on default seed)
split.stage <- FALSE # use three stages early/intermediate/late
comparison.name <- "random forest MIPLIB"


iskaccurrate <- function(x,k) {
  (x >= (1/k)) & (x <= k)
}

computeEstimation <- function(x,current) {
  x <- pmax(x,1e-6)
  x <- pmin(x, 1.0)

  current / x
}

SaveCsvMessage <- function(data, filename, data.description) {
    write_csv(data, filename)
    print("Stored %s under %s" %>% sprintf(data.description, filename))
}

printMethod <- function(x) {
  x %>% tolower() %>% gsub(".", " ", ., fixed = T) %>% gsub("reg tree", "regression tree", .)
}

zeroIndex <- function(x) {
  (x %>% as.integer()) - 1
}

turnIntoCSV <- function(rf.model, filename) {
  ntrees <- rf.model$ntree
  tree_as_data <- do.call(rbind, lapply(1:ntrees, function(x) {randomForest::getTree(rf.model, k = x)})) %>% as.data.frame()

  index_cols <- c("left daughter", "right daughter", "split var")
  tree_as_data[index_cols] <- lapply(tree_as_data[index_cols], zeroIndex)
  tree_as_data["node"] <- zeroIndex(rownames(tree_as_data))
  tree_as_data <- tree_as_data %>% mutate(value=ifelse(`split var` == -1, prediction, `split point`))

  "### NTREES=%d FEATURE_DIM=%d LENGTH=%d\n" %>%
    sprintf(ntrees, max(tree_as_data$`split var`) + 1, nrow(tree_as_data)) %>%
    cat(file = filename, append = FALSE)

  write.table(tree_as_data[c("node", "left daughter", "right daughter", "split var", "value")],
            file = filename,
            append = TRUE,
            col.names = FALSE,
            row.names = FALSE,
            sep=",")
}

# function that summarizes records by custom grouping
# data must have columns 'ApproxError' and 'Ratio'
SummarizeErrors = function(data, ...) {

  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    summarise(n=n(),
              "MSE"=mean(ApproxError),
              "meanAbsE" = 2**mean(abs(log2(Ratio))),
              "2Accurate" = 100 * mean(iskaccurrate(Ratio,2)),
              "3Accurate" = 100 * mean(iskaccurrate(Ratio,3)),
              "4Accurate" = 100 * mean(iskaccurrate(Ratio,4))
    ) %>% as.data.frame()
}


#
# reorder the methods as they should finally appear
#
reorderByMethod <- function(data) {
  myultimatelevels <- c(
    "open nodes",
    "gap",
    "ssg",
    "tree weight",
    "leaf frequency",
    "profile",
    "wbe",
    "Linear.model",
    "linear.monotone",
    "Reg.tree",
    "Random.Forest.Big",
    "Random.Forest.Reasonable"
  )

  data %>% arrange(factor(Method, levels = myultimatelevels))
}

args <- commandArgs(trailingOnly = TRUE)
#args <- c("output.cutoff/")
if( length(args) < 1 ) {
    stop("Missing positional argument for output directory")
}

output.dir <- args[1]
actual.filename <- "%s/actual.csv" %>% sprintf(output.dir)
raw.table.filename <- "%s/table1.csv" %>% sprintf(output.dir)
transformed.table.filename <- "%s/table2.csv" %>% sprintf(output.dir)
filtered.table.filename <- "%s/table3.csv" %>% sprintf(output.dir)
training.data.filename <- "%s/training_dataset.csv" %>% sprintf(output.dir)
test.data.filename <- "%s/test_dataset.csv" %>% sprintf(output.dir)
training.labels.filename <- "%s/training_labels.csv" %>% sprintf(output.dir)
test.labels.filename <- "%s/test_labels.csv" %>% sprintf(output.dir)
regression.tree.filename <- "%s/RegTree.pdf" %>% sprintf(output.dir)
errors.summary.table.filename <- "%s/errors_summary_table.csv" %>% sprintf(output.dir)
search.completion.mse.plot.filename <- "%s/searchcompletion_mse.pdf" %>% sprintf(output.dir)
data.density.plot.filename <- "%s/RelTime.pdf" %>% sprintf(output.dir)
rf.model.big.filename <- "%s/rf_model_big.rfcsv" %>% sprintf(output.dir)
rf.model.big.rdsfilename <- "%s/rf_model_big.rds" %>% sprintf(output.dir)
rf.model.comparison.rdsfilename <- "%s/other_rf_model.rds" %>% sprintf(output.dir)
rf.model.reasonable.filename <- "%s/rf_model_reasonable.rfcsv" %>% sprintf(output.dir)

# read actual nodes
actual <- read.csv(actual.filename, header=FALSE, col.names = c("File", "Actual", "TotTime"), stringsAsFactors = FALSE)
row.names(actual) <- actual$File

config.filename <- sprintf("%s/config.R", output.dir)
if (file.exists(config.filename)) {
  source(config.filename)
  cat(sprintf("Sourcing configuration from '%s'", config.filename), sep = "\n")
}


# read estimations
table1 <- read_csv(raw.table.filename, col_names = c(
  "File",
  "Method",
  "Report",
  "Time",
  "Current",
  "Leaves",
  "Unsolved",
  "TreeWeight",
  "Estim",
  "Value",
  "Trend",
  "Resolution",
  "Smooth"
  ), col_types = cols(
    File=col_character(),
    Method=col_character(),
    .default = col_double())
  )
# combine them
table1$Actual <- actual[table1$File, "Actual"]
table1$TotTime <- actual[table1$File, "TotTime"]
table1$CorrEstim <- ifelse(table1$Estim < table1$Current, table1$Current + table1$Unsolved, table1$Estim)
table1$Ratio <- (table1$CorrEstim / table1$Actual)
table1$AbsRatio <- abs(table1$Ratio)
table1$Level <- (table1$TreeWeight * 100) %>% as.integer() / 100.0
table1$RelTime <- ((table1$Time/table1$TotTime) * 100) %>% as.integer() / 100.0
table1$ApproxError <- NaN
table1$Prob <- basename(table1$File) %>% gsub("bzfhende.(miplib2017|MMMc).\\d+_([^.]*)", "\\2", ., perl = T)
table1$Prob2 <- table1$Prob %>% as.character() %>%  gsub("periodic_report.*.out", "", .)
table1$Method <- table1$Method %>% gsub("-", " ", .) %>% gsub("treeprofile", "profile", .)
# try to determine the seed
seedlist <- c("report-s1.out$", "report-s2.out", "report-s3.out$")
table1$Seed <- 0
for (i in 1:3) {
  table1$Seed[grep(seedlist[i], table1$File)] <- i
}

if (split.stage) {
table1 <- table1 %>% dplyr::mutate(Stage=ifelse(TreeWeight <= 0.3, "\\GrpEarly",
                                              ifelse(TreeWeight <= 0.6, "\\GrpIntermediate", "\\GrpLate")))
} else {
  table1$Stage <- "\\GrpAll"
}


SaveCsvMessage(table1, transformed.table.filename, "transformed data")

#
# Step 2 Filtering the results. We use a fresh data frame
#

table2 <- read.csv(transformed.table.filename)
table2 <- table2 %>% dplyr::filter(Actual >= 100) %>%
  dplyr::filter((TreeWeight != 1.0) | (Report != 1)) # filter out records before restarts
  # output.marc/cutoff/table3.csv# keep only instances that require at least 100 nodes
# in rare cases, there might be two observations recorded at the same level. Keep the first
table2 <- table2 %>% dplyr::distinct(Method,Prob,Level, .keep_all = TRUE) %>% as.data.frame()


# write.csv(table2, "sap-results/table3.csv")
SaveCsvMessage(table2, filtered.table.filename, "filtered data")

table3 <- read.csv(filtered.table.filename)

dataset <- data.frame(
  (table3 %>% filter(Method == "tree weight") %>% select(Value)),
  (table3 %>% filter(Method == "tree weight") %>% select(Trend)),
  (table3 %>% filter(Method == "ssg") %>% select(Value)),
  (table3 %>% filter(Method == "ssg") %>% select(Trend)),
  (table3 %>% filter(Method == "leaf frequency") %>% select(Value)),
  (table3 %>% filter(Method == "leaf frequency") %>% select(Trend)),
  (table3 %>% filter(Method == "gap") %>% select(Value)),
  (table3 %>% filter(Method == "gap") %>% select(Trend)),
  (table3 %>% filter(Method == "open nodes") %>% select(Trend)) %>% { . < 0 }
)

colnames(dataset) <- c("TreeWeight.Value", "TreeWeight.Trend",
                       "Ssg.Value", "Ssg.Trend",
                       "Leaffreq.Value", "Leaffreq.Trend",
                       "Gap.Value", "Gap.Trend",
                       "OpenTrend"
                       )

singletable <- table3 %>% filter(Method == "ssg")
singletable <- singletable %>% dplyr::mutate(Relnodes = Current / Actual)
relnodes <- singletable$Relnodes

set.seed(Random.Seed)
if( split.method == 'record' ) {
  isTrain <- (1:nrow(dataset)) %in% sample(1:nrow(dataset), 0.8 * nrow(dataset))
} else if (split.method == 'prob') {
  if (file.exists("%s/testprobs.list" %>% sprintf(output.dir))) {
    testprobs <- scan("%s/testprobs.list" %>% sprintf(output.dir), what=character())
    isTrain <- !(singletable$Prob2 %in% testprobs)
  } else {
    probs <- unique(singletable$Prob2)
    isTrain <- singletable$Prob2 %in% sample(probs, 0.8 * length(probs), replace = FALSE)
    singletable$Prob2[!isTrain] %>% as.character() %>% unique() %>% cat(file = "%s/testprobs.list" %>% sprintf(output.dir))
  }

} else if (split.method == 'seeds') {
  # split on seeds
  isTrain <- grepl("-s\\d.out$", singletable$Prob, perl = T)
} else {
  stop(sprintf("unkown split.method '%s' specified", split.method))
}

table3$Training <- ifelse(rep(isTrain, each=7), "Training", "Test")


trainingset <- dataset[isTrain,]
testset <- dataset[!isTrain,]
print(table(isTrain))
SaveCsvMessage(trainingset, training.data.filename, "training features")
SaveCsvMessage(testset, test.data.filename, "test features")
cat(relnodes[isTrain], file = training.labels.filename, sep = "\n")
cat(relnodes[!isTrain], file = test.labels.filename,sep = "\n")

# learn linear models and regression tree
linear.model <- lm(relnodes[isTrain]~.,data = trainingset)
linear.monotone <- lm(relnodes[isTrain]~TreeWeight.Value+Ssg.Value,data = trainingset)
print("Summary of linear monotone regression")
print(summary(linear.monotone))

# learn regression tree
set.seed(Random.Seed)
reg.tree <- rpart(relnodes[isTrain]~.,data = trainingset)
pdf(regression.tree.filename)
rpart.plot::rpart.plot(reg.tree, type =4)
dev.off()

# learn regression forests
set.seed(Random.Seed)
rf.model.big <- randomForest(relnodes[isTrain]~.,data = trainingset, ntree = 200, nodesize=25)
set.seed(Random.Seed)
rf.model.reasonable <- randomForest(relnodes[isTrain]~.,data = trainingset, ntree = 100, nodesize=75)
turnIntoCSV(rf.model.big, rf.model.big.filename)
turnIntoCSV(rf.model.reasonable, rf.model.reasonable.filename)
saveRDS(rf.model.big, file=rf.model.big.rdsfilename)

# load a random forest model to compare
if (file.exists(rf.model.comparison.rdsfilename)) {
  rf.model.comp <- readRDS(rf.model.comparison.rdsfilename)
  cat(sprintf("Additional model read from '%s'", rf.model.comparison.rdsfilename), sep = "\n")
} else {
  rf.model.comp <- NULL
  cat("No additional model given", sep = "\n")
}

model.list <- list(reg.tree, rf.model.big, rf.model.reasonable, linear.model,linear.monotone)
model.names <- c("Reg.tree","Random.Forest.Big","Random.Forest.Reasonable", "Linear.model", "linear.monotone")
if (!is.null(rf.model.comp)) {
  model.list[[length(model.list) + 1]] <- rf.model.comp
  model.names[[length(model.names) + 1]] <- comparison.name
}
# combine predicted labels into test results data frame
p <- lapply(model.list, function(x) {predict(x, dataset)})
testresults <- do.call(cbind, p) %>% as.data.frame()
colnames(testresults) <- model.names
testresults$Relnodes <- relnodes
testresults <- testresults %>% dplyr::mutate("Reg.Tree.Estim"=computeEstimation(Reg.tree, singletable$Current),
                              "Linear.Estim"=computeEstimation(Linear.model, singletable$Current),
                              "Linear.Easy.Estim"=computeEstimation(linear.monotone, singletable$Current),
                              "Random.Forest.Big.Estim"=computeEstimation(Random.Forest.Big, singletable$Current),
                              "Random.Forest.Reasonable.Estim"=computeEstimation(Random.Forest.Reasonable, singletable$Current)
                                )
if(!is.null(rf.model.comp)) {
  testresults[["%s.Estim" %>% sprintf(comparison.name)]] <- computeEstimation(testresults[[comparison.name]], singletable$Current)
}
testresults$Stage <- singletable$Stage

# compute an error table including MSE for the simple measures gap,ssg, tree weight, leaf frequency
approx.data <- dataset %>%
  mutate("tree weight"=TreeWeight.Value,
         ssg= 1 - Ssg.Value,
         gap=Gap.Value,
         "leaf frequency"=2 * pmax(Leaffreq.Value, 0.0)) %>%
  select(gap, ssg, `tree weight`, `leaf frequency`) %>%
  reshape2::melt(value.name = "Approx")

approx.data <- approx.data %>%
  mutate(ApproxError=(Approx - singletable$Relnodes) ** 2,
         Estim=computeEstimation(Approx, singletable$Current),
         Ratio=Estim / singletable$Actual,
         Training=(rep(isTrain, 4) %>% ifelse("Training", "Test"))
  )
simple.search.completion.errors <- approx.data %>%
  SummarizeErrors(Training, rep(singletable$Stage, 4), variable)
colnames(simple.search.completion.errors)[2:3] <- c("Stage", "Method")
print(simple.search.completion.errors)

custom.methods <- c("tree-profile", "wbe", "treeprofile", "profile") # we wrote tree-profile w/o hyphen in an earlier version

my_error_summary <- table3 %>%
  mutate(Training=rep(isTrain, each=7) %>% ifelse("Training", "Test")) %>%
  SummarizeErrors(Training, Stage, Method)
print(my_error_summary)

# custom methods: take estimation from table3
custom.method.errors <- my_error_summary %>%
  filter(Method %in% custom.methods) %>%
  as.data.frame()
print(custom.method.errors)

# compute errors for the remaining double exponential smoothing methods
double.exponential.errors <- my_error_summary %>%
  filter(!Method %in% custom.methods) %>%
  as.data.frame()
print(double.exponential.errors)

# learned search completion methods (monotone) linear regression, regression tree and forests
learned.data.search.completion.1 <- testresults[,model.names] %>%
  reshape2::melt(value.name = "Approx")

learned.data.search.completion.1 <- learned.data.search.completion.1 %>%
  mutate(ApproxError=(Approx - singletable$Relnodes) ** 2,
         Estim=computeEstimation(Approx, singletable$Current),
         Ratio=Estim / singletable$Actual,
         Training=(rep(isTrain, length(model.list)) %>% ifelse("Training", "Test"))
  )

learned.data.search.completion.1.errors <- learned.data.search.completion.1 %>%
  SummarizeErrors(Training, rep(singletable$Stage, length(model.list)), variable)
colnames(learned.data.search.completion.1.errors)[2:3] <- c("Stage", "Method")
print(learned.data.search.completion.1.errors)

# set up estimation ensemble coefficients
ensemble.coeffs.early <- c("ssg"=0.292,
                           "wbe"=0.0,
                           "leaf frequency"=0.469,
                           "tree weight"=0.381,
                           "profile"=0.0,
                           "gap"=0.02,
                           "open nodes"=0.004)
ensemble.coeffs.intermediate <- c("ssg"=0.012,
                           "wbe"=0.156,
                           "leaf frequency"=0.351,
                           "tree weight"=0.193,
                           "profile"=0.0,
                           "gap"=0.011,
                           "open nodes"=0.051)
ensemble.coeffs.late <- c("ssg"=0.003,
                          "wbe"=0.579,
                          "leaf frequency"=0.282,
                          "tree weight"=0.033,
                          "profile"=0.0,
                          "gap"=0.0,
                          "open nodes"=0.024)
# extend them to the length of the entire table
ens.coeffs.early <- rep(ensemble.coeffs.early, length.out=nrow(table3))
ens.coeffs.inter <- rep(ensemble.coeffs.intermediate, length.out=nrow(table3))
ens.coeffs.late <- rep(ensemble.coeffs.late, length.out=nrow(table3))
# merge them into 1 column by considering the stage
table3 <- table3 %>% mutate(EnsembleCoeffs=ifelse(TreeWeight <= 0.3,
                                           ens.coeffs.early,
                                           ifelse(TreeWeight <= 0.6,
                                                  ens.coeffs.inter,
                                                  ens.coeffs.late
                                                  )
                                           )
                  )

# ! this table has a different sorting than dataset and singletable !
ensembleestim.data <- table3 %>%
  group_by(Prob, Level) %>%
  summarise(
     Ens.Estim = EnsembleCoeffs %*% Estim,
     TreeWeight=TreeWeight[1],
     Actual=Actual[1],
     Ratio=Ens.Estim / Actual,
     Stage=ifelse(TreeWeight <= 0.3, "\\GrpEarly",
                  ifelse(TreeWeight <= 0.6, "\\GrpIntermediate", "\\GrpLate")
     ),
     ApproxError=NaN,
     Method="ensemble"
 )

ensembleestim.errors <- ensembleestim.data %>%
  SummarizeErrors(Stage,Method)
print(ensembleestim.errors)

errors.summary.table <- rbind(simple.search.completion.errors %>% reorderByMethod(),
      custom.method.errors %>% reorderByMethod(),
      double.exponential.errors  %>% reorderByMethod(),
      learned.data.search.completion.1.errors  %>% reorderByMethod()
      # ensembleestim.errors
      )

SaveCsvMessage(errors.summary.table, errors.summary.table.filename, "summary of errors for each stage")

# loop over the 3 stages early, intermediate, late
for( s.pos in 1:length(levels(table3$Stage)) )
{
  for (t in c("Training", "Test")) {

    options(knitr.kable.NA = '-')
    knitr::opts_current$set(label = "summary%d" %>% sprintf(s.pos))
    stage <- unique(errors.summary.table$Stage)[s.pos]
    suffix <- sprintf("_%d_%s", s.pos, t)
    # table.for.printing <-
    table.for.printing <- errors.summary.table %>% filter(Stage == stage) %>% filter(Training == t) %>% select(-Stage,-Training) %>%
      mutate(Method=Method %>% printMethod())
    table.for.printing[,c("2Accurate", "3Accurate", "4Accurate")] <- apply(
      table.for.printing[,c("2Accurate", "3Accurate", "4Accurate")], 2, function(f) {paste0(format(f, digits=3, nsmall = 1), "\\,\\%")})
    # %>% mutate_if(is.numeric, format, digits=3, nsmall=0, scientific=100)
    caption <- "Estimate comparison during the %s stage ($ %s $) on %s set" %>%
      sprintf(ifelse(s.pos == 1, "early", ifelse(s.pos == 2, "intermediate", "late")), stage, tolower(t))
    knitr::kable(table.for.printing, format = "latex", booktabs = TRUE, escape = FALSE, linesep="",
                 col.names = c("Method", "$n$", "MSE", "\\meanAbsE", "2-Acc", "3-Acc", "4-Acc"),
                 digits = c(rep(3,4),rep(1,3)),
                 caption = caption) %>%
      kableExtra::kable_styling() %>%
      kableExtra::group_rows(index=c("Search Completion Approximation" = 4,
                                     "Custom Estimations" = 2,
                                     "Double Exponential Smoothing" = 5,
                                     "Learned Methods"=length(model.list)
                                     )) %>%
      cat(file=sprintf("%s/errors_all_instances_and_methods%s.tex", output.dir, suffix))
  }
}

learned.data.search.completion.1.bytraining.errors <- learned.data.search.completion.1 %>%
  SummarizeErrors(rep(singletable$Stage, length(model.list)), variable, Training)
colnames(learned.data.search.completion.1.bytraining.errors)[1:2] <- c("Stage", "Method")

simple.search.completion.bytraining.errors <- approx.data %>%
  SummarizeErrors(rep(singletable$Stage, 4), variable, Training)
colnames(simple.search.completion.bytraining.errors)[1:2] <- c("Stage", "Method")


bytraining.errors <- rbind(simple.search.completion.bytraining.errors,
                           learned.data.search.completion.1.bytraining.errors)
bytraining.errors2 <- bytraining.errors
bytraining.errors3 <- table3 %>% SummarizeErrors(Stage, Method, Training)
write.csv(file = "%s/train-testerrors.csv" %>% sprintf(output.dir), x = rbind(bytraining.errors2 %>% as.data.frame(),bytraining.errors3 %>% as.data.frame()))
bytraining.errors <- bytraining.errors %>% mutate(Method= Method %>% printMethod())
bytraining.errors$Training <- factor(bytraining.errors$Training)
bytraining.errors$Training <- factor(bytraining.errors$Training, levels=levels(bytraining.errors$Training)[c(2,1)])
bytraining.errors$Method <-factor(bytraining.errors$Method)
if(is.null(rf.model.comp)) {
  bytraining.errors$Method <- factor(bytraining.errors$Method, levels = levels(bytraining.errors$Method)[c(5,6,7,4,3,2,9,8,1)])
}

bytraining.errors$Stage <- bytraining.errors$Stage %>%
  gsub(".*GrpEarly", "[0.0, 0.3]", .) %>%
  gsub(".*GrpIntermediate", "[0.3, 0.6]", .) %>%
  gsub(".*GrpLate", "[0.6, 1.0]", .)
bytraining.errors$Stage <- factor(bytraining.errors$Stage, levels = c("[0.6, 1.0]", "[0.3, 0.6]", "[0.0, 0.3]"))


bytraining.errors <- bytraining.errors[order(bytraining.errors$Stage, bytraining.errors$Training),]
ssg.pos <- which(bytraining.errors$Method == "ssg") %>% rep(each=4 + length(model.list))
bytraining.errors$MSEQ <- bytraining.errors$MSE / bytraining.errors$MSE[ssg.pos]
bytraining.errors <- bytraining.errors %>%
  mutate(MSELabel=sprintf("%.3f (x%.2f)", MSE, MSEQ))
g <- ggplot(bytraining.errors, aes(Method, MSE, fill = Stage, label=MSELabel)) +
  geom_col(position = "dodge") +
  geom_text(check_overlap = TRUE, position = position_dodge(1.0), hjust = -0.3) +
  facet_wrap(~Training) + coord_flip() +
  # we revert the breaks to match vertical placement of stages.
  scale_fill_manual(values=viridis::plasma(3, begin = 0.1, end = 0.7), breaks=c("[0.0, 0.3]", "[0.3, 0.6]", "[0.6, 1.0]")) +
  # scale_fill_viridis_d(option="plasma",begin = 0.1, end = 0.7) +
  theme_light() + xlab(NULL) + ylim(0.0, 0.8)
ggsave(search.completion.mse.plot.filename, width = 8.5, height=5.5)

# save the data density plot, as well.
g <- ggplot(singletable, aes(singletable$RelTime - singletable$Relnodes)) +
  geom_density(aes(y=..scaled..), fill="#3F8BCA", alpha=0.75) + theme_light() +
  xlab("Difference Relative Time - Search Completion") + ylab("")

ggsave(data.density.plot.filename, width = 8.5, height=5.5)

