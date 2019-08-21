#! /usr/bin/Rscript
require(ggplot2)
require(magrittr)
#
# plot the interesting error results as a function of alpha and beta
#
# in order to produce the data "errors_alpha_beta.csv", run the script "compute_alphabeta_errors.R"
#
errors_alphabeta <- read.csv("./errors_alphabeta.csv")
errors_alphabeta$beta.as.factor <- factor(errors_alphabeta$beta)
for( d in c("Prog", "Freq", "Uns", "Ssg") )
{
  colname <- paste("medErr", d, sep = "")
  print(errors_alphabeta[[colname]])
  ggplot(errors_alphabeta, aes_string(x="alpha", y=colname, col="beta.as.factor")) + geom_line() + ylab("Mean Abs. Error log(predicted/actual)") +
    xlab("alpha") + scale_color_discrete("beta")
  ggsave("%s_alpha_beta.pdf" %>% sprintf(colname), width=7.98, height = 5.64)
}
