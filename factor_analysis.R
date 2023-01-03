library(tidyverse)
library(tidyr)
library(ggpubr)
library(rstatix)
library(dplyr)
library(PairedData)
library(gtools)

conduct_wilcox <- function(data, f, ...) {
  # args <- list(...)
  # print(class(args[1]))
  # groups <- as.character(args)
  # print(groups)
  # groups <- paste0(collapse = "-")
  # fname <- str_interp("${f}_GR-${groups}")
  # print(fname)
  temp <- data %>%
    group_by(...) %>%
    wilcox_test(f) %>%
    adjust_pvalue(method = "holm") %>%
    add_significance() %>%
    add_y_position(step=0.25)
  return(temp)
}

conduct_wilcox_nogr <- function(data, f) {
  temp <- data %>%
    wilcox_test(f) %>%
    adjust_pvalue(method = "holm") %>%
    add_significance() %>%
    add_y_position(step=0.25)
  return(temp)
}


setwd('/Volumes/Samsung_T5/Lab/Work/NEUROMYTHS_2022-apr/')
df_all <- read.csv('data/data_processed/myths-removed35-added-gen-int-src.csv')

df_all <- df_all %>% pivot_wider(id_cols = c(
  Subject,
  Education,
  Specialization,
  Gender,
  Age,
  Reliable.source,
  Interest,
  Question
), names_from = c(Answer))

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
write.csv(df_all, "data/data_processed/NEUROMYTHS_ALL_LABELS@1.1.csv", row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Gender, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Gender_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Specialization, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Specialization_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Unsure ~ Specialization, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Specialization_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Specialization, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Specialization_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Unsure ~ Specialization, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Specialization_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Education, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Education_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Unsure ~ Education, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Education_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

###

stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

###

stat.test <- conduct_wilcox(df_all, Unsure ~ Interest, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Interest_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Interest, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Interest_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Interest, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Interest_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Interest, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Interest_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Interest, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Interest_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Interest, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Interest_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

###

stat.test <- conduct_wilcox(df_all, Unsure ~ Education, Question, Reliable.source) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Education) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Education@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question, Education, Specialization) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question-Education-Specialization@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)

###

stat.test <- conduct_wilcox(df_all, Unsure ~ Reliable.source, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Unsure~Reliable.source_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
stat.test <- conduct_wilcox(df_all, Correct ~ Reliable.source, Question) %>%
  apply(2,as.character)
fname <- "data/data_processed/stat_results/Correct~Reliable.source_GR-Question@OUTLIERS_REMOVED@1.1.csv"
write.csv(stat.test, fname, row.names=FALSE)
