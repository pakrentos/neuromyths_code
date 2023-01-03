library(tidyverse)
library(tidyr)
library(ggpubr)
library(rstatix)
library(dplyr)
library(PairedData)
library(gtools)


setwd('/Volumes/Samsung_T5/Lab/Work/NEUROMYTHS_2022-apr/')
# df <- read.csv('myths.csv')
# df_2 <- read.csv('myths_typ2.csv')
# df_all <- read.csv('myths_all_labels.csv')
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

add_new_column <- function(temp) {
  new <- remove_outliers(temp$value)
  return(cbind(new,temp))
}

df <- df %>%
  group_by(Specialization, Ed, Question) %>%
  nest() %>%
  mutate(data = map(data, add_new_column)) %>%
  unnest_legacy()

# summary statistics
df_sum <- df_all %>%
  group_by(Label, Gender) %>%
  get_summary_stats(Correct, type = "mean_sd")

# boxplot
bxp <- df %>% ggboxplot(
  x = "Ed", y = "Correct",
  color = "Specialization", palette = "jco"
)
bxp





# normality test
df %>% filter(Question == "Myths") %>% ggqqplot("Correct", ggtheme = theme_bw()) +
  facet_grid(Specialization ~ Ed, labeller = "label_both")

norm <- df_all %>%
  group_by(Label, Ed, Question) %>%
  ks.test()
  shapiro_test(Correct)

# variance homogenity
hom <- df_all %>%
  group_by(Question, Answer) %>%
  levene_test(value ~ Label)



res.aov <- df %>%
  anova_test( dv = Correct, wid=Subject, between = c(Specialization, Ed)
  )

get_anova_table(res.aov)

df <- df_all

stat.test <- df %>%
  group_by(Question) %>%
  wilcox_test(Correct ~ Education) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")

a <- ggboxplot(df, x = "Education", y = "Correct", palette = "jco", facet.by = c("Question"))+
  stat_pvalue_manual(stat.test, label = "p.adj") +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

stat.test <- df %>%
  group_by(Question) %>%
  wilcox_test(Correct ~ Group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")

# facet.by = c("Question")
b <- ggboxplot(df, x = "Group", y = "Correct", palette = "jco", facet.by = c("Question"))+
  stat_pvalue_manual(stat.test, label = "p.adj") +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

stat.test <- df %>%
  group_by(Question) %>%
  wilcox_test(Unsure ~ Education) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")

c <- ggboxplot(df, x = "Education", y = "Unsure", palette = "jco", facet.by = c("Question"))+
  stat_pvalue_manual(stat.test, label = "p.adj") +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

stat.test <- df %>%
  group_by(Question) %>%
  wilcox_test(Unsure ~ Group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")

d <- ggboxplot(df, x = "Group", y = "Unsure", palette = "jco", facet.by = c("Question"))+
  stat_pvalue_manual(stat.test, label = "p.adj")+
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

figure <- ggarrange(a, b, c, d,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure

stat.test <- df %>%
  group_by(Question, Education) %>%
  wilcox_test(Correct ~ Group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")
stat.test

a <- ggboxplot(df, x = "Group", y = "Correct", palette = "jco", facet.by = c("Question", "Education"))+
  stat_pvalue_manual(stat.test, label = "p.adj") +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

stat.test <- df %>%
  group_by(Question, Group) %>%
  wilcox_test(Correct ~ Education) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")
# stat.test

b <- ggboxplot(
    df,
    x = "Education",
    y = "Correct",
    palette = "jco",
    facet.by = c("Question", "Group")
  )+
  stat_pvalue_manual(stat.test, label = "p.adj", bracket.nudge.y = -0.2) +
  # font("x.text", size = 11) +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

  figure <- ggarrange(
            a, b,
            labels = c("A", "B"),
            ncol = 1, nrow = 2,
            font.label=list(color="black",size=16)
          )
  # theme(text = element_text(size = 16))

figure

# stat.test <- df_all %>%
#   group_by(Question, Education) %>%
#   wilcox_test(Unsure ~ Gender) %>%
#   adjust_pvalue(method = "holm") %>%
#   add_significance() %>%
#   add_y_position(step=0.25) %>%
#   filter(p.adj.signif != "ns")
#
# ggboxplot(df_all, x = "Gender", y = "Unsure", palette = "jco", facet.by = c("Question", "Group"))+
#   stat_pvalue_manual(stat.test, label = "p.adj") +
#   theme(
#     strip.text.x = element_text(size = 15),
#     strip.text.y = element_text(size = 15),
#   )



stat.test <- df_all %>%
  group_by(Question, Education) %>%
  wilcox_test(Unsure ~ Reliable.source) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")
# stat.test

a <- ggboxplot(df_all, x = "Reliable.source", y = "Unsure", palette = "jco", facet.by = c("Question", "Education"))+
  stat_pvalue_manual(stat.test, label = "p.adj") +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

stat.test <- df_all %>%
  group_by(Question, Specialization) %>%
  wilcox_test(Unsure ~ Reliable.source) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance() %>%
  add_y_position(step=0.25) %>%
  filter(p.adj.signif != "ns")
# stat.test

b <- ggboxplot(
  df_all,
  x = "Reliable.source",
  y = "Unsure",
  palette = "jco",
  facet.by = c("Question", "Specialization")
)+
  stat_pvalue_manual(stat.test, label = "p.adj", bracket.nudge.y = -0.2) +
  # font("x.text", size = 11) +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
  )

figure <- ggarrange(
  a, b,
  labels = c("A", "B"),
  ncol = 1, nrow = 2,
  font.label=list(color="black",size=16)
)
# theme(text = element_text(size = 16))

figure

#
#
#
#
#

clean_df %>% group_by(Ed, Question) %>%
  wilcox_effsize(True ~ Teacher)


# pwc <- pwc %>% add_xy_position(x = "Ed")
bxp +
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# comparisons <- function(data_vec) {
#   unique(data_vec) %>%
#   combinations(length(.), 2, .) %>%
#   transpose()
# }


my_comparisons <- list( c("Педагог", "Не педагог"), c("Педагог", "Психолог"), c("Психолог", "Не педагог"))
