# load packages -----------------------------------------------------------
required_packages <- c(
  "gt",
  "readxl",
  "rsvg",
  "ggplot2",
  "dplyr",
  "reshape2",
  "webshot2",
  "tibble"
)

installed_packages <- required_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(required_packages[!installed_packages])
}
invisible(lapply(required_packages, library, character.only = TRUE))

# load data --------------------------------------------------------------------
survey_data <- tibble(read_xlsx("data/processed/anki_data_comprehensive.xlsx")) # read survey data with all five groups

groups <- unique(survey_data$exam) # vector of different groups

students_per_group <- c(196, 107, 111, 266, 232) # number of students who wrote the exam in each group (excluding students absent from the exam). Data taken from score reports for each exam.
survey_data$exam <- factor(survey_data$exam, levels = groups) # transform to factor and preserve order

# z standardize scores
survey_data$z_score <- c(scale(survey_data$score[survey_data$exam == "seminar_22"]),
                         scale(survey_data$score[survey_data$exam == "lecture_22"]),
                         scale(survey_data$score[survey_data$exam == "seminar_22_23"]),
                         scale(survey_data$score[survey_data$exam == "lecture_22_23"]),
                         scale(survey_data$score[survey_data$exam == "seminar_23"]))

# factorize performance assessment
survey_data$performance_general_factor <- factor(
  survey_data$performance_general,
  levels = 1:5,
  labels = c("Far below","Below","Average","Above","Far above"),
  ordered = TRUE
)

# chi2 anki helpful -------------------------------------------------------

subset_anki_helpful <- subset(survey_data, subset = exam != "seminar_22")

count_anki_helpful_and_used <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute == 1 &
      subset_anki_helpful$helpful_anki_institute == 1
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_anki_not_helpful_and_used <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute == 1 &
      subset_anki_helpful$helpful_anki_institute == 0
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_anki_used_but_helpful_unknown <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute == 1 &
      is.na(subset_anki_helpful$helpful_anki_institute)
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_used_anki <- rowsum(
  as.numeric(subset_anki_helpful$used_anki_institute == 1),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

df_anki_helpful <- data.frame(
  "Considered_helpful" = count_anki_helpful_and_used,
  "Not_considered_helpful" = count_anki_not_helpful_and_used,
  "No_answer" = count_anki_used_but_helpful_unknown,
  "total" = count_used_anki
)

chisq.test(df_anki_helpful[, 1:2])

# self rated performance and exam score ----------------------------

subset_performance_scores <- subset(survey_data,
                                    subset = !is.na(survey_data$score) &
                                      survey_data$score > 0 &
                                      !is.na(survey_data$performance_pharm))

subset_performance_scores_red <- data.frame("z_score" = subset_performance_scores$z_score,
                                            "performance_pharm" = subset_performance_scores$performance_pharm,
                                            "exam" = subset_performance_scores$exam)

cor.test(
  subset_performance_scores_red$performance_pharm,
  subset_performance_scores_red$z_score,
  method = "spearman",
  exact = FALSE
)

# self-rated general performance and Anki usage --------------------------------

# correlation using entire data set
cor.test(survey_data$performance_general, survey_data$used_anki_institute, method = "spearman",
         exact = FALSE)

# correlation without seminar 2022
cor.test(survey_data$performance_general[survey_data$exam != "seminar_22"], 
         survey_data$used_anki_institute[survey_data$exam != "seminar_22"], method = "spearman",
         exact = FALSE)

# MWUT using entire data set
wilcox.test(survey_data$performance_general[survey_data$exam != "seminar_22"] ~ 
              survey_data$used_anki_institute[survey_data$exam != "seminar_22"])

# MWUT without seminar 2022
wilcox.test(survey_data$performance_general[survey_data$exam != "seminar_22"] ~ 
              survey_data$used_anki_institute[survey_data$exam != "seminar_22"])

# Anki usage and exam score ----------------------------------------------------

# full data set
t.test(survey_data$z_score[survey_data$used_anki_institute == 0], 
       survey_data$z_score[survey_data$used_anki_institute == 1])

# removed seminar_22
t.test(survey_data$z_score[survey_data$used_anki_institute == 0 &
                             survey_data$exam != "seminar_22"], 
       survey_data$z_score[survey_data$used_anki_institute == 1 &
         survey_data$exam != "seminar_22"])
# conclusion: removing seminar_22 improves stance

# reduced data set vs. controlled for self-assessment:
t.test(survey_data$z_score[survey_data$used_anki_institute == 0 & 
                             !is.na(survey_data$performance_general)], # uncontrolled
       survey_data$z_score[survey_data$used_anki_institute == 1 & 
                             !is.na(survey_data$performance_general)])

# compare with lm
result_unadjusted_restricted <- lm(z_score ~ used_anki_institute, data = 
                                   survey_data[!is.na(survey_data$performance_general), ])
summary(result_unadjusted_restricted)

# compare when restricting set to used_anki_institute == 1 || 0
result_unadjusted_restricted <- lm(z_score ~ used_anki_institute, data = 
                                     survey_data[!is.na(survey_data$performance_general) &
                                                   survey_data$used_anki_institute == 0 | 1, ])
summary(result_unadjusted_restricted)

result_adjusted_restricted <- lm(z_score ~ used_anki_institute + performance_general, data = 
                                   survey_data[!is.na(survey_data$performance_general), ])
summary(result_adjusted_restricted)

t.test(survey_data$z_score[survey_data$used_anki_institute == 0 & 
                             !is.na(survey_data$performance_general)], # controlled
       survey_data$z_score[survey_data$used_anki_institute == 1 & 
                             !is.na(survey_data$performance_general)])

# reduced data set vs. controlled for self-assessment without seminar_22
survey_data_no_s_22 <- survey_data[survey_data$exam != "seminar_22", ]
result_unadjusted_restricted_no_s_22 <- lm(z_score ~ used_anki_institute, data = 
                                     survey_data_no_s_22[!is.na(survey_data_no_s_22$performance_general) &
                                                           survey_data_no_s_22$used_anki_institute == 0 | 1, ])
summary(result_unadjusted_restricted_no_s_22)

result_adjusted_restricted_no_s_22 <- lm(z_score ~ used_anki_institute + performance_general, data = 
                                   survey_data_no_s_22[!is.na(survey_data_no_s_22$performance_general), ])
summary(result_adjusted_restricted_no_s_22)

result_adjusted_restricted_no_s_22 <- lm(z_score ~ used_anki_institute + performance_general, data = 
                                           survey_data_no_s_22)
summary(result_adjusted_restricted_no_s_22)


