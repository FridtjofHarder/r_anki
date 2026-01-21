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
survey_data <- subset(survey_data, exam != "seminar_22")

groups <- unique(survey_data$exam) # vector of different groups

students_per_group <- c(107, 111, 266, 232) # number of students who wrote the exam in each group (excluding students absent from the exam). Data taken from score reports for each exam.
survey_data$exam <- factor(survey_data$exam, levels = groups) # transform to factor and preserve order

# z standardize scores
survey_data$z_score <- c(scale(survey_data$score[survey_data$exam == "lecture_22"]),
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

# create new category seminar vs. lecture
survey_data$exam_type[grepl("lecture", survey_data$exam, fixed = TRUE)] <- "lecture"  
survey_data$exam_type[grepl("seminar", survey_data$exam, fixed = TRUE)] <- "seminar"

total_number <- sum(c(107, 111, 266, 232))
# considered helpful and used, seminar vs lecture ------------------------------

# used for exam seminar vs. lecture
table_anki_exam <- table(survey_data$used_anki_institute, survey_data$exam_type)
table_anki_exam_row_sums <- cbind(table_anki_exam, totals = rowSums(table_anki_exam))
table_anki_exam_all_sums <- rbind(table_anki_exam_row_sums, 
                                  totals = colSums(table_anki_exam_row_sums))
table_anki_exam_shares <- sweep(table_anki_exam_all_sums, 2, 
                                table_anki_exam_all_sums["totals", ], FUN = "/")

chisq.test(table_anki_exam)

count_anki_helpful_and_used <- rowsum(
  as.numeric(
    survey_data$used_anki_institute == 1 &
      survey_data$helpful_anki_institute == 1
  ),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

count_anki_not_helpful_and_used <- rowsum(
  as.numeric(
    survey_data$used_anki_institute == 1 &
      survey_data$helpful_anki_institute == 0
  ),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

count_used_anki <- rowsum(
  as.numeric(survey_data$used_anki_institute == 1),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

df_anki_helpful <- data.frame(
  "Considered_helpful" = count_anki_helpful_and_used,
  "Not_considered_helpful" = count_anki_not_helpful_and_used,
  "total" = count_used_anki
)

# difference between cohorts in share of anki exam users indicating Anki as "particularly useful"
chisq.test(df_anki_helpful[, 1:2])

# difference between seminar and lecture groups of anki exam users indicating Anki as "particularly useful"
df_anki_helpful_l_vs_s <- rbind(df_anki_helpful[1, ] + df_anki_helpful[3, ],
                                     df_anki_helpful[2, ] + df_anki_helpful[4, ])
chisq.test(df_anki_helpful_l_vs_s[, 1:2])

# repeat analyses above but with Anki in general

table_anki_exam_gen <- table(survey_data$used_anki_institute_general, survey_data$exam_type)
table_anki_exam_row_sums_gen <- cbind(table_anki_exam, totals = rowSums(table_anki_exam))
table_anki_exam_all_sums_gen <- rbind(table_anki_exam_row_sums, 
                                  totals = colSums(table_anki_exam_row_sums))
table_anki_exam_shares_gen <- sweep(table_anki_exam_all_sums, 2, 
                                table_anki_exam_all_sums["totals", ], FUN = "/")

chisq.test(table_anki_exam_gen)

count_anki_helpful_and_used_gen <- rowsum(
  as.numeric(
    survey_data$used_anki_institute_general == 1 &
      survey_data$helpful_anki_institute_general == 1
  ),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

count_anki_not_helpful_and_used_gen <- rowsum(
  as.numeric(
    survey_data$used_anki_institute_general == 1 &
      survey_data$helpful_anki_institute_general == 2
  ),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

count_used_anki_gen <- rowsum(
  as.numeric(survey_data$used_anki_institute_general == 1),
  group = factor(survey_data$exam, levels = unique(survey_data$exam)),
  na.rm = T
)

df_anki_helpful_gen <- data.frame(
  "Considered_helpful" = count_anki_helpful_and_used_gen,
  "Not_considered_helpful" = count_anki_not_helpful_and_used_gen,
  "total" = count_used_anki_gen
)

chisq.test(df_anki_helpful_gen[1:4, 1:2])

df_anki_helpful_gen_l_vs_s <- rbind(df_anki_helpful_gen[1, ] + df_anki_helpful_gen[3, ],
                                df_anki_helpful_gen[2, ] + df_anki_helpful_gen[4, ])
chisq.test(df_anki_helpful_gen_l_vs_s[, 1:2])


# difficulty exam vs. lecture -------------------------------------------------

t.test(survey_data$score_percentage[grepl("lecture", survey_data$exam, fixed = TRUE)],
       survey_data$score_percentage[grepl("seminar", survey_data$exam, fixed = TRUE)])
mean(survey_data$score_percentage, na.rm = TRUE)

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

# correlation
cor.test(survey_data$performance_general[survey_data$exam != "seminar_22"], 
         survey_data$used_anki_institute[survey_data$exam != "seminar_22"], method = "spearman",
         exact = FALSE) # same as used_anki_institute_general

# MWUT
wilcox.test(survey_data$performance_general[survey_data$exam != "seminar_22"] ~ 
              survey_data$used_anki_institute_general[survey_data$exam != "seminar_22"])

# Anki usage and exam score ----------------------------------------------------

# full data set
t.test(survey_data$z_score[survey_data$used_anki_institute == 0], 
       survey_data$z_score[survey_data$used_anki_institute == 1])
# same as used_anki_institute_general

# conclusion: removing seminar_22 improves stance

# reduced data set vs. controlled for self-assessment:
t.test(survey_data$z_score[survey_data$used_anki_institute == 0 & 
                             !is.na(survey_data$performance_general)], # uncontrolled
       survey_data$z_score[survey_data$used_anki_institute == 1 & 
                             !is.na(survey_data$performance_general)])
# same as used_anki_institute_general

# compare when restricting set to used_anki_institute == 1 || 0
result_unadjusted_restricted <- lm(z_score ~ used_anki_institute, data = 
                                     survey_data[!is.na(survey_data$performance_general) &
                                                   survey_data$used_anki_institute == 0 | 1, ])
summary(result_unadjusted_restricted) # same as used_anki_institute_general

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

# study frequency --------------------------------------------------------------
table_frequency <- table(survey_data$anki_time_spent, survey_data$exam)
chisq.test(table_frequency)

table_frequency_exam_type <- table(survey_data$anki_time_spent, survey_data$exam_type)

# transform for Wilcox test
df <- data.frame(
  study_freq = rep(rep(1:3, 2), times = c(table_frequency_exam_type)),
  exam_type  = rep(c("lecture", "seminar"), each = nrow(table_frequency_exam_type), 
                   times = c(table_frequency_exam_type))
)

wilcox.test(study_freq ~ exam_type, data = df)

# gender and age with exam score and anki usage
age <- lm(z_score ~ age, data = survey_data)
summary(age)

table_gender <- table(survey_data$gender, survey_data$used_anki_institute)
chisq.test(table_gender[1:2, 1:2]) # no significant impact of gender on anki usage






