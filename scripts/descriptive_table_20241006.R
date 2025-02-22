####### install all required packages
required_packages <- c("gt", "readxl","tidyverse", "grDevices", "ggpubr", "effsize", "superb", "nlme","webshot2",
                       "htmltools")
installed_packages <- required_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(required_packages[!installed_packages])
}
invisible(lapply(required_packages, library, character.only = TRUE))

survey_data <- tibble(read_xlsx("data/processed/anki_data_comprehensive.xlsx")) # read survey data with all five groups


# create df structure for descriptive stats.  -----------------------------
# Age is not yet analysed. Order of groups is: seminar 2022, lecture 2022, seminar 2022/23, lecture 2022/23, seminar 2023

students_per_group <- c(196, 107, 111, 266, 232) # number of students who wrote the exam in each group (excluding students absent from the exam). Data taken from score reports for each exam.
groups <- unique(survey_data$group) # vector of different groups

respondents <- table(survey_data$group)[groups] # create table of group frequencies and sort them by vector "groups". Otherwise, columns will be sorted by alphabet

response_rate <-  round(100*respondents/students_per_group, 2) # create table of response rates in percentages

gender_table <- table(survey_data$group, survey_data$gender)[groups,] # create table of declared gender per group (1 = male, 2 = female, 3 = diverse or not declared)
gender_undeclared <- respondents - rowSums(gender_table) # get no of students who did not declare gender in each group
gender_table[,3] <- gender_table[,3] + gender_undeclared # add no of undeclared genders per group to gender table. row "3" now comprises both students of answered diverse, or who did not answer the question at all.

score_percentages_means <- aggregate(score_percentage ~ group, data=survey_data, mean) # create df of mean score percentages by group
score_percentages_means_sorted <- score_percentages_means[match(groups, score_percentages_means$group), ]

score_percentages_sds <- aggregate(score_percentage ~ group, data=survey_data, sd) # create df of score percentage standard deviations by group
score_percentages_sds_sorted <- score_percentages_sds[match(groups, score_percentages_sds$group), ]

score_percentages_means_sds <- paste(round(score_percentages_means_sorted[,2], 2), "\u00B1", round(score_percentages_sds_sorted[,2], 2)) # paste means +/- sds after rounding to 2 digits

descriptive_df <- data.frame(respondents = as.vector(respondents), # create data frame with all important descriptive data
                                 response_rate = as.vector(response_rate),
                                 score_percentages_means = score_percentages_means_sorted[, 2],
                                 score_percentages_sds = score_percentages_sds_sorted[, 2],
                                 as.data.frame.matrix(gender_table))
# descriptive_matrix <- data.frame(cbind(respondents, response_rate = response_rate, gender_table, score_percentages_means_sds = score_percentages_means_sds)) # combine to table with all descriptive stats
# descriptive_matrix_df <- cbind.data.frame([respondents = respondents, response_rate = response_rate, gender_table, score_percentages_means_sds = score_percentages_means_sds], make.row.names = FALSE) # combine to table with all descriptive stats
# descriptive_df <- data.frame(descriptive_matrix) # convert matrix to df
# descriptive_df["totals", 1] <- sum(descriptive_df[, "respondents"])

descriptive_df["totals", ] <- c(sum(respondents), # add bottom row with totals over all seminar and lecture exams
                                100 * sum(respondents) / sum(students_per_group),
                                mean(survey_data$score_percentage, na.rm = TRUE),
                                sd(survey_data$score_percentage, na.rm = TRUE),
                                sum(survey_data$gender == 1, na.rm = TRUE),
                                sum(survey_data$gender == 2, na.rm = TRUE),
                                sum(respondents) - sum(survey_data$gender == 1, na.rm = TRUE) -
                                sum(survey_data$gender == 2, na.rm = TRUE)
)

score_percentages_means_sds <- paste(round(c(score_percentages_means_sorted[,2],mean(survey_data$score_percentage, na.rm = TRUE)), 2), 
                                     "\u00B1", 
                                     round(c(score_percentages_sds_sorted[,2],sd(survey_data$score_percentage, na.rm = TRUE)), 2)) # paste means +/- sds after rounding to 2 digits
descriptive_df_processed <- descriptive_df[,-c(3,4)] # percentage means and sds converted to char and combined as mean +- sd
descriptive_df_processed$means_and_sds <- score_percentages_means_sds
descriptive_df_processed$response_rate <- round(descriptive_df_processed$response_rate, 2)


# descriptive table of sample characteristics. ----------------------------

rownames(descriptive_df_processed) <- c("Seminar 2022", "Lecture 2022", "Seminar 2022/23", "Lecture 2022/23", "Seminar 2023", "Totals")

descriptive_tbl <- as_tibble(cbind(nms = names(descriptive_df_processed), t(descriptive_df_processed))) # transpose and convert to tibble object

row_names <- c("Respondents [n]", "Response rate [%]", "Male", "Female", "Non-binary or refused to disclose", "Mean percentage of correct anwers [%] \u00B1 standard deviation") # expressive row names esp. for gender options

descriptive_tbl$nms = row_names # fill first column in descriptive_tbl with row names

descriptive_tbl_gt <- gt(descriptive_tbl, rowname_col = "nms") # create gt object with $nms as row names

indented_rows <- c("Male", "Female", "Non-binary or refused to disclose") # define rows to be indented (gender rows)

descriptive_tbl_gt <- tab_row_group(descriptive_tbl_gt, label = "Gender",  rows = indented_rows) # create gt_tbl object, label Gender rows for grouping and indenting

descriptive_tbl_gt <- tab_stub_indent(descriptive_tbl_gt, rows= indented_rows, indent = 3) # indent Gender rows

descriptive_tbl_gt <- row_group_order(descriptive_tbl_gt, groups = c(NA, "Gender")) # reorder groups with Gender being on the bottom

tab_header(descriptive_tbl_gt, "Subject characteristics")

descriptive_tbl_gt <- tab_style( # make column total in bold
  descriptive_tbl_gt,
  style = cell_text(weight = "bold"),
  locations = cells_body(columns = Totals)
)

descriptive_tbl_gt <- tab_style( # make column name "Totals" in bold as well
  descriptive_tbl_gt,
  style = cell_text(weight = "bold"),
  locations = cells_column_labels(columns = Totals)
)

# save table as vectorized pdf

gtsave(descriptive_tbl_gt, "output/descriptive_stats.pdf")

# line graph. ------------------------------------------------------------

variables_of_interest <- c("group", "used_script_digital", "used_script_physical", "used_textbook", "used_guideline",
                           "used_anki_institute", "used_anki_custom") # declare variables for line graph
#options ignored: moodle quiz, moodle task, other
subset_of_interest <- survey_data[variables_of_interest]

share_used_script_digital <- aggregate(used_script_digital ~ group, data=subset_of_interest, mean, na.rm = TRUE)
share_used_script_digita_sorted <- share_used_script_digital[match(groups, share_used_script_digital$group),]

match(groups, share_used_script_digital$group)
 # gives share of students who used physical scripts per group and in total


frequency_tables <- frequency_table(survey_data, variables_of_interest) # create list of dataframes

list_of_percentages <- percentages(frequency_tables) # creates list of percentages for study material

percentages_of_study_material <- lapply(list_of_percentages,`[`,,"1") # unlist

df_of_percentages <- do.call(data.frame, percentages_of_study_material) # transform to df with study materials in cols and groups in rows
colnames(df_of_percentages) <- variables_of_interest # name cols after study material
df_of_percentages$groups <- row.names(df_of_percentages) # transform row names to column

df_reshaped <- reshape(df_of_percentages, dir = "long", idvar = "groups", varying = variables_of_interest, v.names = "percentages", 
                       times = variables_of_interest, timevar = "study_material")
relocate(df_reshaped, study_material)

df_reshaped$groups <- factor(df_reshaped$groups, levels = unique(df_reshaped$groups), ordered = T)

df_reshaped_for_plot <- df_reshaped # prepare for renaming

df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_script_digital"] <- "digital script" # rename for plot legend
df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_script_physical"] <- "physical script"
df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_textbook"] <- "textbook"
df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_guideline"] <- "guidelines"
df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_anki"] <- "institute Anki decks"
df_reshaped_for_plot$study_material[df_reshaped_for_plot$study_material == "used_custom_anki"] <- "Anki decks created by student"

df_reshaped_for_plot$study_material <- factor(df_reshaped_for_plot$study_material, 
                                              levels = unique(df_reshaped_for_plot$study_material), ordered = T)

figure <- ggplot(data=df_reshaped_for_plot, aes(x=as.factor(groups), y=percentages, group=study_material))

figure +
  geom_line(aes(linetype = study_material, color = study_material), linewidth = 2)+
  geom_point(aes(color = study_material), size = 2)+
  expand_limits(y = c(0, 100))+
  theme_bw()+
  labs(color  = "preferred study material", linetype = "preferred study material",
       x = "group", y = " Usage (% of students in each group)") +
  ggtitle("Preferred study material by group")
ggsave(path = "H:/R/figures", filename= "preferred_study_material.png", device='png', dpi=700)

######## material considered helpful

variables_of_interest_helpful <- c("helpful_script", "helpful_video", "helpful_streaming", "helpful_contact_event",
                                   "helpful_anki", "helpful_anki_custom", "helpful_all_equally")
#options ignored: moodle quiz, moodle task
frequency_tables_helpful <- frequency_table(survey_data, variables_of_interest_helpful)
list_of_percentages_helpful <- percentages(frequency_tables_helpful)

percentages_of_study_material_helpful <- lapply(list_of_percentages_helpful,`[`,,"1") # unlist

df_of_percentages_helpful <- do.call(data.frame, percentages_of_study_material_helpful) # transform to df with study materials in cols and groups in rows
colnames(df_of_percentages_helpful) <- variables_of_interest_helpful # name cols after study material
df_of_percentages_helpful$groups <- row.names(df_of_percentages_helpful) # transform row names to column

df_reshaped_helpful <- reshape(df_of_percentages_helpful, dir = "long", idvar = "groups", varying = variables_of_interest_helpful, v.names = "percentages", 
                       times = variables_of_interest_helpful, timevar = "study_material")
relocate(df_reshaped_helpful, study_material)

df_reshaped_helpful$groups <- factor(df_reshaped_helpful$groups, levels = unique(df_reshaped_helpful$groups), ordered = T)

df_reshaped_helpful_for_plot <- df_reshaped_helpful # prepare for renaming



df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_script"] <- "script" # rename for plot legend
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_video"] <- "video"
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_streaming"] <- "streaming"
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_contact_event"] <- "contact event"
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_anki_general"] <- "institute Anki decks"
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_anki_custom"] <- "Anki decks created by student"
df_reshaped_helpful_for_plot$study_material[df_reshaped_helpful_for_plot$study_material == "helpful_all_equally"] <- "all equally helpful"

df_reshaped_helpful_for_plot$study_material <- factor(df_reshaped_helpful_for_plot$study_material, 
                                              levels = unique(df_reshaped_helpful_for_plot$study_material), ordered = T)

figure <- ggplot(data=df_reshaped_helpful_for_plot, aes(x=as.factor(groups), y=percentages, group=study_material))

figure +
  geom_line(aes(linetype = study_material, color = study_material), linewidth = 2)+
  geom_point(aes(color = study_material), size = 2)+
  expand_limits(y = c(0, 100))+
  theme_bw()+
  labs(color  = "study material considered especially helpful", linetype = "study material considered especially helpful",
       x = "group", y = " considered especially helpful (% of students in each group)") +
  ggtitle("Study material considered especially helpful by group")

ggsave(path = "H:/R/figures", filename= "helpful_material.png", device='png', dpi=700)
####### Anki considered helpful as percentage of users


count_anki_helpful_and_used_general <- rowsum(as.numeric(survey_data$used_anki_general == 1 & 
                                                           survey_data$helpful_anki_general == 1), 
       group = factor(survey_data$group, levels = unique(survey_data$group)), na.rm = T)

count_anki_not_helpful_and_used_general <- rowsum(as.numeric(survey_data$used_anki_general == 1 & 
                                                           survey_data$helpful_anki_general == 2), 
                                              group = factor(survey_data$group, levels = unique(survey_data$group)), na.rm = T)

count_either_helpful_and_used_general <- count_anki_helpful_and_used_general+
  count_anki_not_helpful_and_used_general

percent_anki_helpful <- 100*count_anki_helpful_and_used_general/count_either_helpful_and_used_general

df_percent_anki_helpful <- data.frame(groups = factor(row.names(percent_anki_helpful), levels = row.names(percent_anki_helpful)), 
                                      percentage = percent_anki_helpful,
                                      row.names = NULL)
  
figure <- ggplot(data=df_percent_anki_helpful, aes(x=as.factor(groups), y=percentage)) + 
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(x= "group", y = "% of Anki users who considered the cards helpful") +
  ggtitle("Students who considered Anki cards helpful relative to number of users by group") +
  theme_bw()+
  expand_limits(y = c(NA, 100))
ggsave(path = "H:/R/figures", filename= "share_anki_helpful.png", device='png', dpi=700)

survey_data_boxplot <- survey_data
survey_data_boxplot$used_anki[survey_data_boxplot$used_anki == 0] <- "did not study with Anki"
survey_data_boxplot$used_anki[survey_data_boxplot$used_anki == 1] <- "studied with Anki" 

ggplot(data = survey_data_boxplot, aes(x=factor(group, levels = unique(group)), y=100*score_percentage, fill=as.factor(used_anki))) + 
  geom_boxplot() +
  labs(fill = " ", x = "group", y = "correct exam questions (%)", title = "% of correct exam questions depending on group and Anki usage") +
  geom_line() +
  theme_bw()
ggsave(path = "H:/R/figures", filename= "exam scores and Anki usage.png", device='png', dpi=700)

##### t-tests and multiple Regression
p_values_t_test <- rep(NA, length(unique(survey_data$group)))
mean_difference <- rep(NA, length(unique(survey_data$group)))

for(i in 1:length(unique(survey_data$group))){
  p_values_t_test[i] <-t.test(score_percentage ~ used_anki, data = survey_data[survey_data$group == groups[i],])$p.value
  mean_difference[i] <- diff(t.test(score_percentage ~ used_anki, data = survey_data[survey_data$group == groups[i],])$estimate)
}

p_values_tbble <- as_tibble(data.frame(groups, round(p_values_t_test, 2), round(100*mean_difference, 2)))

colnames(p_values_tbble) <-  c("group", "p-value", "mean difference")

ankitable <- gt(p_values_tbble)
tab_caption(ankitable, "Two-sample t-tests for exam score percentage differences between Anki users and non-users in each group, uncorrected for multiple comparisons. Negative difference indicates higher exam score in non-users.")

##### multilevel regression
intercept_only <- gls(score_percentage ~ 1, data = survey_data, method = "ML", na.action = na.exclude)
random_intercept_only <- lme(score_percentage ~ 1, data = survey_data, random = ~1|group, method = "ML", na.action = na.exclude)
anova(intercept_only, random_intercept_only)
random_intercept <- lme(score_percentage ~ used_anki, data = survey_data, random = ~1|group, method = "ML", na.action = na.exclude)
summary(random_intercept)
ctrl <- lmeControl(opt='optim')
random_slope <- lme(score_percentage ~ used_anki, data = survey_data, random = ~used_anki|group, method = "ML", control = ctrl, na.action = na.exclude)
anki_only <- gls(score_percentage ~ used_anki, data = survey_data, method = "ML", na.action = na.exclude)
multiple_regression <- lm(score_percentage ~ used_anki + group + used_anki*group, data = survey_data)
summary(multiple_regression)
group_regression <- lm(score_percentage ~ group, data = survey_data)
summary(group_regression)
anova_group <- aov(score_percentage~group, data = survey_data)
summary(anova_group)
anova_interaction <- aov(score_percentage ~ used_anki*group, data = survey_data)
summary(anova_interaction)


# Andy Field MLR
install.packages("car")
install.packages("nlme")
install.packages("reshape")
install.packages("discovr")
surgeryData <- Cosmetic.Surgery
surgeryLinearModel <- lm(Post_QoL ~ Surgery, data = surgeryData) 
summary(surgeryLinearModel)
summary(lm(formula = Post_QoL ~ Surgery + Base_QoL, data = surgeryData))

interceptOnly <-gls(Post_QoL ~ 1, data = surgeryData, method = "ML") 
summary(interceptOnly) 
randomlnterceptOnly <-lme(Post_QoL ~ 1, data = surgeryData, random = ~1|Clinic, method = "ML") 
summary(randomlnterceptOnly) 
RandomInterceptSurgery <- lme(Post_QoL ~ Surgery, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(RandomInterceptSurgery)
RandomInterceptSurgeryQoL <- lme(Post_QoL ~ Surgery + Base_QoL, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(RandomInterceptSurgeryQoL)
summary(RandomInterceptSurgery)
addRandomSlope <- lme(Post_QoL ~ Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)

#### reasons for Anki non-usage

reasons_not_used <- rowsum(subset(survey_data, select = not_used_no_time:not_used_other), 
       group = factor(survey_data$group, levels = unique(survey_data$group)), na.rm = T)
count_not_used <- rowsum(as.numeric(survey_data$used_anki_general == 2), 
                                                  group = factor(survey_data$group, levels = unique(survey_data$group)), na.rm = T)

percentage_reasons <- 100*reasons_not_used/count_not_used
percentage_reasons$groups <- row.names(percentage_reasons)

percentage_reasons_reshaped <- reshape(percentage_reasons, dir = "long", idvar = "groups", 
                                       varying = colnames(percentage_reasons)[1:length(colnames(percentage_reasons))-1], 
                                       v.names = "percentages", 
        times = colnames(percentage_reasons)[1:length(colnames(percentage_reasons))-1], timevar = "reasons")

### rephrase reasons
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_no_time"] <- "no time" # rename for plot legend
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_not_interested"] <- "not interested"
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_sufficient_material"] <- "other material is sufficient"
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_long_installation"] <- "installation too time-consuming"
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_unaware"] <- "unaware of the cards"
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_custom"] <- "used own/other cards instead"
percentage_reasons_reshaped$reasons[percentage_reasons_reshaped$reasons == "not_used_other"] <- "other reason given"

figure <- ggplot(percentage_reasons_reshaped, aes(x=factor(groups, levels = unique(groups)), y=percentages, group=reasons))

figure +
  geom_line(aes(linetype = reasons, color = reasons), linewidth = 2)+
  geom_point(aes(color = reasons), size = 2)+
  expand_limits(y = c(0, 100))+
  theme_bw()+
  # labs(color  = "preferred study material", linetype = "preferred study material",
  #      x = "group", y = " Usage (% of students in each group)") +
  labs(color  = "Reasons given", linetype = "Reasons given",
       x = "group", y = "Share of reason given (% of all Anki non-users)") +
  ggtitle("Reasons for not using Anki cards")

ggsave(path = "H:/R/figures", filename= "reason_not_used.png", device='png', dpi=700)

