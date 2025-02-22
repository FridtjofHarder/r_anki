# ToDo:
# Introduction
# Methods
# Results
# Discussion
# Descriptive Stats
# Regression, controlling for factors?
#
#
#
#
#
#

# install all required packages ------------------------------------------------------------
required_packages <- c("gt", "readxl","tidyverse", "grDevices", "ggpubr", "effsize", "superb", "nlme", "reshape2",
                       "svglite")
installed_packages <- required_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(required_packages[!installed_packages])
}
invisible(lapply(required_packages, library, character.only = TRUE))

survey_data <- tibble(read_xlsx("data/processed/anki_data_comprehensive.xlsx")) # read survey data with all five groups
names(survey_data)[names(survey_data) == "group"] <- "exam" # rename "group" to "exam" to avoid collisions in ggplot2


# create df structure for descriptive stats.  -----------------------------
# Age is not yet analysed. Order of groups is: seminar 2022, lecture 2022, seminar 2022/23, lecture 2022/23, seminar 2023

students_per_group <- c(196, 107, 111, 266, 232) # number of students who wrote the exam in each group (excluding students absent from the exam). Data taken from score reports for each exam.
groups <- unique(survey_data$exam) # vector of different groups

respondents <- table(survey_data$exam)[groups] # create table of group frequencies and sort them by vector "groups". Otherwise, columns will be sorted by alphabet

response_rate <-  round(100*respondents/students_per_group, 2) # create table of response rates in percentages

gender_table <- table(survey_data$exam, survey_data$gender)[groups,] # create table of declared gender per group (1 = male, 2 = female, 3 = diverse or not declared)
gender_undeclared <- respondents - rowSums(gender_table) # get no of students who did not declare gender in each group
gender_table[,3] <- gender_table[,3] + gender_undeclared # add no of undeclared genders per group to gender table. row "3" now comprises both students of answered diverse, or who did not answer the question at all.

score_percentages_means <- aggregate(score_percentage ~ exam, data=survey_data, mean) # create df of mean score percentages by group
score_percentages_means_sorted <- score_percentages_means[match(groups, score_percentages_means$exam), ]

score_percentages_sds <- aggregate(score_percentage ~ exam, data=survey_data, sd) # create df of score percentage standard deviations by group
score_percentages_sds_sorted <- score_percentages_sds[match(groups, score_percentages_sds$exam), ]

score_percentages_means_sds <- paste(round(score_percentages_means_sorted[,2], 2), "\u00B1", round(score_percentages_sds_sorted[,2], 2)) # paste means +/- sds after rounding to 2 digits

descriptive_df <- data.frame(respondents = as.vector(respondents), # create data frame with all important descriptive data
                                 response_rate = as.vector(response_rate),
                                 score_percentages_means = score_percentages_means_sorted[, 2],
                                 score_percentages_sds = score_percentages_sds_sorted[, 2],
                                 as.data.frame.matrix(gender_table))

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
  locations = cells_body(columns = "Totals")
)

descriptive_tbl_gt <- tab_style( # make column name "Totals" in bold as well
  descriptive_tbl_gt,
  style = cell_text(weight = "bold"),
  locations = cells_column_labels(columns = Totals)
)

descriptive_tbl_gt

gtsave(descriptive_tbl_gt, "output/descriptive_stats.pdf")

# line graph of used methods. ------------------------------------------------------------

vector_of_methods <- c("used_script_digital", "used_script_physical", "used_textbook", "used_guideline",
                           "used_anki_institute", "used_anki_custom") # declare variables for line graph
#options ignored: moodle quiz, moodle task, other
subset_of_interest <- survey_data[c("exam", vector_of_methods)] # relevant subset of survey_data df

df_used_materials <- as.data.frame(matrix(nrow = length(groups), ncol = length(vector_of_methods)))
colnames(df_used_materials) <- vector_of_methods
rownames(df_used_materials) <- groups # creates df with columns for used studying methods and rows for groups

for (i in 1:length(vector_of_methods)){ # fill df with shares of material used by group
  method <- vector_of_methods[i]
  share_used_method <- aggregate(get(vector_of_methods[i]) ~ exam, data=subset_of_interest, mean, na.rm = TRUE) # gives share of students who used physical scripts per group and in total
  share_used_method <- share_used_method[match(groups, share_used_method$exam),] # sort by group
  df_used_materials[,i] <- share_used_method[, 2]
}

# convert data frame to long format to be usable in ggplot
df_used_materials <- cbind(exam = rownames(df_used_materials), df_used_materials) # create rownames as expressive data column

# rename methods for aesthetic plot labels
colnames(df_used_materials) <- c(
  "exam", "Digital script", "Physical script", "Textbooks", "Guidelines", "Anki institute cards", "Anki own cards")

df_used_materials_reshaped <- reshape(data = df_used_materials,
        direction = "long",
        varying = list(names(df_used_materials)[-1]),
        idvar = names(df_used_materials)[1],
        times = names(df_used_materials)[-1],
        timevar = "method_used",
        v.names = "share")

#plot
#define colors
cols <- c("Digital script" = "red", "Physical script" = "blue",
          "Textbooks" = "green","Guidelines" = "yellow", 
          "Anki institute cards" = "black", "Anki own cards" = "orange")
#define shape
shapes <- c("Digital script" = 15, "Physical script" = 16,
            "Textbooks" = 17,"Guidelines" = 18, 
            "Anki institute cards" = 8, "Anki own cards" = 3)
#define linetype
linetypes <- c("Digital script" = "solid", "Physical script" = "dashed",
            "Textbooks" = "dotted","Guidelines" = "dotdash", 
            "Anki institute cards" = "longdash", "Anki own cards" = "twodash")
  
figure <- ggplot(data = df_used_materials_reshaped, 
                 aes(x = exam, y = share, group = method_used, colour = method_used))
figure + geom_point(aes(shape = method_used), size = 3) +

<<<<<<< HEAD
  geom_line(aes(linetype = method_used), linewidth = 2, ) +
=======
  geom_line(aes(linetype = method_used), linewidth = 1, ) +
>>>>>>> 4f9ec581cb8c8cbe19b149ebdce3e946fa5a5617
  scale_x_discrete(limits = groups, name = "Exam", 
                   labels = c("Seminar 2022", "Lecture 2022", "Seminar 2022/23", "Lecture 2022/23", "Seminar 2023")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) +
  theme_bw() +labs(title = "Methods used by group",
                   color = "Methods used", 
                   shape = "Methods used", 
                   linetype = "Methods used",
                   x = "Exam",
<<<<<<< HEAD
                   y = "Share [%]")

ggsave("output/plot_used_methods.svg")
=======
                   y = "Share") + 
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes)
>>>>>>> 4f9ec581cb8c8cbe19b149ebdce3e946fa5a5617

# line graph of helpful. ------------------------------------------------------------

# vector_of_methods <- c("used_script_digital", "used_script_physical", "used_textbook", "used_guideline",
# "used_anki_institute", "used_anki_custom")

vector_of_helpful_methods <- c("helpful_script", "helpful_video", "helpful_streaming", "helpful_contact_event",
                                   "helpful_anki_institute", "helpful_anki_custom", "helpful_all_equally")
#options ignored: moodle quiz, moodle task
  
subset_of_interest_helpful <- survey_data[c("exam", vector_of_helpful_methods)]  # relevant subset of survey_data df

df_helpful_materials <- as.data.frame(matrix(nrow = length(groups), ncol = length(vector_of_helpful_methods)))
colnames(df_helpful_materials) <- vector_of_helpful_methods
rownames(df_helpful_materials) <- groups # creates df with columns for used studying methods and rows for groups
 
for (i in 1:length(vector_of_helpful_methods)){ # fill df with shares of material used by group
  method <- vector_of_helpful_methods[i]
  share_helpful_method <- aggregate(get(vector_of_helpful_methods[i]) ~ exam, data=subset_of_interest_helpful, mean, na.rm = TRUE) # gives share of students who used physical scripts per group and in total
  share_helpful_method <- share_helpful_method[match(groups, share_helpful_method$exam),] # sort by group
  df_helpful_materials[,i] <- share_helpful_method[, 2]
}

# convert data frame to long format to be usable in ggplot
df_helpful_materials <- cbind(exam = rownames(df_helpful_materials), df_helpful_materials) # create rownames as expressive data column

# rename methods for aesthetic plot labels
colnames(df_helpful_materials) <- c("exam", "Scripts", "Videos of lectures and seminars", 
                                    "Live-streamed casts", "Contact events", "Anki institute cards", "Anki own cards", 
                                    "All equally useful")

df_helpful_materials_reshaped <- reshape(data = df_helpful_materials,
                                      direction = "long",
                                      varying = list(names(df_helpful_materials)[-1]),
                                      idvar = names(df_helpful_materials)[1],
                                      times = names(df_helpful_materials)[-1],
                                      timevar = "method_helpful",
                                      v.names = "share")

df_helpful_materials_reshaped[is.na(df_helpful_materials_reshaped)] <- 0 # just for testing, delete when done

#define colors
cols <- c("Scripts" = "red", "Videos of lectures and seminars" = "blue",
          "Live-streamed casts" = "green","Contact events" = "yellow", 
          "Anki institute cards" = "black", "Anki own cards" = "orange",
          "All equally useful" = "cyan")
#define shape
shapes <- c("Scripts" = 15, "Videos of lectures and seminars" = 16,
            "Live-streamed casts" = 17,"Contact events" = 18, 
            "Anki institute cards" = 8, "Anki own cards" = 3,
            "All equally useful" = 3)
#define linetype
linetypes <- c("Scripts" = "solid", "Videos of lectures and seminars" = "dashed",
               "Live-streamed casts" = "dotted","Contact events" = "dotdash", 
               "Anki institute cards" = "longdash", "Anki own cards" = "twodash",
               "All equally useful" = 11)

figure <- ggplot(data = df_helpful_materials_reshaped, 
                 aes(x = exam, y = share, group = method_helpful, colour = method_helpful))
<<<<<<< HEAD
figure + geom_point(aes(shape = method_helpful), size = 3) +
  
  geom_line(aes(linetype = method_helpful), linewidth = 2, ) +
  scale_x_discrete(limits = groups, name = "Exam", 
                   labels = c("Seminar 2022", "Lecture 2022", "Seminar 2022/23", "Lecture 2022/23", "Seminar 2023")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) +
 
=======
figure + geom_point(aes(shape = method_helpful), size = 3, na.rm = TRUE) +
  geom_line(aes(linetype = method_helpful), linewidth = 1, ) +
  scale_x_discrete(limits = groups, name = "Exam", 
                   labels = c("Seminar 2022", "Lecture 2022", "Seminar 2022/23", "Lecture 2022/23", "Seminar 2023")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
>>>>>>> 4f9ec581cb8c8cbe19b149ebdce3e946fa5a5617
  theme_bw() +labs(title = "Methods considered useful by group",
                   color = "Methods considered helpful", 
                   shape = "Methods considered helpful", 
                   linetype = "Methods considered helpful",
                   x = "Exam",
                   y = "Share") + 
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes)



ggsave("output/plot_helpful_methods.svg")

# Anki considered helpful as percentage of users-------------------

# count all who responded positively to Q5 and Q6

count_anki_helpful_and_used_general <- rowsum(as.numeric(survey_data$used_anki_institute_general == 1 & 
                                                           survey_data$helpful_anki_institute_general == 1), 
       group = factor(survey_data$exam, levels = unique(survey_data$exam)), na.rm = T)

count_anki_not_helpful_and_used_general <- rowsum(as.numeric(survey_data$used_anki_institute_general == 1 & 
                                                           survey_data$helpful_anki_institute_general == 2), 
                                              group = factor(survey_data$exam, levels = unique(survey_data$exam)), na.rm = T)

count_anki_used_but_helpful_unknown <- rowsum(as.numeric(survey_data$used_anki_institute_general == 1 & 
                                                           is.na(survey_data$helpful_anki_institute_general)), 
                                              group = factor(survey_data$exam, levels = unique(survey_data$exam)), na.rm = T)

count_used_anki <- rowsum(as.numeric(survey_data$used_anki_institute_general == 1), 
                          group = factor(survey_data$exam, levels = unique(survey_data$exam)), na.rm = T)

df_anki_helpful <- data.frame("Considered_helpful" = count_anki_helpful_and_used_general,
                              "Not_considered_helpful" = count_anki_not_helpful_and_used_general,
                              "No_answer" = count_anki_used_but_helpful_unknown,
                              "total" = count_used_anki)

# convert to shares
df_anki_helpful_shares <- df_anki_helpful[-ncol(df_anki_helpful)]/df_anki_helpful$total

colnames(df_anki_helpful_shares) <-  c("Considered helpful", 
                                       "Not considered helpful",
                                       "No answer")

df_filtered <- df_anki_helpful_shares[-1, , drop = FALSE]
df_filtered$Group <- rownames(df_filtered)
df_long <- data.frame(
  Group = rep(df_filtered$Group, times = 3),
  Response = rep(colnames(df_filtered)[1:3], each = nrow(df_filtered)), 
  Share = c(df_filtered$"Considered helpful", df_filtered$"Not considered helpful", df_filtered$"No answer")
)
##
df_long$Response <- factor(df_long$Response, levels = c(
  "No answer",       # Bottom
  "Not considered helpful",   # Middle
  "Considered helpful"                 # Top
))

ggplot(df_long, aes(x = Group, y = Share, fill = Response)) +
  geom_col() +  # Stacked bars
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0, 0),
                     breaks = seq(0, 1, 0.2)) +  # Convert to %
  scale_x_discrete(limits = groups[-1], name = "Exam", 
                   labels = c("Lecture 2022", "Seminar 2022/23", "Lecture 2022/23", "Seminar 2023")) + 
  labs(title = "Acceptance of Anki institute deck among users", 
       x = "Exam", y = "Percentage", fill = "Response") +
  scale_fill_manual(values = c("gray95", "gray70", "gray20")) +
  theme_classic()

ggsave("output/plot_percentage_anki_helpful.svg")




ggsave("output/plot_methods.svg")

# boxplots of exam scores

boxplot_data <- survey_data[survey_data$exam != "seminar_22",]



ggplot(data = subset(boxplot_data, !is.na(used_anki_institute_general)),
       aes(x = exam, y=score_percentage, fill = factor(used_anki_institute_general)), na.rm = TRUE) + 
  geom_boxplot()
# create a data frame



variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

# grouped boxplot
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()

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

