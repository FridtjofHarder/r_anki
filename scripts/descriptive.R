#### install all required packages ---------------------------------------------
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

#### create df structure for descriptive stats.  -------------------------------
respondents <- table(survey_data$exam) # create table of group frequencies
response_rate <-  round(100 * respondents / students_per_group, 1) # create table of response rates in percentages

# create descriptive table -----------------------------------------------------

gender_table <- table(survey_data$exam, survey_data$gender) # create table of declared gender per group (1 = male, 2 = female, 3 = diverse or not declared)
gender_undeclared <- respondents - rowSums(gender_table) # get no of students who did not declare gender in each group
gender_table[, 3] <- gender_table[, 3] + gender_undeclared # add no of undeclared genders per group to gender table. row "3" now comprises both students of answered diverse, or who did not answer the question at all.

score_percentages_means <- aggregate(score_percentage ~ exam, data = survey_data, mean) # create df of mean score percentages by group

score_percentages_sds <- aggregate(score_percentage ~ exam, data = survey_data, sd) # create df of score percentage standard deviations by group

# paste means +/- sds after rounding to 2 digits for score percentages and add total
score_percentages_means_sds <- paste(round(c(
  score_percentages_means[, 2],
  mean(survey_data$score_percentage, na.rm = TRUE)
), 1), "\u00B1", round(c(
  score_percentages_sds[, 2],
  sd(survey_data$score_percentage, na.rm = TRUE)
), 1))

# calc mean and sd of age and sort it
age_means <- aggregate(age ~ exam, data = survey_data, mean)
age_sds <- aggregate(age ~ exam, data = survey_data, sd)

# paste means +/- sds after rounding to 2 digits for age and add total
age_means_sds <- paste(round(c(
  age_means[, 2], mean(survey_data$age, na.rm = TRUE)
), 1), "\u00B1", round(c(
  age_sds[, 2], sd(survey_data$age, na.rm = TRUE)
), 1))

# create complete data frame
descriptive_df <- data.frame(
  respondents = as.vector(respondents),
  # create data frame with all important descriptive data
  response_rate = as.vector(response_rate),
  as.data.frame.matrix(gender_table)
)

descriptive_df["totals", ] <- c(
  sum(respondents),
  # add bottom row with totals over all seminar and lecture exams
  round(100 * sum(respondents) / sum(students_per_group), 1),
  sum(survey_data$gender == 1, na.rm = TRUE),
  sum(survey_data$gender == 2, na.rm = TRUE),
  sum(respondents) - sum(survey_data$gender == 1, na.rm = TRUE) -
    sum(survey_data$gender == 2, na.rm = TRUE)
)

descriptive_df$score_means_and_sds <- score_percentages_means_sds
descriptive_df$age_means_and_sds <- age_means_sds

#### descriptive table of sample characteristics. ------------------------------------

rownames(descriptive_df) <- c(
  "Seminar 2022",
  "Lecture 2022",
  "Seminar 2022/23",
  "Lecture 2022/23",
  "Seminar 2023",
  "Totals"
)

descriptive_tbl <- as_tibble(cbind(nms = names(descriptive_df), t(descriptive_df))) # transpose and convert to tibble object

row_names <- c(
  "Respondents [n]",
  "Response rate [%]",
  "Male",
  "Female",
  "Non-binary or refused to disclose",
  "Mean percentage of correct anwers [%] \u00B1 standard deviation",
  "Mean age [years] \u00B1 standard deviation"
) # expressive row names esp. for gender options

descriptive_tbl$nms = row_names # fill first column in descriptive_tbl with row names

descriptive_tbl_gt <- gt(descriptive_tbl, rowname_col = "nms") # create gt object with $nms as row names

indented_rows <- c("Male", "Female", "Non-binary or refused to disclose") # define rows to be indented (gender rows)

descriptive_tbl_gt <- tab_row_group(descriptive_tbl_gt, label = "Gender", rows = indented_rows) # create gt_tbl object, label Gender rows for grouping and indenting

descriptive_tbl_gt <- tab_stub_indent(descriptive_tbl_gt, rows = indented_rows, indent = 3) # indent Gender rows

descriptive_tbl_gt <- row_group_order(descriptive_tbl_gt, groups = c(NA, "Gender")) # reorder groups with Gender being on the bottom

descriptive_tbl_gt <- tab_header(descriptive_tbl_gt, "Subject characteristics")

descriptive_tbl_gt <- tab_style(
  # make column total in bold
  descriptive_tbl_gt,
  style = cell_text(weight = "bold"),
  locations = cells_body(columns = "Totals")
)

descriptive_tbl_gt <- tab_style(
  # make column name "Totals" in bold as well
  descriptive_tbl_gt,
  style = cell_text(weight = "bold"),
  locations = cells_column_labels(columns = Totals)
)

gtsave(descriptive_tbl_gt, "output/descriptive_stats.pdf")

#### line graph of used methods. ------------------------------------------------------------

vector_of_methods <- c(
  "used_script_digital",
  "used_script_physical",
  "used_textbook",
  "used_guideline",
  "used_anki_institute",
  "used_anki_custom"
) # declare variables for line graph
#options ignored: moodle quiz, moodle task, other
subset_of_interest <- survey_data[c("exam", vector_of_methods)] # relevant subset of survey_data df

df_used_materials <- as.data.frame(matrix(nrow = length(groups), ncol = length(vector_of_methods)))
colnames(df_used_materials) <- vector_of_methods
rownames(df_used_materials) <- groups # creates df with columns for used studying methods and rows for groups

for (i in 1:length(vector_of_methods)) {
  # fill df with shares of material used by group
  method <- vector_of_methods[i]
  share_used_method <- aggregate(get(vector_of_methods[i]) ~ exam,
                                 data = subset_of_interest,
                                 mean,
                                 na.rm = TRUE) # gives share of students who used physical scripts per group and in total
  share_used_method <- share_used_method[match(groups, share_used_method$exam), ] # sort by group
  df_used_materials[, i] <- share_used_method[, 2]
}

# convert data frame to long format to be usable in ggplot
df_used_materials <- cbind(exam = rownames(df_used_materials), df_used_materials) # create rownames as expressive data column

# rename methods for aesthetic plot labels
colnames(df_used_materials) <- c(
  "exam",
  "Digital script",
  "Physical script",
  "Textbooks",
  "Guidelines",
  "Anki (institute cards)",
  "Anki (own cards)"
)

df_used_materials_reshaped <- reshape(
  data = df_used_materials,
  direction = "long",
  varying = list(names(df_used_materials)[-1]),
  idvar = names(df_used_materials)[1],
  times = names(df_used_materials)[-1],
  timevar = "method_used",
  v.names = "share"
)

#plot
#define colors
cols <- c(
  "Digital script" = "red",
  "Physical script" = "blue",
  "Textbooks" = "green",
  "Guidelines" = "yellow",
  "Anki (institute cards)" = "black",
  "Anki (own cards)" = "orange"
)
#define shape
shapes <- c(
  "Digital script" = 15,
  "Physical script" = 16,
  "Textbooks" = 17,
  "Guidelines" = 18,
  "Anki (institute cards)" = 8,
  "Anki (own cards)" = 3
)
#define linetype
linetypes <- c(
  "Digital script" = "solid",
  "Physical script" = "dashed",
  "Textbooks" = "dotted",
  "Guidelines" = "dotdash",
  "Anki (institute cards)" = "longdash",
  "Anki (own cards)" = "twodash"
)

figure <- ggplot(data = df_used_materials_reshaped,
                 aes(
                   x = exam,
                   y = share,
                   group = method_used,
                   colour = method_used
                 ))
figure + geom_point(aes(shape = method_used), size = 3) +
  
  geom_line(aes(linetype = method_used), linewidth = 2, ) +
  
  scale_x_discrete(
    limits = groups,
    name = "Exam",
    labels = c(
      "Seminar 2022",
      "Lecture 2022",
      "Seminar 2022/23",
      "Lecture 2022/23",
      "Seminar 2023"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  theme_bw() + labs(
    title = "Methods used for exam preparation",
    color = "Methods used",
    shape = "Methods used",
    linetype = "Methods used",
    x = "Exam",
    y = "Share per cohort"
  ) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes)

ggsave("output/plot_used_methods.svg",
       width = 10,
       height = 5) # decomment when you want to save

#### line graph of helpful methods. ------------------------------------------------------------

vector_of_helpful_methods <- c(
  "helpful_script",
  "helpful_video",
  "helpful_streaming",
  "helpful_contact_event",
  "helpful_anki_institute",
  "helpful_anki_custom",
  "helpful_all_equally"
)
#options ignored: moodle quiz, moodle task

subset_of_interest_helpful <- survey_data[c("exam", vector_of_helpful_methods)]  # relevant subset of survey_data df

df_helpful_materials <- as.data.frame(matrix(
  nrow = length(groups),
  ncol = length(vector_of_helpful_methods)
))
colnames(df_helpful_materials) <- vector_of_helpful_methods
rownames(df_helpful_materials) <- groups # creates df with columns for used studying methods and rows for groups

for (i in 1:length(vector_of_helpful_methods)) {
  # fill df with shares of material used by group
  method <- vector_of_helpful_methods[i]
  share_helpful_method <- aggregate(get(vector_of_helpful_methods[i]) ~ exam,
                                    data = subset_of_interest_helpful,
                                    mean,
                                    na.rm = TRUE) # gives share of students who used physical scripts per group and in total
  share_helpful_method <- share_helpful_method[match(groups, share_helpful_method$exam), ] # sort by group
  df_helpful_materials[, i] <- share_helpful_method[, 2]
}

# convert data frame to long format to be usable in ggplot
df_helpful_materials <- cbind(exam = rownames(df_helpful_materials), df_helpful_materials) # create rownames as expressive data column

# rename methods for aesthetic plot labels
colnames(df_helpful_materials) <- c(
  "exam",
  "Scripts",
  "Videos of lectures and seminars",
  "Live-streamed casts",
  "Contact events",
  "Anki (institute cards)",
  "Anki (own cards)",
  "All equally useful"
)

df_helpful_materials_reshaped <- reshape(
  data = df_helpful_materials,
  direction = "long",
  varying = list(names(df_helpful_materials)[-1]),
  idvar = names(df_helpful_materials)[1],
  times = names(df_helpful_materials)[-1],
  timevar = "method_helpful",
  v.names = "share"
)

#define colors
cols <- c(
  "Scripts" = "red",
  "Videos of lectures and seminars" = "blue",
  "Live-streamed casts" = "green",
  "Contact events" = "yellow",
  "Anki (institute cards)" = "black",
  "Anki (own cards)" = "orange",
  "All equally useful" = "cyan"
)
#define shape
shapes <- c(
  "Scripts" = 15,
  "Videos of lectures and seminars" = 16,
  "Live-streamed casts" = 17,
  "Contact events" = 18,
  "Anki (institute cards)" = 8,
  "Anki (own cards)" = 3,
  "All equally useful" = 3
)
#define linetype
linetypes <- c(
  "Scripts" = "solid",
  "Videos of lectures and seminars" = "dashed",
  "Live-streamed casts" = "dotted",
  "Contact events" = "dotdash",
  "Anki (institute cards)" = "longdash",
  "Anki (own cards)" = "twodash",
  "All equally useful" = 11
)

figure <- ggplot(data = df_helpful_materials_reshaped,
                 aes(
                   x = exam,
                   y = share,
                   group = method_helpful,
                   colour = method_helpful
                 ))

figure + geom_point(aes(shape = method_helpful), size = 3) +
  
  geom_line(aes(linetype = method_helpful), linewidth = 2, ) +
  scale_x_discrete(
    limits = groups,
    name = "Exam",
    labels = c(
      "Seminar 2022",
      "Lecture 2022",
      "Seminar 2022/23",
      "Lecture 2022/23",
      "Seminar 2023"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  
  theme_bw() + labs(
    title = "Methods considered helpful for exam preparation",
    color = "Methods considered helpful",
    shape = "Methods considered helpful",
    linetype = "Methods considered helpful",
    x = "Exam",
    y = "Share"
  ) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes)

ggsave("output/plot_helpful_methods.svg",
       width = 10,
       height = 5) # decomment if desired to save

#### Anki considered helpful as percentage of users--------------------------------

subset_anki_helpful <- subset(survey_data, subset = exam != "seminar_22")

# Anki for exam preparation -------------------------------------------------------------------------
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

df_anki_helpful_with_total <- rbind(df_anki_helpful, "Total" = colSums(df_anki_helpful))

# convert to shares
df_anki_helpful_shares_with_total <- df_anki_helpful_with_total[-ncol(df_anki_helpful_with_total)] / 
  df_anki_helpful_with_total$total

colnames(df_anki_helpful_shares_with_total) <-  c("Considered helpful", "Not considered helpful", "No answer")

df_anki_helpful_shares_with_total$Group <- rownames(df_anki_helpful_shares_with_total)
# transform to long
df_long_with_total <- data.frame(
  Group = rep(df_anki_helpful_shares_with_total$Group, times = 3),
  Response = rep(colnames(df_anki_helpful_shares_with_total)[1:3], each = nrow(df_anki_helpful_shares_with_total)),
  Share = c(
    df_anki_helpful_shares_with_total$"Considered helpful",
    df_anki_helpful_shares_with_total$"Not considered helpful",
    df_anki_helpful_shares_with_total$"No answer"
  )
)

#factorize answers
df_long_with_total$Response <- factor(
  df_long_with_total$Response,
  levels = c("No answer", # Bottom
             "Not considered helpful", # Middle
             "Considered helpful"                 # Top)))
  )
)

df_long_no_missing_answers_with_total <- subset(df_long_with_total, subset = !Response == "No answer")

gp <- ggplot(df_long_no_missing_answers_with_total, aes(x = Group, y = Share, fill = Response)) +
  geom_col() +  # Stacked bars
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0),
    breaks = seq(0, 1, 0.2)
  ) +  # Convert to %
  scale_x_discrete(
    limits = df_long_no_missing_answers_with_total$Group,
    name = "Exam",
    labels = c(
      paste0("Lecture 2022\nn = ", df_anki_helpful_with_total$total[1]),
      paste0("Seminar 2022/23\nn = ", df_anki_helpful_with_total$total[2]),
      paste0("Lecture 2022/23\nn = ", df_anki_helpful_with_total$total[3]),
      paste0("Seminar 2023\nn = ", df_anki_helpful_with_total$total[4]),
      paste0("Total\nn = ", df_anki_helpful_with_total$total[5])
    )
  ) +
  labs(x = "Exam",
       y = "Share relative to Anki users per cohort",
       fill = "Response") +
  scale_fill_manual(values = c("gray70", "gray20")) +
  theme_classic()

gp

ggsave(
  "output/Anki_helpful_exam_preparation.svg",
  width = 8,
  height = 4
)

# share anki used for exam preparation
share_used_for_exam <- sum(survey_data$used_anki_institute, na.rm = TRUE) /
  sum(students_per_group[-1])

# Anki use in general ----------------------------------------------------------
count_anki_helpful_and_used_general <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute_general == 1 &
      subset_anki_helpful$helpful_anki_institute_general == 1
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_anki_not_helpful_and_used_general <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute_general == 1 &
      subset_anki_helpful$helpful_anki_institute_general == 2
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_anki_used_but_helpful_unknown_general <- rowsum(
  as.numeric(
    subset_anki_helpful$used_anki_institute_general == 1 &
      is.na(subset_anki_helpful$helpful_anki_institute_general)
  ),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

count_used_anki_general <- rowsum(
  as.numeric(subset_anki_helpful$used_anki_institute_general == 1),
  group = factor(subset_anki_helpful$exam, levels = unique(subset_anki_helpful$exam)),
  na.rm = T
)

df_anki_helpful_general <- data.frame(
  "Considered_helpful" = count_anki_helpful_and_used_general,
  "Not_considered_helpful" = count_anki_not_helpful_and_used_general,
  "No_answer" = count_anki_used_but_helpful_unknown_general,
  "total" = count_used_anki_general
)

df_anki_helpful_general_with_total <- rbind(df_anki_helpful_general, "Total" = colSums(df_anki_helpful_general))

# convert to shares
df_anki_helpful_general_shares_with_total <- df_anki_helpful_general_with_total[-ncol(df_anki_helpful_general_with_total)] / 
  df_anki_helpful_general_with_total$total

colnames(df_anki_helpful_general_shares_with_total) <-  c("Considered helpful", "Not considered helpful", "No answer")

df_anki_helpful_general_shares_with_total$Group <- rownames(df_anki_helpful_general_shares_with_total)
# transform to long
df_long_general_with_total <- data.frame(
  Group = rep(df_anki_helpful_general_shares_with_total$Group, times = 3),
  Response = rep(colnames(df_anki_helpful_general_shares_with_total)[1:3], each = nrow(df_anki_helpful_general_shares_with_total)),
  Share = c(
    df_anki_helpful_general_shares_with_total$"Considered helpful",
    df_anki_helpful_general_shares_with_total$"Not considered helpful",
    df_anki_helpful_general_shares_with_total$"No answer"
  )
)

#factorize answers
df_long_general_with_total$Response <- factor(
  df_long_general_with_total$Response,
  levels = c("No answer", # Bottom
             "Not considered helpful", # Middle
             "Considered helpful"                 # Top)))
  )
)

gp_general <- ggplot(df_long_general_with_total, aes(x = Group, y = Share, fill = Response)) +
  geom_col() +  # Stacked bars
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0),
    breaks = seq(0, 1, 0.2)
  ) +  # Convert to %
  scale_x_discrete(
    limits = df_long_general_with_total$Group,
    name = "Exam",
    labels = c(
      paste0("Lecture 2022\nn = ", df_anki_helpful_general_with_total$total[1]),
      paste0("Seminar 2022/23\nn = ", df_anki_helpful_general_with_total$total[2]),
      paste0("Lecture 2022/23\nn = ", df_anki_helpful_general_with_total$total[3]),
      paste0("Seminar 2023\nn = ", df_anki_helpful_general_with_total$total[4]),
      paste0("Total\nn = ", df_anki_helpful_general_with_total$total[5])
    )
  ) +
  labs(x = "Exam",
       y = "Share relative to Anki users per cohort",
       fill = "Response") +
  scale_fill_manual(values = c("No answer" = "gray90", 
                               "Considered helpful" = "gray20", 
                               "Not considered helpful" = "gray70")) +
  theme_classic()

gp_general

ggsave(
  "output/Anki_helpful_general_use.svg",
  width = 8,
  height = 4
)

share_used_for_exam <- sum(survey_data$used_anki_institute_general == 1, na.rm = TRUE) /
  sum(students_per_group[-1])

#### boxplots of exam scores using anki in multiple choice ("used anki institute")----------------------

# select subset with only defined anki usage and score percentages. Scores of 0 were exluded.
boxplot_subset_mc <- subset(
  survey_data,
  subset =
    !is.na(used_anki_institute) &
    !is.na(score_percentage) &
    score_percentage > 0
)

# factorize groups
boxplot_subset_mc$exam <- factor(boxplot_subset_mc$exam , levels = unique(boxplot_subset_mc$exam))
ggplot(data = boxplot_subset_mc,
       aes(
         x = exam,
         y = score_percentage,
         fill = factor(used_anki_institute, levels = c(1, 0))
       ),
       na.rm = TRUE) +
  geom_boxplot () +
  labs(title = "Exam score percentages among Anki users and non-users",
       x = "Exam",
       y = "Exam score percentages",
       fill = "Exam preparation with Anki institute cards") +
  scale_x_discrete(labels = c(
    paste0("Seminar 2022"),
    paste0("Lecture 2022"),
    paste0("Seminar 2022/23"),
    paste0("Lecture 2022/23"),
    paste0("Seminar 2023")
  )) +
  scale_fill_manual(
    labels = c("0" = "no", "1" = "yes"),
    values = c("0" = "#DDAA33", "1" = "#004488")
  ) +
  scale_y_continuous(
    labels = function(x)
      paste0(x, "%"),
    limits = c(0, 100),
    expand = c(0, 0),
    breaks = seq(0, 100, 10)
  )

ggsave(
  "output/Anki_score_percentage_over_usage_and_exam_mc.svg",
  width = 8,
  height = 4
)

#### boxplot anki usage frequency and exam scores ------------------------------

# subset those who indicated whether they used Anki, have a score, and are not from seminar_22 (no frequency
# information in cohort seminar_22)
boxplot_subset_freq <- subset(
  survey_data,
  subset =
    !is.na(used_anki_institute) &
    !is.na(score_percentage) &
    score_percentage > 0 &
    exam != "seminar_22"
)

# create new column combining frequency of anki usage and whether used Anki at all
boxplot_subset_freq$combined_info <- boxplot_subset_freq$anki_time_spent
boxplot_subset_freq$combined_info[boxplot_subset_freq$used_anki_institute == 0] <- 0
# 1 indicates Anki several times a week, 2 indicates 1/week, 3 indicates 1/month, 0 indicates no usage

# subset those without combined_info = NA
boxplot_subset_freq_no_NA <- subset(
  boxplot_subset_freq, subset = !is.na(boxplot_subset_freq$combined_info))

boxplot_subset_freq_no_NA$combined_info <- factor( # reorder frequency coding
  boxplot_subset_freq_no_NA$combined_info,
  levels = c("0", "3", "2", "1")
)
ggplot(data = boxplot_subset_freq_no_NA,
       aes(
         x = exam,
         y = score_percentage,
         fill = factor(combined_info)
       ),
       na.rm = TRUE) +
  geom_boxplot () +
  labs(title = "Exam score percentages relative to frequency of Anki usage",
       x = "Exam",
       y = "Exam score percentages",
       fill = "Frequency of Anki usage") +
  scale_x_discrete(labels = c(
    paste0("Seminar 2022"),
    paste0("Lecture 2022"),
    paste0("Seminar 2022/23"),
    paste0("Lecture 2022/23"),
    paste0("Seminar 2023")
  )) +
  scale_fill_manual(
    labels = c("0" = "Did not use", "1" = "Multiple times per week", 
               "2" = "Once per week", "3" = "Once per month"),
    values = c(
      "0" = "#DDAA33",  # orange
      "1" = "#004488",  # blue
      "2" = "#BB5566",  # reddish
      "3" = "#228833"   # green
    )
  ) +
  scale_y_continuous(
    labels = function(x)
      paste0(x, "%"),
    limits = c(0, 100),
    expand = c(0, 0),
    breaks = seq(0, 100, 10)
  )

ggsave(
  "output/Anki_score_frequency.svg",
  width = 8,
  height = 4
)

#### exam scores and self-rated performance ------------------------------------
subset_performance_scores <- subset(survey_data,
                                    subset = !is.na(survey_data$score) &
                                      survey_data$score > 0 &
                                      !is.na(survey_data$performance_pharm))

subset_performance_scores_red <- data.frame("score" = subset_performance_scores$z_score,
                                            "performance_pharm" = subset_performance_scores$performance_pharm,
                                            "exam" = subset_performance_scores$exam)

subset_performance_scores_red$performance_pharm_factor <-  # factorize performance levels
  factor(
    subset_performance_scores_red$performance_pharm,
    levels = 1:5,
    labels = c("Far below", "Below", "Average", "Above", "Far above"),
    ordered = TRUE
  )

ggplot(subset_performance_scores_red, aes(x = performance_pharm_factor, y = z_score)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    x = "Self-rated performance in pharmacology",
    y = "Z-standardized exam score"
  ) +
  theme_minimal(base_size = 14) +
  ggtitle("Exam scores dependent on self-rated performance")

ggsave(
  "output/Score_and_rated_performance.svg",
  width = 8,
  height = 4
)

#### plot self rated performance vs. Anki usage --------------------------------
subset_performance_anki <- subset(survey_data,
                                    subset = !is.na(survey_data$performance_general) &
                                    !is.na(survey_data$used_anki_institute))

subset_performance_anki_red <- data.frame("performance_general" = subset_performance_anki$performance_general,
                                          "used_anki_institute" = subset_performance_anki$used_anki_institute)
subset_performance_anki_red$performance_general <- factor(subset_performance_anki_red$performance_general,
                                                        levels = 1:5, labels = c("Far below", "Below", "Average", "Above", "Far above"),
                                                        ordered = TRUE)

subset_performance_anki_red$used_anki_institute <- factor(subset_performance_anki_red$used_anki_institute,
                                                          levels = 0:1, labels = c("Did not use Anki", "Used Anki"),
                                                          ordered = TRUE)

# performance stacked, bars for Anki
ggplot(subset_performance_anki_red, aes(x = used_anki_institute, fill = performance_general)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = 1) +
  labs(
    x = "Used Anki",
    y = "Share of students",
    fill = "Self-rated performance"
  ) +
  theme_minimal(base_size = 14)

# Anki stacked, bars for performance
ggplot(subset_performance_anki_red, aes(x = performance_general, fill = used_anki_institute)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = 1) +
  labs(
    x = "Self-rated performance",
    y = "Percent of students",
    fill = "Anki Usage"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "output/performance_and_Anki.svg",
  width = 8,
  height = 4
)

# same with Wilcoxon
wilcox.test(survey_data$z_score ~ survey_data$used_anki_institute)
# --> same result

restricted_subset <- subset(survey_data, subset = !is.na(performance_general))

# same with lm

result_adjusted_restricted <- lm(z_score ~ used_anki_institute + performance_general, data = restricted_subset)
summary(result_adjusted_restricted)

result_unadjusted_restricted <- lm(z_score ~ used_anki_institute, data = restricted_subset)
summary(result_unadjusted_restricted)

result <- lm(z_score ~ used_anki_institute * performance_general, data = survey_data)
summary(result)
# --> same result

# lm with adjustment for general performance
result_adjusted <- lm(z_score ~ used_anki_institute + performance_general, data = survey_data)
summary(result_adjusted)

# lm with adjustment for pharma performance
result_adjusted <- lm(z_score ~ used_anki_institute + performance_pharm, data = survey_data)
summary(result_adjusted)

# lm with adjustment for general performance as factor (ordinal)
result_adjusted <- lm(z_score ~ used_anki_institute + performance_general_factor, data = survey_data)
summary(result_adjusted)

# lm with adjustment for pharma performance as factor (ordinal)
result_adjusted <- lm(z_score ~ used_anki_institute + performance_pharm_factor, data = survey_data)
summary(result_adjusted)

# what happens when restricting the subset to complete data? Not much!
survey_data_retricted <- subset(survey_data, subset = 
                                  !is.na(survey_data$performance_pharm_factor))

result_unadjusted <- lm(z_score ~ used_anki_institute_general, data = survey_data)
summary(result_unadjusted)

result_adjusted <- lm(z_score ~ performance_pharm_factor + used_anki_institute_general, data = survey_data)
summary(result_adjusted)

#### relationship "Anki useful" with various variables -------------------------

current_subset <- subset(survey_data, subset = !is.na(survey_data$helpful_anki_institute_general))
result <- lm(z_score ~ used_anki_institute_general * helpful_anki_institute_general, data = current_subset)
summary(result)

# subgroup: all except first cohort
subset_score_useful <- subset(survey_data, subset = survey_data$exam != "seminar_22")

# same as above, but without group seminar_22
result_unadjusted <-  lm(z_score ~ helpful_anki_institute, data = subset_score_useful)
summary(result_unadjusted)

# relationship anki helpful vs. score
result_unadjusted <-  lm(z_score ~ helpful_anki_institute, data = subset_score_useful)
summary(result_unadjusted)

# control for  anki helpful
result_adjusted <-  lm(z_score ~ used_anki_institute_general + helpful_anki_institute_general, data = subset_score_useful)
summary(result_adjusted)

modelsummary::modelsummary(
  list("Unadjusted" = result1, "Adjusted" = result2),
  stars = TRUE,
  statistic = "({std.error})",  # show SEs in parentheses
  gof_omit = "AIC|BIC|Log.Lik", # drop clutter
  )

# relationship how Anki used with exam score

mean(survey_data$z_score[survey_data$anki_on_pc == 1],
     na.rm = TRUE)

sum(survey_data$anki_on_mobile, na.rm = TRUE)

mean(survey_data$z_score[survey_data$anki_on_laptop == 1],
     na.rm = TRUE)

mean(survey_data$z_score[survey_data$anki_on_tablet == 1],
     na.rm = TRUE)

mean(survey_data$z_score[survey_data$anki_on_mobile == 1],
     na.rm = TRUE)

# significant test score differences between lecture and seminar exam?
survey_data_exam_type <- survey_data
survey_data_exam_type$exam <- ifelse(grepl("lecture", survey_data_exam_type$exam, ignore.case = TRUE),
                                     "lecture", "seminar")
survey_data_exam_type$exam <- factor(survey_data_exam_type$exam)
t.test(score_percentage ~ exam, data = survey_data_exam_type)

# test for significance in Anki usage between men and women

restricted_for_gender <- subset(survey_data, subset = gender == 1 | 2)
restricted_for_gender$gender <- factor(restricted_for_gender$gender, 
                          levels = c(1, 2),
                          labels = c("female", "male"))
table_gender_anki <- table(restricted_for_gender$gender, restricted_for_gender$used_anki_institute)
chisq.test(table_gender_anki)

# test significant differences in Anki usage between lecture and seminar exam

table_anki_exam_type <-  
