####### install all required packages
required_packages <- c("gt", "readxl", "dplyr", "tibble", "grDevices")
installed_packages <- required_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(required_packages[!installed_packages])
}
invisible(lapply(required_packages, library, character.only = TRUE))

####### read .xlsx file
survey_data <- read_excel("H:/R/Lehrforschung/Daten/anki_data_comprehensive.xlsx")
View(survey_data)

####### create df structure for descriptive stats. Age is not yet analysed descriptivally. Order of groups is: KS22, PD22, KS23, PD23

students_per_group <- c(196, 107, 111, 266) # number of students who wrote the exam in each group (excluding students absent for the exam). Data taken from score reports for each exam.
groups <- unique(survey_data$group) # vector of different groups

respondents <- table(survey_data$group)[groups] # create table of group frequencies and sort them by vector "groups". Otherwise, columns will be sorted by alphabet

response_rate <-  round(100*respondents/students_per_group, 2) # create table of response rates in percentages

gender_table <- table(survey_data$group, survey_data$gender)[groups,] # create table of declared gender per group (1 = male, 2 = female, 3 = diverse or not declared)
gender_undeclared <- respondents - rowSums(gender_table) # get no of students who did not declare gender in each group
gender_table[,3] <- gender_table[,3] + gender_undeclared # add no of undeclared genders per group to gender table. row "3" now comprises both students of answered diverse, or who did not answer the question at all.

score_percentages_means <- aggregate(100*score_percentage ~ group, data=survey_data, mean) # create df of mean score percentages by group
score_percentages_sds <- aggregate(100*score_percentage ~ group, data=survey_data, sd) # create df of score percentage standard deviations by group
score_percentages_means_sds <- paste(round(score_percentages_means[,2], 2), "\u00B1", round(score_percentages_sds[,2], 2)) # paste means +/- sds after rounding to 2 digits
percentage_anki_users <- 

descriptive_matrix <- cbind(respondents, response_rate = response_rate, gender_table, score_percentages_means_sds = score_percentages_means_sds) # combine to table with all descriptive stats
descriptive_df <- data.frame(descriptive_matrix) # convert matrix to df
####### create neat descriptive table

descriptive_tbl <- as_tibble(cbind(nms = names(descriptive_df), t(descriptive_df))) # transpose and convert to tibble object

row_names <- c("Respondents [n]", "Response rate [%]", "Male", "Female", "Non-binary or refused to disclose", "Mean percentage of correct anwers [%] \u00B1 standard deviation")

descriptive_tbl$nms = row_names # fill first column in descriptive_tbl with row names

descriptive_tbl_gt <- gt(descriptive_tbl, rowname_col = "nms") # create gt object with $nms as row names

indented_rows <- c("Male", "Female", "Non-binary or refused to disclose") # define rows to be idented (gender rows)

descriptive_tbl_gt <- tab_row_group(descriptive_tbl_gt, label = "Gender",  rows = indented_rows) # create gt_tbl object

descriptive_tbl_gt <- tab_stub_indent(descriptive_tbl_gt, rows= indented_rows, indent = 3)

descriptive_tbl_gt <- row_group_order(descriptive_tbl_gt, groups = c(NA, "Gender"))

descriptive_tbl_gt

###### trace graph
variables_of_interest <- c("used_script_digital", "")
table(survey_data$group, survey_data$used_script_digital)
###### inference statistics

###### create trace graph