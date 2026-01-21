### create table with descriptive characteristics

descriptive_characteristics <- data.frame(group = c("PD22", "KS22", "PD23", "KS23"),
                                          n = c(sum(data$group=="PD22"), sum(data$group=="KS22"), sum(data$group=="PD23"), sum(data$group=="KS23")),
                                          share_female = c(sum(data$gender==1 & data$group=="PD22", na.rm=T)/sum(data$group=="PD22"),
                                                           sum(data$gender==1 & data$group=="KS22", na.rm=T)/sum(data$group=="KS22"),
                                                           sum(data$gender==1 & data$group=="PD23", na.rm=T)/sum(data$group=="PD23"),
                                                           sum(data$gender==1 & data$group=="KS23", na.rm=T)/sum(data$group=="KS23")),
                                          share_male   = c(sum(data$gender==2 & data$group=="PD22", na.rm=T)/sum(data$group=="PD22"),
                                                           sum(data$gender==2 & data$group=="KS22", na.rm=T)/sum(data$group=="KS22"),
                                                           sum(data$gender==2 & data$group=="PD23", na.rm=T)/sum(data$group=="PD23"),
                                                           sum(data$gender==2 & data$group=="KS23", na.rm=T)/sum(data$group=="KS23")),
                                          share_divers = c(sum(data$gender==3 & data$group=="PD22", na.rm=T)/sum(data$group=="PD22"),
                                                           sum(data$gender==3 & data$group=="KS22", na.rm=T)/sum(data$group=="KS22"),
                                                           sum(data$gender==3 & data$group=="PD23", na.rm=T)/sum(data$group=="PD23"),
                                                           sum(data$gender==3 & data$group=="KS23", na.rm=T)/sum(data$group=="KS23")),
                                          share_unknown =c(sum(is.na(data$gender[data$group =="PD22"]))/sum(data$group=="PD22"),
                                                           sum(is.na(data$gender[data$group =="KS22"]))/sum(data$group=="KS22"),
                                                           sum(is.na(data$gender[data$group =="PD23"]))/sum(data$group=="PD23"),
                                                           sum(is.na(data$gender[data$group =="KS23"]))/sum(data$group=="KS23")),
                                          age_mean =     c(mean(data$age[data$group=="PD22"], na.rm=T),
                                                           mean(data$age[data$group=="KS22"], na.rm=T),
                                                           mean(data$age[data$group=="PD23"], na.rm=T),
                                                           mean(data$age[data$group=="KS23"], na.rm=T)),
                                          age_unknown =  c(sum(is.na(data$age[data$group=="PD22"])),
                                                           sum(is.na(data$age[data$group=="KS22"])),
                                                           sum(is.na(data$age[data$group=="PD23"])),
                                                           sum(is.na(data$age[data$group=="KS23"]))),
                                          exam_mean_score_percentage =
                                                         c(mean(data$score[data$group=="PD22"], na.rm=T),
                                                           mean(data$score[data$group=="KS22"], na.rm=T),
                                                           mean(data$score[data$group=="PD23"], na.rm=T),
                                                           mean(data$score[data$group=="KS23"], na.rm=T)),
                                          score_unknown= c(sum(is.na(data$score[data$group=="PD22"])),
                                                           sum(is.na(data$score[data$group=="KS22"])),
                                                           sum(is.na(data$score[data$group=="PD23"])),
                                                           sum(is.na(data$score[data$group=="KS23"]))))

descriptive_characteristics$share_female <- round(100*descriptive_characteristics$share_female, digits = 2)
descriptive_characteristics$share_male <- round(100*descriptive_characteristics$share_male, digits = 2)
descriptive_characteristics$share_divers <- round(100*descriptive_characteristics$share_divers, digits = 2)
descriptive_characteristics$share_unknown <- round(100*descriptive_characteristics$share_unknown, digits = 2)
descriptive_characteristics$age_mean <- round(descriptive_characteristics$age_mean, digits = 1)
descriptive_characteristics$exam_mean_score_percentage <- round(100*descriptive_characteristics$exam_mean_score_percentage, digits = 2)
descriptive_characteristics$group <- c("Propädeutikum 22", "Kurssemester 22", "Propädeutikum 22/23", "Kurssemester 22/23")
descriptive_characteristics[nrow(descriptive_characteristics)+1,] <- c("Total/mean \u00B1 SD", 
                                                                       nrow(data),
                                                                       round(100*sum(data$gender==1, na.rm = T)/nrow(data), digits = 2),
                                                                       round(100*sum(data$gender==2, na.rm = T)/nrow(data), digits = 2),
                                                                       round(100*sum(data$gender==3, na.rm = T)/nrow(data), digits = 2),
                                                                       round(100*sum(is.na(data$gender))/nrow((data)), digits = 2),
                                                                       paste(round(mean(data$age,na.rm = T), digits = 1), "\u00B1", round(sd(data$age, na.rm = T), digits = 2)),
                                                                       sum(is.na(data$age)),
                                                                       paste(round(100*mean(data$score, na.rm = T), digits = 2), "\u00B1", round(sd(data$score, na.rm = T), digits = 2)),
                                                                       sum(is.na(data$score)))

descriptive_characteristics_aux <- descriptive_characteristics
descriptive_characteristics_aux[1,] <- descriptive_characteristics[2,]
descriptive_characteristics_aux[2,] <- descriptive_characteristics[1,]
descriptive_characteristics_aux[3,] <- descriptive_characteristics[4,] 
descriptive_characteristics_aux[4,] <- descriptive_characteristics[3,]
descriptive_characteristics <- descriptive_characteristics_aux



descriptive_characteristics_table <- gt(descriptive_characteristics)

table_w_header <- tab_header(data = descriptive_characteristics_table, title = "Respondents' characteristics")
table_w_labels <- cols_label(table_w_header, group = " ", n = "Respondents (n)", share_female = "Female (%)", share_male = "Male (%)",
                             share_divers= "Diverse (%)", share_unknown = "Gender not specified (%)", age_mean = "Age (mean, years)", age_unknown = "Age not specified", 
                             exam_mean_score_percentage = "Mean of correct exam questions (%)", score_unknown = "Exam score not known")
table_bold <- tab_style(table_w_labels,
  style = list(
    cell_text(weight = "bold")
  ),
  locations = cells_body(
     # not needed if coloring all columns
    rows = nrow(descriptive_characteristics))
)

table_aligned <- cols_align(table_bold, align = "left")
gtsave(table_aligned, "descriptive_table_rearranged.png", r"(H:\Lehrforschung\Anki\plots)")
############### timeline studying material #######################

### attention: usage from July 2021 estimated from Susanne's paper!
data_PD22 <- data[data$group == "PD22",]
data_KS22 <- data[data$group == "KS22",]
data_PD23 <- data[data$group == "PD23",]
data_KS23 <- data[data$group == "KS23",]

studying_material_timeline <- data.frame(row.names = descriptive_characteristics$group[1:4])
for (i in c(colnames(data)[1:9])){
  studying_material_timeline[i] <- c(sum(data_PD22[i], na.rm = T)/sum((!is.na(data_PD22[i]))),
                                     sum(data_KS22[i], na.rm = T)/sum((!is.na(data_KS22[i]))),
                                     sum(data_PD23[i], na.rm = T)/sum((!is.na(data_PD23[i]))),
                                     sum(data_KS22[i], na.rm = T)/sum((!is.na(data_KS22[i]))))
}

share_questions <- sum(data$used_moodle_quiz, na.rm = T)/sum(!is.na(data$used_moodle_quiz))
share_tasks <- sum(data$used_moodle_task, na.rm = T)/sum(!is.na(data$used_moodle_task))
share_others <- sum(data$used_other, na.rm = T)/sum(!is.na(data$used_other))

                                        
                                         
                                                               

                                         


