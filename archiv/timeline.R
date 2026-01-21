### create timeline of studying materials and studying material satisfaction

data_PD22 <- data[data$group == "PD22",]
data_KS22 <- data[data$group == "KS22",]
data_PD23 <- data[data$group == "PD23",]
data_KS23 <- data[data$group == "KS23",]

studying_material_timeline <- data.frame(row.names = descriptive_characteristics$group[1:4])
for (i in c(colnames(data)[1:9])){
  studying_material_timeline[i] <- 100*c(sum(data_PD22[i], na.rm = T)/sum((!is.na(data_PD22[i]))),
                                         sum(data_KS22[i], na.rm = T)/sum((!is.na(data_KS22[i]))),
                                         sum(data_PD23[i], na.rm = T)/sum((!is.na(data_PD23[i]))),
                                         sum(data_KS23[i], na.rm = T)/sum((!is.na(data_KS23[i]))))
}

ggplot(data = studying_material_timeline, aes(x = row.names(studying_material_timeline), y = studying_material_timeline$used_script_digital, group = 1)) + 
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) + 
  scale_x_discrete(limits=row.names(studying_material_timeline)) + 
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey50"),
    panel.ontop = TRUE,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  

  ylim(75,100) +
  ylab("Usage (% of students") +
  xlab(" ") +
  geom_line(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_script_physical, color = "blue", size = 1) +
  geom_point(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_script_physical, color = "blue", size = 3) +
  geom_line(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_textbook, color = "black", size = 1) +
  geom_point(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_textbook, color = "black", size = 3) +
  geom_line(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_guideline, color = "green", size = 1) +
  geom_point(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_guideline, color = "green", size = 3) +
  geom_line(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_anki, color = "yellow", size = 1) +
  geom_point(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_anki, color = "yellow", size = 3) +
  geom_line(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_custom_anki, color = "orange", size = 1) +
  geom_point(aes(x = row.names(studying_material_timeline)), y = studying_material_timeline$used_custom_anki, color = "orange", size = 3)
  
write.csv(studying_material_timeline, "H:/Lehrforschung/Anki/timeline.csv", row.names=T)
  


  