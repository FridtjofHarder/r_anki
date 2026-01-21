### recreate figure on favored teaching format

preference_data <- select(data, "helpful_script":"helpful_all_equally", "group")

preference_data$group <- factor(preference_data$group, levels=unique(preference_data$group))

preference_data_by_group <- split(preference_data, preference_data$group)

vector_of_means <- c(colMeans(preference_data_by_group[[1]][,-c(7:8, 10)], na.rm = T),
                     colMeans(preference_data_by_group[[2]][,-c(7:8, 10)], na.rm = T),
                     colMeans(preference_data_by_group[[3]][,-c(7:8, 10)], na.rm = T),
                     colMeans(preference_data_by_group[[4]][,-c(7:8, 10)], na.rm = T))

vector_of_methods <- as.factor(rep(c("Learning scripts", "Videos", "Live streaming", "Contact events", "Medication tasks",
                                     "Comprehension questions", "All forms are equally useful"), 4))

vector_of_groups <- as.factor(c(rep("Kurssemester 22", 7), rep("Propaedeutikum 22", 7), rep("Kurssemester 23", 7), rep("Propaedeutikum 23", 7)))

df <- data.frame(vector_of_means, vector_of_methods, vector_of_groups)

df_ordered <- df[order(factor(df$vector_of_methods, levels = unique(df$vector_of_methods))),]

ggplot(df_ordered, aes(fill=vector_of_groups, y=vector_of_means, x=vector_of_methods)) + 
  geom_bar(position="dodge", stat="identity") + scale_x_discrete(limits=df_ordered$vector_of_methods)