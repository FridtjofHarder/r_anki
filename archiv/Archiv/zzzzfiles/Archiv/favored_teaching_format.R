### recreate figure on favored teaching format
install.packages("data.table")


format_data <- select(data, "prefer_online", "group")


format_data$group <- factor(format_data$group, levels=unique(format_data$group))

format_data_by_group <- split(format_data, format_data$group)

format_table <- table(format_data)
prop_format_table <- prop.table(format_table, margin = 2)
df_format <- as.data.frame(prop_format_table)


df_ordered <- df[order(factor(df$vector_of_methods, levels = unique(df$vector_of_methods))),]

ggplot(data = df_format, aes(x = group, y = Freq, fill = factor(prefer_online)))+
  geom_bar(stat="identity") +
  scale_x_discrete(limits=rev(df_format$group)) +
  coord_flip()
   
