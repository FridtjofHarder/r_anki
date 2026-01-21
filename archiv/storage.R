### create boxplot
# grouped boxplot
ggplot(anki_data, aes(x=group, y=score, fill=anki_used)) + 
  geom_boxplot() + ylab("exam score") + 
  scale_x_discrete(labels=c('Kurssemester 22', 'Kurssemester 22/23', 'Propaedeutikum 22', 'Propaedeutikum 22/23')) + 
  scale_fill_discrete(name = "studied with Anki", labels = c("no", "yes")) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))