library(ggplot2)

#Section 1 - Demogrpahic Analysis
#From 'demographic_analysis.R'

#num responses per year
count_per_year <- ggplot(count_per_year_df, aes(variable, total)) + 
  geom_bar(stat = "identity", fill = "#369A97") +
  labs(title = 'Number of Responses by Year', x = 'Year', y = 'Number of Responses') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  geom_text(aes(label = total), vjust = -0.5)

#num responses per disc
count_per_disc <- ggplot(count_per_disc_df, aes(discipline, total)) + 
  geom_bar(stat = "identity", fill = "#369A97") +
  labs(title = 'Number of Responses by Discipline', x = 'Discipline', y = 'Number of Responses') + 
  scale_x_discrete(labels=c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "A&S", "T1")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none") +
  geom_text(aes(label = total), vjust = -0.5)

#num esponses per year per disc (appendix)
facet_disc_year <- ggplot(eng_data, aes(x=Year, y= Responses, fill=Year, label=Year)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Discipline, ncol =3) + 
  geom_text(aes(label = Responses), vjust = -0.5, size = 2) +
  ylim(0,35) +
  labs(title = 'Number of Responses Per Discipline Per Year') +
  theme(plot.title = element_text(hjust = 0.5)) 

#Section 2 - Pain Points
#From "pain_points.R"
average_ratings_graph_lolly <- ggplot(ratings_average, aes(variable, av)) + 
  ylim(0,5) +
  geom_segment(aes(x=variable,xend = variable, y=0, yend = av), color="skyblue") +
  geom_point(color="blue", size=10)  + 
  geom_text(aes(label=av, y=av), vjust=.35, size=3, color="white")+
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Average Ratings of Job Application Competencies", x = 'Ratings', y = 'Average Response') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Resume', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })

