library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(directlabels)
  library(grid)


#select desired data
census_data <- census_data_raw[2:22]

#update column names
colnames(census_data) <- c("program", "year", "YOB", "type.of.work.interest", "exposure.FTCorp", 
                           "exposure.FTStart", "exposure.research", "exposure.graduate", "exposure.ent", "industry.interest",
                           "work.experience", "workshops.attended", "workshops.wanted", "Resume", "Cover Letter",
                           "Networking", "Behavioural Interview", "Technical Interview", "Business Case", 
                           "help.resources", "pain.points")
census_data$program <- as.factor(census_data$program)
census_data$year <- as.factor(census_data$year)

ratings <- census_data[c(1,2,14,15,16,17,18,19)]



#overall average problem counts
ratings_disciplines_all <- melt(ratings[-2], id.vars = 'program')
ratings_average <- ratings_disciplines_all %>% group_by(variable) %>% summarise(av = mean(value))
ratings_average$av <- as.numeric(format(ratings_average$av, digits = 2))

average_ratings_graph_lolly <- ggplot(ratings_average, aes(variable, av)) + 
  ylim(0,5) +
  geom_segment(aes(x=variable,xend = variable, y=0, yend = av), color="skyblue") +
  geom_point(color="navyblue", size=10)  + 
  geom_text(aes(label=av, y=av), vjust=.35, size=3, color="white")+
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Average Ratings of Job Application Competencies", x = element_blank(), y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Resume', 'Cover\nLetter', 'Networking', 'Behavioural\nInterview', 'Technical\nInterview', 'Business\nCase')) +
  theme(legend.position="none") 

#scale_x_discrete(labels = function(labels) {
#  sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
#})


#average problem counts per discipline
ratings_discipline <- melt(filter(ratings[-2], program != 'Arts' & program != 'Commerce' & program != 'Science/Math'), id.vars = 'program')
ratings_average_per_displine <- ratings_discipline %>% group_by(program, variable) %>% summarise(av = mean(value))
ratings_average_per_displine$av <- as.numeric(format(ratings_average_per_displine$av, digits = 2))

ratings_average_facet <- ggplot(ratings_average_per_displine, aes(variable, av)) + 
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
  facet_wrap( ~ program, ncol=2) + xlab('Ratings') + 
  labs(title = "Average Ratings of Job Application Competencies", x = 'Area', y = 'Average Competency Rating') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Resume', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test <- ratings_average_per_displine
levels(test$program)<- c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "Sci/Math", "T1")
layered_data <- data.frame(filter(test, program != 'Min' & program != 'T1'))

layered_facet_line <- ggplot(layered_data, aes(x=variable, y = av, group = program, colour = program)) + 
  geom_line() + 
  geom_point() +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank()) +
  theme_hc()+ 
  scale_colour_hc() +
  ylim(0,5) +
  scale_x_discrete(labels= c('Resume', 'Cover Letter', 'Networking', 'Behavioural\nInterview', 'Technical\nInterview', 'Business\nCase'))+
  labs(title = "Average Ratings of Job Application Competencies by Discipline", x = element_blank(), y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 



#Responses to optional biggest-pain point open question (260 responses)

pain_points_resp <- census_data[,c(1,2,21)]
pain_points_resp <- na.omit(pain_points_resp) 
temp <- pain_points_resp$pain.points
pain_points_resp$interviews <- str_detect(temp, '[Ii]nterview')
pain_points_resp$resume <- str_detect(temp, '[Rr]esume')
pain_points_resp$CV <- str_detect(temp, '[Cc]over')
pain_points_resp$networking <- str_detect(temp, '[Nn]etwork')

#by year
pain_points_year <- melt(pain_points_resp[c(-1, -3)], id.vars = 'year')
count_per_year <- pain_points_year %>% group_by(year, variable) %>% summarise(sum(value))  
names(count_per_year)[3] <- 'sum'
pp_by_year <- ggplot(count_per_year, aes(variable, sum)) +
geom_bar(aes(fill=variable), stat = "identity") + 
  facet_wrap( ~ year, ncol=3) +
  labs(title = "Biggest Pain Points in Job-Finding Process per Year", x = 'Pain Point', y = 'Number of Responses') +
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_x_discrete(labels= c('Interviews', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })

#total

pp_total <- ggplot(count_per_year, aes(variable, sum)) +
  geom_bar(stat = "identity", fill = "#369A97", width = 0.6) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  scale_x_discrete(labels= c('Interviews', 'Resume', 'Cover Letter', 'Networking')) +
  labs(title = "Biggest Pain Points in Job-Finding Process", x = element_blank(), y = 'Number of Responses') +
  theme(legend.position="none")


#finding -biggest pain point is interviews
#average rating between behavioural, technical, and business case interviews
average_interview_rating <- mean(ratings_average$av[c(4, 5, 6)]) # equal to 2.85452
#this is also the lowest rated compentency





