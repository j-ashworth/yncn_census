library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

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
average_ratings_graph <- ggplot(ratings_average, aes(variable, av)) + 
  geom_bar(aes(fill=variable), stat = "identity") + 
  ylim(0,5) +
  labs(title = "Average Ratings of Job Application Competencies", x = 'Ratings', y = 'Average Response') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Resume', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })


#average problem counts per discipline
ratings_discipline <- melt(filter(ratings[-2], program != 'Arts' & program != 'Commerce' & program != 'Science/Math'), id.vars = 'program')
ratings_average_per_displine <- ratings_discipline %>% group_by(program, variable) %>% summarise(av = mean(value))
ratings_average_facet <- ggplot(ratings_average_per_displine, aes(variable, av)) + 
  geom_bar(aes(fill=variable), stat = "identity") + 
  facet_wrap( ~ program, ncol=5) + xlab('Ratings') + 
  ylim(0,5) +
  labs(title = "Average Ratings of Job Application Competencies", x = 'Ratings', y = 'Average Response') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Resume', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })



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
  geom_bar(aes(fill=variable), stat = "identity") + 
  labs(title = "Biggest Pain Points in Job-Finding Process per Year", x = 'Pain Point', y = 'Number of Responses') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Interviews', 'Cover Letter', 'Networking', 'Behavioural Interview', 'Technical Interview', 'Business Case')) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })
#finding - biggest pain point is interviews
#average rating between behavioural, technical, and business case interviews
average_interview_rating <- mean(ratings_average$av[c(4, 5, 6)]) # equal to 2.85452
#this is also the lowest rated compentency





