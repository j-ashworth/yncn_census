library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

census_data_raw2 <- read_csv("census_data.csv")
marketing_data <- census_data_raw2[c(3,23)]
names(marketing_data) <- c('year' ,'marketing.channel')

#separate answers into individual columns
temp1 <- marketing_data$marketing.channel
marketing_data$facebook <- str_detect(temp1, 'Facebook')
marketing_data$email <- str_detect(temp1, 'Email')
marketing_data$website <- str_detect(temp1, 'YNCN Website')
marketing_data$ecf <- str_detect(temp1, 'ECF & Plasma Screen')
marketing_data$class.announcement <- str_detect(temp1, 'Class Announcement')
marketing_data$word.of.mouth <- str_detect(temp1, 'Word of mouth')
marketing_data$faculty.member <- str_detect(temp1, 'From a faculty member')
marketing_data$clubs.fair <- str_detect(temp1, 'Clubs Fair')
marketing_data$instagram <- str_detect(temp1, 'Instagram')

marketing_data <- melt(marketing_data[-2], id.vars = 'year')
marketing_data_disc <- marketing_data %>% group_by(year, variable) %>% summarise(sum = sum(value)) %>% mutate(freq = sum / sum(sum))
marketing_data_overall <- marketing_data %>% group_by(variable) %>% summarise(sum1 = sum(value)) %>% mutate(freq = sum1 / sum(sum1))

#over all marketing data

marketing_total <- ggplot(marketing_data_overall[-3], aes(x=variable, y = sum1)) + 
  geom_bar(fill = "#369A97", stat="identity") +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Marketing Reach", x = element_blank(), y = 'Number Responded') +
  scale_x_discrete(labels = c("Facebook", "Email", "Website", "ECF", "Class\nAnnouncement","Word Of\nMouth", "Faculty Member", "Clubs Fair", "Instagram")) +
  theme(legend.position="none") +
  geom_text(aes(label = sum1), vjust = -0.5)


#by year marketing data

marketing_line_prop <- ggplot(marketing_data_disc, aes(x=variable, y = freq, group = year, colour = year)) + 
  geom_line() + 
  geom_point() +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Marketing Reach By Discipline", x = element_blank(), y = 'Proportion Responded') +
  scale_x_discrete(labels = c("Facebook", "Email", "Website", "ECF", "Class\nAnnouncement","Word Of\nMouth", "Faculty Member", "Clubs Fair", "Instagram")) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 













