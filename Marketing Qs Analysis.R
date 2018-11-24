library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

marketing_data <- census_data_raw[23]
names(marketing_data) <- 'marketing.channel'

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


summary(marketing_data)
results <- t(data.frame(as.list(colSums(marketing_data[-1,-1]))))















