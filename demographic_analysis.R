library(readr)
library(dplyr)
library(ggplot2)
library(reshape2) 
library(ggthemes) 

#select relevant demographic columns
census <- read.csv('census_data.csv')
names(census)[2] <- 'discipline'
names(census)[3] <- 'year'
census$discipline <- as.factor(census$discipline)
census$year <- as.factor(census$year)
df <- census[2:3]

#group by discipline and year
df_grouped <- df %>% group_by(discipline, year) %>% summarise(num_students = length(year))
#create dfs of each year, rename cols so they can be merged
first <- filter(df_grouped, year == '1st') 
names(first)[3] <- 'first'
second <- filter(df_grouped, year == '2nd')
names(second)[3] <- 'second'
third <- filter(df_grouped, year == '3rd')
names(third)[3] <- 'third'
PEY <- filter(df_grouped, year == 'PEY')
names(PEY)[3] <- 'PEY'
fourth <- filter(df_grouped, year == '4th')
names(fourth)[3] <- 'fourth'

plot_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(unique(df_grouped[1]), first[-2], second[-2], third[-2], PEY[-2], fourth[-2]))
plot_df[is.na(plot_df)] <- 0
plot_df$total <- rowSums(plot_df[,2:6])
data.m <- melt(plot_df[-7], id.vars = 'discipline')


#total count per year
count_per_year_df <- data.m %>% group_by(variable) %>% summarise(total = sum(value)) 
count_per_year_graph <- ggplot(count_per_year_df, aes(variable, total)) + 
  geom_bar(stat = "identity", fill = "#369A97", width = 0.6) +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = 'Number of Responses Per Year', x = element_blank(), y = element_blank()) + 
  scale_x_discrete(labels=c("First", "Second", "Third","PEY", "Fourth"))



#total count per discipline
count_per_disc_df <- plot_df[,c(1,7)]
count_per_disc <- ggplot(count_per_disc_df, aes(discipline, total)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8)) +
  theme_hc()+ 
  scale_colour_hc() +
  geom_bar(stat = "identity", fill = "#369A97") +
  labs(title = 'Number of Responses Per Discipline', x = element_blank(), y = element_blank()) + 
  scale_x_discrete(labels=c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "A&S", "T1"))



#facet plot - breakdown by discipline by year, non-engineering disciplines omitted
#eng_data <- filter(data.m, discipline != 'Arts', discipline != 'Commerce', discipline != 'Science/Math')
eng_data <- filter(data.m, discipline != 'Track One')
names(eng_data) <- c("Discipline", "Year", "Responses")
#eng_data$Year <- factor(eng_data$Year, levels=rev(levels(eng_data$Year)))
facet_disc_year <- ggplot(eng_data, aes(x=Year, y= Responses, fill=Year, label=Year)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Discipline, ncol =3) + 
  geom_text(aes(label = Responses), vjust = -0.5, size = 2) +
  ylim(0,35) +
  theme_light() +
  labs(title = 'Number of Responses Per Discipline Per Year') +
  theme(plot.title = element_text(hjust = 0.5)) 


