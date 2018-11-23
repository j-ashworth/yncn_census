library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

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
count_per_year <- ggplot(count_per_year_df, aes(variable, total)) + 
  geom_bar(stat = "identity", fill = "#369A97") +
  labs(title = 'Number of Responses by Year', x = 'Year', y = 'Number of Responses') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  geom_text(aes(label = total), vjust = -0.5)

#total count per discipline
count_per_disc_df <- plot_df[,c(1,7)]
count_per_disc <- ggplot(count_per_disc_df, aes(discipline, total)) + 
  geom_bar(stat = "identity", fill = "#369A97") +
  labs(title = 'Number of Responses by Discipline', x = 'Discipline', y = 'Number of Responses') + 
  scale_x_discrete(labels=c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "A&S", "T1")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none") +
  geom_text(aes(label = total), vjust = -0.5)

#facet plot - breakdown by discipline by year, non-engineering disciplines omitted
#eng_data <- filter(data.m, discipline != 'Arts', discipline != 'Commerce', discipline != 'Science/Math')
eng_data <- filter(data.m, discipline != 'Track One')
eng_data$variable <- factor(eng_data$variable, levels=rev(levels(eng_data$variable)))
facet_disc_year <- ggplot(eng_data, aes(x=variable, y=value, fill=variable, label=variable)) + 
  geom_bar(stat="identity") +
  facet_wrap(~discipline, ncol =3) + 
  geom_text(aes(label = value), vjust = -0.5, size = 2)
  

