library(readr)
library(dplyr)
library(ggplot2)
library(directlabels)
library(reshape2)
library(magrittr)
library(ggthemes)

census_data_raw <- read_csv("census_data.csv")

#select desired data
census_data1 <- census_data_raw[2:22]

#update column names
colnames(census_data1) <- c("program", "year", "YOB", "type.of.work.interest", "exposure.FTCorp", 
                           "exposure.FTStart", "exposure.research", "exposure.graduate", "exposure.ent", "industry.interest",
                           "work.experience", "workshops.attended", "workshops.wanted", "resume.rating", "coverletter.rating",
                           "networking.rating", "behav.interview.rating", "tech.interview.rating", "bizcase.rating", 
                           "help.resources", "pain.points")

#factor required columns
census_data1$program <- as.factor(census_data1$program)
census_data1$year <- as.factor(census_data1$year)
census_data1$YOB <- as.factor(census_data1$YOB)
census_data1$program <- as.factor(census_data1$program)
census_data1$exposure.FTCorp <- as.factor(census_data1$exposure.FTCorp)
census_data1$exposure.FTStart <- as.factor(census_data1$exposure.FTStart)
census_data1$exposure.research <- as.factor(census_data1$exposure.research)
census_data1$exposure.graduate <- as.factor(census_data1$exposure.graduate)
census_data1$exposure.ent <- as.factor(census_data1$exposure.ent)

temp <- census_data1$type.of.work.interest
census_data1$FTCorp <- str_detect(temp, 'corporate')
census_data1$FTStart <- str_detect(temp, 'start up')
census_data1$research <- str_detect(temp, '[Rr]esearch')
census_data1$grad.school <- str_detect(temp, 'Graduate studies')
census_data1$entrepreneurship <- str_detect(temp, 'Entrepreneurship')

temp <- census_data1$industry.interest
census_data1$aero <- str_detect(temp, '[Aa]ero')
census_data1$biomed.pharma <- str_detect(temp, 'Biomed') | str_detect(temp, 'Pharmaceuticals') | str_detect(temp,'[Mm]ed') | str_detect(temp, '[Hh]ealth')
census_data1$energy <- str_detect(temp, '[Ee]nergy') | str_detect(temp, '[Ee]nviro') | str_detect(temp,'[Ss]ustain')
census_data1$management.consulting <- str_detect(temp, 'Consulting - Management')
census_data1$tech.consulting <- str_detect(temp, 'Consulting - Technology')
census_data1$eng.consulting <- str_detect(temp, 'Consulting - Engineering')
census_data1$finance <- str_detect(temp, '[Ff]inance') | str_detect(temp, '[Bb]ank') | str_detect(temp,'[Aa]ccount')
census_data1$hardware <- str_detect(temp, '[Hh]ardware')| str_detect(temp, '[Pp]hotonics')
census_data1$manufacturing <- str_detect(temp, '[Mm]ining')
census_data1$software <- str_detect(temp, '[Ss]oftware')| str_detect(temp,'AI') | str_detect(temp,'UX') | str_detect(temp, '[Aa]rtificial [Ii]ntell') | str_detect(temp,'[Rr]obot')
census_data1$consulting.general <- str_detect(temp, '[Cc]onsulting')

##################Industry Interests##################
industry_data <- data.frame(census_data1[c(1,2,27,28,29,30,31,32,33,34,35,36,37)])
industry_data_discipline <- melt(industry_data[-2], id.vars = 'program')
industry_count_per_disc <- industry_data_discipline %>% group_by(program, variable) %>% summarise(sum = sum(value)) %>% mutate(freq = sum / sum(sum))
filtered<-filter(industry_count_per_disc[-4], program != 'Arts' & program != 'Commerce' & program != 'Science/Math' & variable != 'management.consulting' & variable != 'tech.consulting' & variable != 'eng.consulting')

#by program facet bar
industry_plot_by_program <- ggplot(filtered, aes(x=variable, y=sum)) +
  geom_bar(aes(fill=variable), stat = "identity")+
  facet_wrap( ~program, ncol=2) +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Industry Interests", x = 'Industry', y = 'Number of Interested Respondants') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Aero', 'Biomed/\nPharma', 'Energy', 'Finance', 'Hard-\nware', 'Manufac-\nturing', 'Soft-\nware', 'Consulting'))+
  geom_text(aes(label = sum), vjust = -0.5, size = 2) +
  theme(legend.position="none")
  



#by program line proportion
levels(industry_count_per_disc$program)<- c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "Sci/Math", "T1")

industry_disc_line_prop <- ggplot(filter(industry_count_per_disc[-3], program !='Min'& program != 'Arts' & program != 'Comm' &program != 'T1'  &program != 'Sci/Math' & variable != 'management.consulting' & variable != 'tech.consulting' & variable != 'eng.consulting'), aes(x=variable, y = freq, group = program, colour = program)) + 
  geom_line() + 
  geom_point() +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Industry Interests Per Discipline", x = element_blank(), y = 'Proportion Interested') +
  scale_x_discrete(labels= c('Aerospace', 'Biomed/\nPharma', 'Energy', 'Finance', 'Hardware', 'Manufacturing', 'Software', 'Consulting')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 


# overall bar
industry_count_total <- industry_data_discipline %>% group_by(variable) %>% summarise(sum = sum(value))  

industry_plot_overall <- ggplot(filter(industry_count_total, variable != 'consulting.general'), aes(variable, sum)) +
  geom_bar(fill = "#369A97", stat = "identity") + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Overall Work Industry Interest", x = element_blank(), y = element_blank()) +
  scale_x_discrete(labels= c('Aerospace', 'Biomed/Pharma', 'Energy', 'Management\nConsulting', 'Tech\nConsulting','Eng\nConsulting','Finance','Hardware', 'Manufac-\nturing', 'Software')) +
  theme(legend.position="none")

#################Type of work###################

type_of_work_data <- data.frame(census_data1[c(1,2,22,23,24,25,26)])
work_type_discipline_temp <- melt(type_of_work_data[-2], id.vars = 'program')
work_type_discipline <- work_type_discipline_temp %>% group_by(program, variable) %>% summarise( sum= sum(value)) %>% mutate(freq = sum / sum(sum))
names(work_type_discipline)[3] <- 'sum'
work_type_total <- work_type_discipline_temp %>% group_by(variable) %>% summarise(sum = sum(value))  
work_type_year <-  melt(type_of_work_data[-1], id.vars = 'year')
work_type_year <- work_type_year %>% group_by(year, variable) %>% summarise(sum = sum(value))  %>% mutate(freq = sum / sum(sum))


#by program`plot`

#count facet
work_type_discipline_plot <- ggplot(filter(work_type_discipline[-4], variable != 'Track One' & program != 'Arts' & program != 'Mineral Engineering'), aes(variable, sum)) +
  geom_bar(aes(fill=variable), stat = "identity") + 
  facet_wrap( ~program, ncol=3) +
  labs(title = "Post-Grad Work Interests", x = 'Type of Work', y = 'Number of Interested Respondants') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })

#layered line proportion
levels(work_type_discipline$program)<- c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "Sci/Math", "T1")

work_type_disc_line_prop <- ggplot(filter(work_type_discipline[-3], program !='Min'& program != 'Arts' & program != 'T1'& program != 'Comm' & program != 'Sci/Math'), aes(x=variable, y = freq, group = program, colour = program)) + 
  geom_line() + 
  geom_point() +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Post-Grad Work Interests Per Discipline", x = element_blank(), y = 'Relative Proportion Interested Respondants') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad School', 'Entrepreneurship')) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 


#by year plot

##facet bar
work_type_year_plot <- ggplot(work_type_year[-3], aes(variable, freq)) +
  geom_bar(aes(fill=variable), stat = "identity") + 
  facet_wrap( ~year, ncol=3) +
  labs(title = "Post-Grad Work Interests", x = 'Type of Work', y = 'Relative Proportion of Interested Respondants') +
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad School', 'Entrepreneurship')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(labels) {
    sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 != 0, '', '\n'), labels[i]))
  })

##layered line - counts
work_type_year_line <- ggplot(work_type_year[-4], aes(x=variable, y = sum, group = year, colour = year)) + 
  geom_line() + 
  geom_point() +
  theme_light() +
  labs(title = "Post-Grad Work Interests", x = 'Type of Work', y = 'Number of Interested Respondants') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad School', 'Entrepreneurship')) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 

##layered line - proprtion
work_type_year_line_prop <- ggplot(work_type_year[-3], aes(x=variable, y = freq, group = year, colour = year)) + 
  geom_line() + 
  geom_point() +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), legend.title = element_blank(), axis.title.y = element_text(size = 10)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Post-Grad Work Interests Per Year", x = element_blank(), y = 'Relative Proportion Interested Respondants') +
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad School', 'Entrepreneurship')) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 



#overall plot
work_type_total_plot <- ggplot(work_type_total, aes(variable, sum)) +
  geom_bar(stat = "identity", fill = "#369A97", width = 0.5) +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8)) +
  theme_hc()+ 
  scale_colour_hc() +
  labs(title = "Post-Grad Work Interests", x = element_blank(), y = element_blank()) +
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad\nSchool', 'Entrepreneurship')) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=9)) +
  theme(legend.position="none") +
  geom_text(stat = 'sum', aes(label=sum, y=sum), size=3, color="white", vjust=1.5)

############ EXPOSURE ###########

exposure_data <- data.frame(census_data1[c(1,2,5,6,7,8,9)])
exposure_data <-filter(exposure_data, year != '1st')
exposure_data_discipline <- melt(exposure_data[-2], id.vars = 'program')
exposure_data_discipline$value <- sapply(exposure_data_discipline$value, as.numeric)
exposure_av_per_disc <- exposure_data_discipline %>% group_by(program, variable) %>% summarise(mean = mean(value)) 
exposure_av_overall <- exposure_data_discipline %>% group_by(variable) %>% summarise(mean = mean(value)) 
levels(exposure_av_per_disc$program)<- c("Arts", "Chem", "Civ", "Comm", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "Min", "Sci/Math", "T1")

exposure_av_overall$count <- work_type_total$sum
exposure_av_overall$mean <- as.numeric(format(exposure_av_overall$mean, digits = 2))

#overall plot
work_type_total_plot_exp <- ggplot(exposure_av_overall) +
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.text = element_text(size = 8), axis.text.y = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank()) +
  labs(title = "Post-Grad Work Interests & Exposure", x = element_blank(), y = element_blank()) +
  geom_bar(aes(x=variable, y=count), stat = "identity", fill = "#369A97", width = 0.5) +
  geom_line(aes(x=variable, y=mean*100, group=1)) +
  geom_point(aes(x=variable, y=mean*100)) +
  geom_text(aes(label = count, x=variable, y=count), colour="black", vjust = -1, size = 3)+
  geom_text(aes(label = mean, x=variable, y=mean*100), colour="black", stat='identity', size = 3, vjust = 2)+
  scale_x_discrete(labels= c('Full Time\nCorporate', 'Full Time\nStartup', 'Research', 'Grad\nSchool', 'Entrepreneurship')) +
  scale_y_continuous(sec.axis = sec_axis(~./100))

exposure_year_line_prop <- ggplot(filter(exposure_av_per_disc, program != 'Arts' & program != 'Commerce' & program != 'Science/Math' & program != 'Mineral Engineering'), aes(x=variable, y = mean, group = program, colour = program)) + 
  geom_line() + 
  geom_point() +
  theme_light() +
  labs(title = "Post-Grad Work Exposure By Discipline", x = 'Type of Work', y = 'Average Response') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  geom_dl(aes(label = program), method = list(dl.trans(x = x + .3), "last.bumpup", cex = 0.8)) +
  geom_dl(aes(label = program), method = list(dl.trans(x = x - .3), "first.bumpup", cex = 0.8)) 


test_data_exp <- filter(exposure_av_per_disc, variable == 'exposure.FTStart' & program !='Arts' & program != 'Comm' & program != 'Sci/Math' & program != 'Min')
test_data_interest <- filter(work_type_discipline, variable == 'FTStart' & program !='Arts' & program != 'Commerce' & program != 'Science/Math' & program != 'Mineral Engineering')
#levels(test_data_interest$program)<- c("Chem", "Civ", "CE", "EE", "EngSci", "Indy", "MSE", "Mech", "T1")
#test_data_final <- rbind(test_data_exp, test_data_interest)
#names(test_data_exp)[3] <- 'test'
#names(test_data_interest)[4] <- 'test1'


  



