library(readr)
library(stringr)
library(dplyr)
census_data_raw <- read_csv("census_data.csv")

#select desired data
census_data <- census_data_raw[2:22]

#update column names
colnames(census_data) <- c("program", "year", "YOB", "type.of.work.interest", "exposure.FTCorp", 
                           "exposure.FTStart", "exposure.research", "exposure.graduate", "exposure.ent", "industry.interest",
                           "work.experience", "workshops.attended", "workshops.wanted", "resume.rating", "coverletter.rating",
                           "networking.rating", "behav.interview.rating", "tech.interview.rating", "bizcase.rating", 
                           "help.resources", "pain.points")

#factor required columns
census_data$program <- as.factor(census_data$program)
census_data$year <- as.factor(census_data$year)
census_data$YOB <- as.factor(census_data$YOB)
census_data$program <- as.factor(census_data$program)
census_data$exposure.FTCorp <- as.factor(census_data$exposure.FTCorp)
census_data$exposure.FTStart <- as.factor(census_data$exposure.FTStart)
census_data$exposure.research <- as.factor(census_data$exposure.research)
census_data$exposure.graduate <- as.factor(census_data$exposure.graduate)
census_data$exposure.ent <- as.factor(census_data$exposure.ent)


#breakout columns with multiple selections
temp <- census_data$type.of.work.interest
census_data$FTCorp <- str_detect(temp, 'corporate')
census_data$FTStart <- str_detect(temp, 'start up')
census_data$research <- str_detect(temp, '[Rr]esearch')
census_data$grad.school <- str_detect(temp, 'Graduate studies')
census_data$entrepreneurship <- str_detect(temp, 'Entrepreneurship')

temp <- census_data$industry.interest
census_data$aero <- str_detect(temp, '[Aa]ero')
census_data$biomed.pharma <- str_detect(temp, 'Biomed') | str_detect(temp, 'Pharmaceuticals') | str_detect(temp,'[Mm]ed') | str_detect(temp, '[Hh]ealth')
census_data$energy <- str_detect(temp, '[Ee]nergy') | str_detect(temp, '[Ee]nviro') | str_detect(temp,'[Ss]ustain')
census_data$consulting <- str_detect(temp, '[Cc]onsulting')
census_data$finance <- str_detect(temp, '[Ff]inance') | str_detect(temp, '[Bb]ank') | str_detect(temp,'[Aa]ccount')
census_data$hardware <- str_detect(temp, '[Hh]ardware')| str_detect(temp, '[Pp]hotonics')
census_data$manufacturing <- str_detect(temp, '[Mm]ining')
census_data$software <- str_detect(temp, '[Ss]oftware')| str_detect(temp,'AI') | str_detect(temp,'UX') | str_detect(temp, '[Aa]rtificial [Ii]ntell') | str_detect(temp,'[Rr]obot')

temp <- census_data$work.experience
census_data$volunteer <- str_detect(temp, '[Vv]olun')
census_data$part.time <- str_detect(temp, '[Pp]art')
census_data$internship <- str_detect(temp, '[Ii]ntern')
census_data$ad.hoc <- str_detect(temp, '[Aa]d [Hh]oc')
census_data$research <- str_detect(temp, '[Rr]esearch')
census_data$no.work.exp <- str_detect(temp, '[Nn]one') | str_detect(temp, 'I have no job experience')

temp <- census_data$workshops.attended
census_data$career.fair <- str_detect(temp, '[Cc]areer [Ff]air')
census_data$interview.prep <- str_detect(temp, 'Interview Prep')
census_data$networking.event <- str_detect(temp, 'Company networking event')
census_data$networking.workshop <- str_detect(temp, 'Networking Workshop')
census_data$no.workshops.attended <- str_detect(temp, '[Nn]one')

































