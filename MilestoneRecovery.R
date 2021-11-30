################################################################################
# Milestone Recovery - STA 564 - Author: Angyalka Valcsics
################################################################################
# read in admit data
admitLog <- read.csv("C:/Users/angya/OneDrive/Documents/R/AdmissionLog.csv")
admitLog <- data.frame(admitLog$DATE..IN,admitLog$TIME.IN,admitLog$DATE.OUT,
                       admitLog$TIME..OUT,admitLog$X..DAYS,admitLog$Bed.nights)
colnames(admitLog) <- c("DateIn", "TimeIn", "DateOut", "TimeOut", "NumDays", 
                        "NumNights")
admitLog <- admitLog[,1:4]
admit = admitLog[!(is.na(admitLog$TimeOut) | admitLog$TimeOut=="-" | admitLog$TimeOut=="" | admitLog$TimeOut=="100-0" | admitLog$TimeOut=="n/a"  | admitLog$TimeOut==" "  | admitLog$TimeOut == "?" | admitLog$TimeOut == "0"),]
admit = admit[!(is.na(admit$TimeIn) | admit$TimeIn== "" | admit$TimeIn== " " | admit$TimeIn == "?" | admit$TimeOut == "0" | admit$TimeOut=="n/a"),]
head(admit)
################################################################################
# Clean data
################################################################################
# READ ME: Please run the following block of code to clean log data.

# first need to combine the data/time columns for arrival and departure
sapply(admit, class)
newTimeIn <- gsub('^([0-9]{1,2})([0-9]{2})$', '\\1:\\2', sprintf('%04d',admit$TimeIn))
admit$TimeOut = as.integer(admit$TimeOut)
newTimeOut <- gsub('^([0-9]{1,2})([0-9]{2})$', '\\1:\\2', sprintf('%04d',admit$TimeOut))
newDateIn = as.Date(admit$DateIn,format='%m/%d/%y')
newDateOut = as.Date(admit$DateOut,format='%m/%d/%y')
admit$arrival <- with(admit, paste0(newDateIn, " ", newTimeIn))
admit$departure <- with(admit, paste0(newDateOut, " ", newTimeOut))
# as.POSIXct() is vectorized
# get arrivals in days
arrInDays = as.numeric(as.POSIXct(admit$arrival, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "days")
admit$arrival_days<- arrInDays
depInDays = as.numeric(as.POSIXct(admit$departure, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "days")
admit$departure_days <- depInDays
# fixes departure times that occur before arrival times
admit = admit[admit["departure_days"] > 0,]
# there's still an issue somewhere...
admit = admit[!(is.na(admit$arrival_days)),]
admit$service = admit$departure_days - admit$arrival_days
admit = admit[admit["service"] > 0,]
admit$interarrival = c(diff(admit$arrival_days), NA)
library(tidyverse)
arrival_counts_days <- admit %>% group_by(DateIn) %>% count()
################################################################################
# Find average service rate, interarrival distribution, service distribution,
# lambda effective (total)
################################################################################
# Find discrete distribution of admits in days
count_days <- arrival_counts_days %>% pull(n)
hist(count_days)
# Find service distribution
hist(admit$service) # Erlang perhaps?
# Find interarrival distribution
hist(admit$interarrival)
# Compute average (effective, total) arrival rate
lambda = mean(count_days)
# 3.492978
# Compute the mean and std of interarrival times in days-units
mean_interarrival = mean(admit$interarrival[1:(length(admit$interarrival)-1)])
# 0.2934559
sd_interarrival = sd(admit$interarrival[1:(length(admit$interarrival)-1)])
# 0.3401754
# these values mean/sd of interarrivals are close so we have some justification 
# for using Poisson arrivals
# compute average service time (days)
mean_service_time = mean(admit$service)
# 3.578522
# average service rate
service_rate = 1/mean_serive_time
# 0.279445
################################################################################
# Read in cleaned raw data
cleaned = read.csv("C:/Users/angya/OneDrive/Documents/R/RawData.cleaned.csv")
library(tidyverse) 
################################################################################
# Find lambda (effective) for admits, males and females, from 2016/2017
################################################################################
# Need to only take rows where the customer was admitted
admit_clean <- cleaned[cleaned$Reason.for.non.admit == "CT ADMITTED TO DETOX",]
admit_clean <- admit_clean[admit_clean$Year == 2016 | admit_clean$Year == 2017,]
################################################################################
# Get info for females
female <- admit_clean[which(admit_clean$Gender == 'FEMALE'),] 
# set up date in MonthYear format for tidyverse group_by function
female["MonthYear"] <- paste(female$Month, female$Year)
# group by date
admit_female <- female %>% group_by(MonthYear) %>% count()
# extract count for each date
count_female_admits <- admit_female %>% pull(n)
# histogram of female count data
hist(count_female_admits, main = "Count Data for Female Admits", xlab = "Female Admits Per Month")
# standard deviation of female count data
sd(count_female_admits) # 5.1012
# compute average effective arrival rate for females per month
lambda_eff_female <- mean(count_female_admits) #22.75
# approximate a conversion to days
lambda_eff_fem_days <- lambda_eff_female/30 # 0.7583
################################################################################
# Get info for males
male <- admit_clean[which(admit_clean$Gender == 'MALE'),]
# set up date in MonthYear format for tidyverse group_by function
male["MonthYear"] <- paste(male$Month, male$Year)
# group by date
admit_male <- male %>% group_by(MonthYear) %>% count()
# extract count for each date 
count_male_admits <- admit_male %>% pull(n)
# histogram of male count data
hist(count_male_admits,  main = "Count Data for Male Admits", xlab = "Male Admits Per Month")
# standard deviation of male count data
sd(count_male_admits) # 9.76
# compute average effective arrival rate for males per month
lambda_eff_male <- mean(count_male_admits) #74.708
# approximate conversion to days
lambda_eff_male_days <- lambda_eff_male/30 # 2.49
################################################################################
# Get lambda effective total
lamda_eff_total < lambda_eff_fem_days + lambda_eff_male_days # 3.2486
################################################################################
# Find lambda for non-admits, males and females, from 2016/2017
################################################################################
nonadmit_clean <- cleaned[cleaned$Reason.for.non.admit != "CT ADMITTED TO DETOX",]
nonadmit_clean <- nonadmit_clean[nonadmit_clean$Year == 2016 | nonadmit_clean$Year == 2017,]
################################################################################
# Get info for females
female <- nonadmit_clean[which(nonadmit_clean$Gender == 'FEMALE'),] 
# set up date in MonthYear format for tidyverse group_by function
female["MonthYear"] <- paste(female$Month, female$Year)
# group by date
nonadmit_female <- female %>% group_by(MonthYear) %>% count()
# extract count for each date
count_female_nonadmit <- nonadmit_female %>% pull(n)
# histogram of female count data
hist(count_female_nonadmit, main = "Count Data for Female Non-Admits", 
     xlab = "Females Turned Away Per Month")
# standard deviation of female count data
sd(count_female_nonadmit) # 8.161
# compute average arrival rate for non-admit females per month
lambda_female_nonadmit <- mean(count_female_nonadmit) #43.42
# approximate a conversion to days
lambda_fem_nonadmit_days <- lambda_female_nonadmit/30 # 1.447
################################################################################
# Get info for males
male <- nonadmit_clean[which(nonadmit_clean$Gender == 'MALE'),] 
# set up date in MonthYear format for tidyverse group_by function
male["MonthYear"] <- paste(male$Month, male$Year)
# group by date
nonadmit_male <- male %>% group_by(MonthYear) %>% count()
# extract count for each date
count_male_nonadmit <- nonadmit_male %>% pull(n)
# histogram of male count data
hist(count_male_nonadmit, main = "Count Data for Male Non-Admits", 
     xlab = "Males Turned Away Per Month")
# standard deviation of male count data
sd(count_male_nonadmit) # 15.73
# compute average arrival rate for non-admit males per month
lambda_male_nonadmit <- mean(count_male_nonadmit) #108.67
# approximate a conversion to days
lambda_male_nonadmit_days <- lambda_male_nonadmit/30 # 3.62
################################################################################
# get total lambda for non-admits
lambda_nonadmit_total <- lambda_male_nonadmit_days + lambda_fem_nonadmit_days
# 5.07
################################################################################
