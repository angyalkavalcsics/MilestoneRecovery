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
# sapply(admit, class) # check data types
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
library(tidyverse)
arrival_counts_days <- admit %>% group_by(DateIn) %>% count()
######################
# Find interarrival distribution
# reduce interval that we are considering so that there is at most 
# one arrival per unit time -- choose mins unit
findinterarrival <- c()
unique_dates <- unique(admit$DateIn)
for(date in unique_dates){
   subset_of_dates <- admit[which(admit$DateIn == date),]
   for(i in 1:(length(subset_of_dates$DateIn) - 1)) {
      inter_in_hrs = as.numeric(as.POSIXct(subset_of_dates$arrival[i+1], format = "%Y-%m-%d %H:%M")-as.POSIXct(subset_of_dates$arrival[i], format = "%Y-%m-%d %H:%M"), units= "mins")
      findinterarrival <- append(findinterarrival, inter_in_hrs)
   }
}
set.seed(1)
findinterarrival <- na.omit(findinterarrival)
mean(findinterarrival) # 139 mins

comp <- rexp(100, rate = 1/139)
ks.test(findinterarrival, comp) 



hist(findinterarrival, main = "Distribution of Interarrival Time", 
     xlab = "Interarrival Time (mins)")
######################
# same issue as before. this is a bi modal distribution because of the time
# that goes by overnight
test <- matrix(0, nrow = (length(admit$arrival) - 1), ncol = 1)
times <- admit$arrival
for (i in 1:length(times)){
   test[i] = as.numeric(as.POSIXct(times[i+1], format = "%Y-%m-%d %H:%M")-as.POSIXct(times[i], format = "%Y-%m-%d %H:%M"), units= "days")
}
hist(test)
mean(test)
################################################################################
# Find average service rate, interarrival distribution, service distribution,
# lambda effective (total)
################################################################################
# Find discrete distribution of admits in days
count_days <- arrival_counts_days %>% pull(n)
hist(count_days)
# Find service distribution
hist(admit$service, main = "Distribution of Service Time", 
     xlab = "Service Time (days)") # Erlang perhaps?
# Find interarrival distribution
hist(admit$interarrival, main = "Distribution of Interarrival Time in Days", 
     xlab = "Interarrival Time (days)")
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
service_rate = 1/mean_service_time
# 0.279445
################################################################################
# Read in cleaned raw data
cleaned = read.csv("C:/Users/angya/OneDrive/Documents/R/RawData.cleaned.csv")
library(tidyverse) 
################################################################################
# Find lambda (effective) for admits, males and females, from 2018/2019
################################################################################
# Need to only take rows where the customer was admitted
admit_clean <- cleaned[cleaned$Reason.for.non.admit == "CT ADMITTED TO DETOX",]
admit_clean <- admit_clean[admit_clean$Year == 2018 | admit_clean$Year == 2019,]
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
lambda_eff_fem_days <- lambda_eff_female/30 # 0.6514
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
lambda_eff_male_days <- lambda_eff_male/30 # 2.7
################################################################################
# Get lambda effective total
lamda_eff_total <- lambda_eff_fem_days + lambda_eff_male_days # 3.2486
################################################################################
# Find lambda for non-admits, males and females, from 2016/2017
################################################################################
nonadmit_clean <- cleaned[cleaned$Reason.for.non.admit == "PROGRAM FULL",]
nonadmit_clean <- nonadmit_clean[nonadmit_clean$Year == 2018 | nonadmit_clean$Year == 2019,]
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
sd(count_female_nonadmit) # 4.1598
# compute average non-admit arrival rate for females per month
lambda_female_nonadmit <- mean(count_female_nonadmit) #9.5
# approximate a conversion to days
lambda_fem_nonadmit_days <- lambda_female_nonadmit/30 # 0.3167
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
sd(count_male_nonadmit) # 7.488
# compute average non-admit arrival rate for males per month
lambda_male_nonadmit <- mean(count_male_nonadmit) #15.375
# approximate a conversion to days
lambda_male_nonadmit_days <- lambda_male_nonadmit/30 # 0.5125
################################################################################
# get total lambda for non-admits
lambda_nonadmit_total <- lambda_male_nonadmit_days + lambda_fem_nonadmit_days
# 0.829
################################################################################
# Do we really need to split these?
total <- cleaned[cleaned$Reason.for.non.admit == "PROGRAM FULL" | cleaned$Reason.for.non.admit == "CT ADMITTED TO DETOX",]
total <- total[total$Year == 2018 | total$Year == 2019,]
################################################################################
# Trying to get interarrivals using the raw data with no luck
months <- total$Month
uni_mon <- unique(months)
new <- match(months, uni_mon)
new <- sprintf('%02d', new)
total["YearMonth"] <- paste0(total$Year, "-", new, "-", "01")
as.Date(total$MonthYear,format="%Y-%m-%d")
arrInDays = as.numeric(as.POSIXct(total$YearMonth, format = "%Y-%m-%d")-as.POSIXct("2016-01-01", format = "%Y-%m-%d"), units= "days")
total["arrival_days"]<- arrInDays
interarr <- diff(total$arrival_days)
hist(interarr)
################################################################################
# Get info for females
female <- total[which(total$Gender == 'FEMALE'),] 
# set up date in MonthYear format for tidyverse group_by function
female["MonthYear"] <- paste(female$Month, female$Year)
# group by date
total_female <- female %>% group_by(MonthYear) %>% count()
# extract count for each date
count_female_total <- total_female %>% pull(n)
# histogram of female count data
hist(count_female_total, main = "Count Data for Female Admits or Nonadmits", 
     xlab = "Female Arrivals Per Month")
# standard deviation of female count data
sd(count_female_total) # 8.0145
# compute average non-admit arrival rate for females per month
lambda_female_total <- mean(count_female_total) #30.167
# approximate a conversion to days
lambda_fem_total_days <- lambda_female_total/30 # 1.006 
################################################################################
# Get info for males
male <- total[which(total$Gender == 'MALE'),] 
# set up date in MonthYear format for tidyverse group_by function
male["MonthYear"] <- paste(male$Month, male$Year)
# group by date
total_male <- male %>% group_by(MonthYear) %>% count()
# extract count for each date
count_male_total <- total_male %>% pull(n)
# histogram of male count data
hist(count_male_total, main = "Count Data for Male Admits or Nonadmits", 
     xlab = "Males Arrivals Per Month")
# standard deviation of male count data
sd(count_male_total) # 9.19
# compute average non-admit arrival rate for males per month
lambda_male_total <- mean(count_male_total) #91.083
# approximate a conversion to days
lambda_male_total_days <- lambda_male_total/30 # 3.036
################################################################################
# get total lambda for non-admits
lambda_total <- lambda_male_total_days + lambda_fem_total_days
# 4.04

################################################################################
# chi-sqr test for two samples to determine if they came from the same distribution

#generate two data sets
probs = dpois(count_female_total, lambda=lambda_female_total)
chisq.test(x=count_female_total, p = probs, rescale.p = TRUE, simulate.p.value=TRUE)

probs = dpois(count_male_total, lambda=lambda_male_total)
chisq.test(x=count_male_total, p = probs, rescale.p = TRUE, simulate.p.value=TRUE)

# Since the p-values are both about 0.0005 and this is less than .05, we reject
# the null hypothesis. 
# We do not have sufficient evidence to say that the data is poisson

write.csv(na.omit(findinterarrival),"C:/Users/angya/OneDrive/Documents/inter.csv")
write.csv(count_male_total,"C:/Users/angya/OneDrive/Documents/male.csv")
write.csv(count_female_total,"C:/Users/angya/OneDrive/Documents/female.csv")
write.csv(admit$service,"C:/Users/angya/OneDrive/Documents/service.csv")

write.csv(total_male,"C:/Users/angya/OneDrive/Documents/male_ct.csv")
write.csv(total_female,"C:/Users/angya/OneDrive/Documents/female_ct.csv")
