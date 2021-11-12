# read in admit data

admitLog <- read.csv("C:/Users/angya/OneDrive/Documents/R/AdmissionLog.csv")
admitLog <- data.frame(admitLog$DATE..IN,admitLog$TIME.IN,admitLog$DATE.OUT,
                       admitLog$TIME..OUT,admitLog$X..DAYS,admitLog$Bed.nights)
colnames(admitLog) <- c("DateIn", "TimeIn", "DateOut", "TimeOut", "NumDays", 
                        "NumNights")
admitLog <- admitLog[,1:4]
# don't really care about na in the other columns at this time
admit = admitLog[!(is.na(admitLog$TimeOut) | admitLog$TimeOut=="-" | admitLog$TimeOut=="" | admitLog$TimeOut=="100-0" | admitLog$TimeOut=="n/a"  | admitLog$TimeOut==" "  | admitLog$TimeOut == "?" | admitLog$TimeOut == "0"),]
admit = admit[!(is.na(admit$TimeIn) | admit$TimeIn== "" | admit$TimeIn== " " | admit$TimeIn == "?" | admit$TimeOut == "0" | admit$TimeOut=="n/a"),]
head(admit)

# read in non admit data

rawData=read.csv("C:/Users/angya/OneDrive/Documents/R/RawData.csv")
rawData <- data.frame(rawData[, 1:8])
colnames(rawData) <- c("MonthYear", "Gender", "BirthYear", "ReasonForNonAdmit",
                       "DetoxFrom", "Insurance", "Returning", "Resident")
head(rawData)

nonadmit <- data.frame(rawData["MonthYear"], rawData["ReasonForNonAdmit"])

head(nonadmit)

################################################################################
# get arrival times in some arbitrary unit -- hours?

# first need to combine the data/time columns for arrival and departure

sapply(admit, class)

newTimeIn <- gsub('^([0-9]{1,2})([0-9]{2})$', '\\1:\\2', sprintf('%04d',admit$TimeIn))

admit$TimeOut = as.integer(admit$TimeOut)
newTimeOut <- gsub('^([0-9]{1,2})([0-9]{2})$', '\\1:\\2', sprintf('%04d',admit$TimeOut))

newDateIn = as.Date(admit$DateIn,format='%m/%d/%y')
newDateOut = as.Date(admit$DateOut,format='%m/%d/%y')

admit$arrival <- with(admit, paste0(newDateIn, " ", newTimeIn))
admit$departure <- with(admit, paste0(newDateOut, " ", newTimeOut))

head(admit)

# as.POSIXct() is vectorized

# in hrs 

arrInHrs = as.numeric(as.POSIXct(admit$arrival, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "hours")
admit$arrival_hrs <- arrInHrs
depInHrs = as.numeric(as.POSIXct(admit$departure, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "hours")
admit$departure_hrs <- depInHrs

# in days

arrInDays = as.numeric(as.POSIXct(admit$arrival, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "days")
admit$arrival_days<- arrInDays
depInDays = as.numeric(as.POSIXct(admit$departure, format = "%Y-%m-%d %H:%M")-as.POSIXct("2018-01-01 08:45", format = "%Y-%m-%d %H:%M"), units= "days")
admit$departure_days <- depInDays

head(admit)

# fixes departure times that occur before arrival times
admit = admit[admit["departure_days"] > 0,]

# there's still an issue somewhere...
admit = admit[!(is.na(admit$arrival_days)),]

admit$service = admit$departure_days - admit$arrival_days

admit = admit[admit["service"] > 0,]

admit$interarrival = c(diff(admit$arrival_days), NA)

hist(admit$service) # This does not look exponential to me
# erlang perhaps?
hist(admit$arrival_days) # looks approx uniform
hist(admit$departure_days) # also approx unif
hist(admit$interarrival)

# compute average arrival rate
lambda = 1 / mean(admit$arrival_days)
# 0.002771421

# compute the mean and std of interarrival times in days-units
mean_interarrival = mean(admit$interarrival[1:(length(admit$interarrival)-1)])
# 0.2934559

sd_interarrival = sd(admit$interarrival[1:(length(admit$interarrival)-1)])
# 0.3401754

# these values are close so we have some justification for using Poisson arrivals

# compute average service time (days)
mu = mean(admit$service)
# 3.578522

# average service rate
1/mu
# 0.279445

# Now compute the performance measures for this system using M/G/c loss model.

# Find model loss probability (with c=16).

# Compare with actual fraction of  patients turned down.

# Will use this for validation of our model.

