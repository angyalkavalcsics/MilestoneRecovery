# read in admit data

admitLog <- read.csv("C:/Users/angya/OneDrive/Documents/R/AdmissionLog.csv")
admitLog <- data.frame(admitLog$DATE..IN,admitLog$TIME.IN,admitLog$DATE.OUT,
                      admitLog$TIME..OUT,admitLog$X..DAYS,admitLog$Bed.nights)
colnames(admitLog) <- c("DateIn", "TimeIn", "DateOut", "TimeOut", "NumDays", 
                       "NumNights")

# don't really care about na in the other columns at this time
admit = admitLog[!(is.na(admitLog$TimeOut) | admitLog$TimeOut=="" | admitLog$TimeOut == "?"),]
admit = admit[!(is.na(admitLog$TimeIn) | admitLog$TimeIn=="" | admitLog$TimeIn == "?"),]
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

# gotta love reformatting everything -_-

newTimeIn <- sprintf("%04d", admit$TimeIn)
newTimeIn <- gsub("(..)(..)", "\\1:\\2", newTimeIn)

newTimeOut <- sprintf("%04s", admit$TimeOut)
newTimeOut <- gsub("(..)(..)", "\\1:\\2", newTimeOut)

newDateIn = as.Date(admit$DateIn,format='%m/%d/%y')
newDateOut = as.Date(admit$DateOut,format='%m/%d/%y')

admit$arrival <- with(admit, paste0(newDateIn, " ", newTimeIn, ":00"))
admit$departure <- with(admit, paste0(newDateOut, " ", newTimeOut, ":00"))

head(admit)

admit = na.omit(admit)

# as.POSIXct() is vectorized


arrInHrs = as.numeric(as.POSIXct(admit$arrival)-as.POSIXct("2018-01-01 08:45:00"), units= "hours")
admit$arrival_hrs <- arrInHrs
depInHrs = as.numeric(as.POSIXct(admit$departure)-as.POSIXct("2018-01-01 08:45:00"), units= "hours")
admit$departure_hrs <- depInHrs
admit$interarrivals = depInHrs - arrInHrs

# this data is really........

admit = admit[admit$interarrivals >= 0, ]

hist(admit$interarrivals) # This does not look exponential to me
# erlang perhaps?
hist(admit$arrival_hrs) # looks approx uniform
hist(admit$departure_hrs) # also approx unif






