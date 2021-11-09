# read in admit data

admitLog <- read.csv("C:/Users/angya/OneDrive/Documents/R/AdmissionLog.csv")
admitLog <- data.frame(admitLog$DATE..IN,admitLog$TIME.IN,admitLog$DATE.OUT,
                       admitLog$TIME..OUT,admitLog$X..DAYS,admitLog$Bed.nights)
colnames(admitLog) <- c("DateIn", "TimeIn", "DateOut", "TimeOut", "NumDays", 
                        "NumNights")

# don't really care about na in the other columns at this time
admit = admitLog[!(is.na(admitLog$TimeOut) | admitLog$TimeOut=="-" | admitLog$TimeOut=="" | admitLog$TimeOut == "?" | admitLog$TimeOut == "0"),]
admit = admit[!(is.na(admit$TimeIn) | admit$TimeIn=="" | admit$TimeIn == "?"),]
head(admit)
admit = admit[, 1:4]

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
admit$TimeOut <- as.integer(admit$TimeOut)
sapply(admit, class)

# gotta love reformatting everything

newTimeIn <- sprintf("%04s", admit$TimeIn)
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
admit = admit[admit$departure_hrs >= 0, ]


admit$service = admit$departure_hrs - admit$arrival_hrs

as.numeric(as.POSIXct("2018-01-16 18:45:00")-as.POSIXct("2018-01-01 08:45:00"), units= "hours")

admit$interarrivals = c(diff(arrInHrs), NA)

hist(admit$service) # This does not look exponential to me
# erlang perhaps?
hist(admit$arrival_hrs) # looks approx uniform
hist(admit$departure_hrs) # also approx unif
hist(admit$interarrivals)
