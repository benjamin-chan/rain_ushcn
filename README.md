USHCN Precipitation Data Analysis Using R
===========

Long-term daily precipitation records from the US Historical Climatology Network (USHCN) are processed and summarized using R. Below is R code which examines the data from Livermore, CA.

Long-term daily precipitation (rain/snow, and also temperature) records for the United States are available from the United States Historical Climatology Network - [USHCN](http://cdiac.ornl.gov/epubs/ndp/ushcn/ushcn.html). These are observations of precipitation for typically about 100 years or longer. The USHCN stations are a subset of the larger/denser (but shorter) observation network from the Global Historical Climatology Network - [GHCN](http://www.ncdc.noaa.gov/oa/climate/ghcn-daily/)

I obtained the data for California and other relevant data from [here](http://cdiac.ornl.gov/ftp/ushcn_daily/).

R code
==========

Identify the station nearest to your location, identified from "ushcn-stations.txt". For me its Livermore, CA.


```r
myId <- "044997"
```


Following is the format of the data, from "data_format.txt"


```r
# .... (Each record in a file contains one month of daily data.)
# 
# Variable Columns Type COOP ID 1-6 Character YEAR 7-10 Integer MONTH
# 11-12 Integer ELEMENT 13-16 Character VALUE1 17-21 Integer MFLAG1 22
# Character QFLAG1 23 Character SFLAG1 24 Character VALUE2 25-29 Integer
# MFLAG2 30 Character QFLAG2 31 Character SFLAG2 32 Character .  .  .
```


Process precipitation data for your state and extract a subset of data corresponding to your station.


```r
# read data for all stations in the state
allData <- readLines("state04_CA.txt")

# extract station ids from the data
idData <- substr(allData, 1, 6)
# create a new data frame, with ids as the first column of the frame
newData <- data.frame(idData, allData, stringsAsFactors = FALSE)
# extract data corresp to your nearest station
myData <- subset(newData, idData == myId)
myData <- myData[, 2]  # discard previously added first column
```


Below function used later to determine the number of days in a month, including leap years.


```r
# function to compute days in a month: input is year and month this is the
# best I could do without downloading external R libraries
FnDaysInMonth <- function(yr, mo) {
    date1 <- paste(yr, mo, "01", sep = "-")  #current month, day 1
    
    mo2 <- ifelse(mo < 12, mo + 1, 1)
    yr2 <- ifelse(mo < 12, yr, yr + 1)
    date2 <- paste(yr2, mo2, "01", sep = "-")  #next month, day 1
    
    return(as.numeric(difftime(as.Date(date2), as.Date(date1))))
}
```


Output file to store the data and read it back again for plotting


```r
outFile <- file(paste(myId, ".txt", sep = ""), "wt")
```


Each line of the data file contains data corresponding to all the days in the month. Discard temperature records and read only "PRCP". Also, check for the data quality flags.


```r
## each line is 1 month of data
for (eachLine in 1:length(myData)) {
    yrVar <- as.numeric(substr(myData[eachLine], 7, 10))
    moVar <- as.numeric(substr(myData[eachLine], 11, 12))
    metVar <- substr(myData[eachLine], 13, 16)
    
    # only extract precipitation info
    if (metVar == "PRCP") {
        ## for each day of the month , check the data flags and get the data
        for (eachDay in 1:FnDaysInMonth(yrVar, moVar)) {
            dayOffset <- 17 + ((eachDay - 1) * 8)
            metVal <- as.numeric(substr(myData[eachLine], dayOffset, dayOffset + 
                4))
            mflag <- substr(myData[eachLine], dayOffset + 5, dayOffset + 5)  #is irrelevant
            qflag <- substr(myData[eachLine], dayOffset + 6, dayOffset + 6)  #should be blank
            sflag <- substr(myData[eachLine], dayOffset + 7, dayOffset + 7)  #should not be blank
            
            # write to ouput
            if (qflag == " " & sflag != " ") {
                writeLines(paste(yrVar, moVar, eachDay, metVal, sep = ","), 
                  outFile)
            }
        }
    }
}
close(outFile)
```


Read back data for summary graphs


```r
prcp <- read.csv(paste(myId, ".txt", sep = ""), header = FALSE, sep = ",", as.is = TRUE)
colnames(prcp) <- c("yr", "mo", "day", "val")
prcp$val <- prcp$val/100  #convert hundredths of inches to inches
```


Graphs ...


```r
# yearly total
yrtot <- aggregate(val ~ yr, data = prcp, FUN = sum)
png(filename = "fig1.png")
plot(yrtot$val ~ yrtot$yr, type = "h", main = "Annual Precipitation Total (inches), Livermore, CA", 
    ylab = "inches/year", xlab = "year")
garbage <- dev.off()

# monthly total
montot <- aggregate(val ~ yr + mo, data = prcp, FUN = sum)
png(filename = "fig2.png")
boxplot(montot$val ~ montot$mo, range = 0, main = "Monthly Precipitation Total (inches), Livermore, CA", 
    ylab = "inches/month", xlab = "calendar month")
garbage <- dev.off()

# number of rainy days per month, rainy day of rain amount > 0.01 inches
prcp$val <- ifelse(prcp$val <= 0.01, 0, 1)
raindays <- aggregate(val ~ yr + mo, data = prcp, FUN = sum)
png(filename = "fig3.png")
boxplot(raindays$val ~ raindays$mo, range = 0, main = "Monthly Rainy Days, Livermore, CA", 
    ylab = "days/month", xlab = "calendar month")
garbage <- dev.off()
```

![fig1][fig1]
![fig2][fig2]
![fig3][fig3]

Summary
========
This code, when modified slightly, could also be used to read GHCN daily precipitation data.

[fig1]: fig1.png "fig1"
[fig2]: fig2.png "fig2"
[fig3]: fig3.png "fig3"
