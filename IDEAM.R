library("lubridate")
raw2page <- function(rawdata) {
# Takes a vector of chars, one page of data, returns a tidy dataframe
# Template for the page header
yearbound <- c(5,60,63)
stationbound <- c(5,105,112)
latbound <- c(7,16,19)
longbound <- c(8,16,19)
deptobound <- c(7,81,101)
municipiobound <- c(8,81,101)

framebounds <- rbind(yearbound,stationbound,latbound,longbound,deptobound,municipiobound)
colnames(framebounds) <- c("line","start","end")
framebounds <- as.data.frame(framebounds)

framedata <- data.frame()
framedata <- as.data.frame(rbind(with(framebounds, substr(rawdata[line],start,end))))
colnames(framedata) <- c("year","station","latitude","longitude","depto","municipio")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
framedata$depto <- trim(framedata$depto)
framedata$municipio <- trim(framedata$municipio)

# Make a column listing all dates of the year
st <- as.Date(paste(framedata[1]$year,"-01-01",sep=""))
en <- as.Date(paste(framedata[1]$year,"-12-31",sep=""))
date <- seq(as.Date(st),as.Date(en), by=1)
pagedata <- cbind(framedata,date)

# horizontal offsets for the last digit of each month (the last digit is aligned)
mboundaries<-c(25,34,43,52,61,70,79,88,97,106,115,124)
# now we can take the dates we generated before and use these coordinates to read the rainfall amount into a vector
rainfall <- as.numeric(substr(rawdata[14+mday(pagedata$date)],mboundaries[month(pagedata$date)]-6,mboundaries[month(pagedata$date)] ))
# and bind the vector to the page data to make a tidy data set 
page <- cbind(pagedata,rainfall)
page
}

raw <- readLines("area1.txt") # read in all the data

# Get all the page header line numbers
headers <- as.data.frame(grep("HIDROLOGIA", raw))
colnames(headers) <- c("linenum")

listOfDataFrames <- vector(mode = "list", length = nrow(headers))

# page by page, append onto the list
output <- data.frame()
for (i in 1:nrow(headers)) {
  start <- headers[i,]
  end <- start + 56
  listOfDataFrames[[i]] <- raw2page(raw[start:end])
      }
library("plyr")
output <- rbind.fill(listOfDataFrames)
print(summary(output))