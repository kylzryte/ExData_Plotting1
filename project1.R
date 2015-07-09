###############################################################################
# Electric Power Consumption Data Set
# Measurements of electric power consumption in one household with a 
# one- minute sampling rate over a period of 4 years. Different electrical 
# quantitites and some sub metering values are below. 
#
#       
#       Date: Date in format dd/mm/yyyy
#       
#       Time: time in format hh:mm:ss
#       
#       Global_active_power: household global minute-averaged active 
#                            power (in kilowatt)
#       
#       Global_reactive_power: household global minute-averaged 
#                              reactive power (in kilowatt)
#       
#       Voltage: minute-averaged voltage (in volt)
#       
#       Global_intensity: household global minute-averaged current 
#                         intensity (in ampere)
#       
#       Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active 
#                       energy). It corresponds to the kitchen, containing 
#                       mainly a dishwasher, an oven and a microwave 
#                       (hot plates are not electric but gas powered).
#       
#       Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active 
#                       energy). It corresponds to the laundry room, 
#                       containing a washing-machine, a tumble-drier, 
#                       a refrigerator and a light.
#       
#       Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active 
#                       energy). It corresponds to an electric 
#                       water-heater and an air-conditioner.

# load misc. libraries
library(data.table)
library(dplyr)
library(lubridate)

# load large txt file
file <- "household_power_consumption.txt"
dat <- fread(file, sep=";", header=TRUE, stringsAsFactors=FALSE, verbose=TRUE)

# look for NA's coerced by fread
sapply(dat, function(x) sum(is.na(x)))


# change Date column to date format
dat$Date <- as.Date(dat$Date, "%d/%m/%Y")

# subset large data from on dates, verify using head() tail()  
data <- dat[dat$Date >= "2007-02-01" & dat$Date <= "2007-02-02", ]
head(data$Date)
tail(data$Date)

# remove large data frame 
rm(dat)

# data.table was giving me a headache, so convert to data.frame
data <- data.frame(data)

data$Date.time <- ymd_hms(paste(data$Date, data$Time))

# change the character classes to numeric for columns 3:9
        for (i in 3:9) {
                data[,i] <- as.numeric(data[,i])
        }

# look for NA's
sapply(data, function(x) sum(is.na(x)))

# make column vector of weekdays

data$Weekdays <- weekdays(data$Date.time)

###############################################################################
#
#  Base Plotting
#
# 
# The first is a histogram of Global Active Power
# The only real snag here was getting the 1200 tick mark on the 
# Y axis to display. It was done by usis the axis() function
png("plot1.png")
hist(data$Global_active_power, xlab="Global Active Power (kilowatts)",
     col="red", main="Global Active Power", ylim=c(0,1200))
axis(side=2, at=1200)
dev.off()

# The second plot is a line plot of Global Active power over time. The x 
# axis is broken by day of week.
png("plot2.png")
plot(data$Date.time,data$Global_active_power, type="n", xlab="", 
     ylab="Global Active Power (kilowatts)")
lines(data$Date.time, data$Global_active_power)
dev.off()
# The third plot is a multiple line plot of the three sub-meterings plotted
# against each other. 
png("plot3.png")
plot(data$Date.time, data$Sub_metering_1, type="n", xlab="",
     ylab="Energy sub metering")
lines(data$Date.time, data$Sub_metering_1, col="Black")
lines(data$Date.time, data$Sub_metering_2, col="Red")
lines(data$Date.time, data$Sub_metering_3, col="Blue")
legend("topright", 
        legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
        col=c("black","red","blue"),
        lwd=c(2,2,2),
        lty=c(1,1,1))
dev.off()
# The fourth plot is a 2x2 of the following:
# (1,1) Global Active Power by Date Time
# (1,2) Voltage by Date Time
# (2,1) Energy Sub by Date Time
# (2,2) Global Reactive power by Date Time
# 
# set 2x2, set margins, set text size
png("plot4.png")
par(mfrow=c(2,2), mar=c(4,4,2,2), cex=0.75)
# plot 1
plot(data$Date.time,data$Global_active_power, type="n", xlab="", 
     ylab="Global Active Power")
lines(data$Date.time, data$Global_active_power)
#plot2
plot(data$Date.time, data$Voltage, type="n", xlab="datetime", ylab="Voltage")
lines(data$Date.time, data$Voltage)
#plot 3
plot(data$Date.time, data$Sub_metering_1, type="n", xlab="",
     ylab="Energy sub metering")
lines(data$Date.time, data$Sub_metering_1, col="Black")
lines(data$Date.time, data$Sub_metering_2, col="Red")
lines(data$Date.time, data$Sub_metering_3, col="Blue")
legend("topright", 
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       col=c("black","red","blue"),
       lwd=c(2,2,2), bty="n",
       lty=c(1,1,1), cex=0.75)
# plot 4
plot(data$Date.time, data$Global_reactive_power, type="n", xlab="datetime", 
        ylab="Global_reactive_power")
axis(side=2, at=c(0.1,0.3,0.5))
lines(data$Date.time, data$Global_reactive_power, col="Black")
dev.off()


