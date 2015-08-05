plot4 <- function(data_url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  data_dir = "./data"){
    #load library to work with dates and times
    library(lubridate)
    
    #prepare a folder for data
    if(file.exists(data_dir)) unlink(data_dir,recursive = TRUE)
    dir.create(data_dir)
    
    #donwload data
    data_zip <- file.path(data_dir,"temp.zip")
    download.file(data_url, destfile = data_zip)
    unzip(data_zip, exdir = data_dir)
    file.remove(data_zip)
    
    #read data
    data_file <- dir(data_dir,full.names = TRUE)[[1]]
    dta <- read.table(data_file, header = TRUE, sep = ";", na.strings = "?", nrows = 100)
    classes <- sapply(dta,class) 
    dta <- read.table(data_file, header = TRUE, sep = ";", na.strings = "?", colClasses = classes)
    
    #convert to date and time format
    dta$Date <- dmy(dta$Date)
    dta$Time <- hms(dta$Time)
    dta$datetime <- dta$Date+dta$Time
    
    #subset data
    time_period <- interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 23:59:59"))
    dta <- dta[dta$datetime %within% time_period,]
    
    #make plots
    png(file = "plot4.png", width = 480, height = 480)
    par(mfrow = c(2,2))
    plot(dta$datetime,dta$Global_active_power,
         xlab = "" ,
         ylab = "Global Active Power",
         type = "l", lwd = 1)
    with(dta,plot(datetime,Voltage,type = "l"))
    plot(dta$datetime,dta$Sub_metering_1,
         type = "n",
         xlab = "",
         ylab = "Energy sub metering")
    lines(dta$datetime,dta$Sub_metering_1, lwd = 1)
    lines(dta$datetime,dta$Sub_metering_2, lwd = 1, col = "red")
    lines(dta$datetime,dta$Sub_metering_3, lwd = 1, col = "blue")
    legend("topright",
           lty = 1,
           bty = "n",
           col = c("black","red","blue"),
           legend = c("Sub_metering_1",
                      "Sub_metering_2",
                      "Sub_metering_3"))
    with(dta,plot(datetime,Global_reactive_power,type = "l"))
    dev.off()
}