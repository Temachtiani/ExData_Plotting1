plot1 <- function(){
  
  # LOAD LIBRARIES
  library(data.table);
  
  # GET DATA FROM LOCAL FILE (SUBSETING DATES 2007-02-01 AND 2007-02-02)
  dat <- subset(as.data.table(read.table("./EDA_data/household_power_consumption.txt", sep=";", header=TRUE)), Date=="1/2/2007" | Date=="2/2/2007");
  
  # TRANSFORM VARIABLE TYPES
  dat <- transform(dat, Date = as.Date(paste0(substr(dat$Date,5,8), "-", substr(dat$Date,3,3), "-", substr(dat$Date,1,1))), Time = format(strptime(dat$Time, "%H:%M:%OS"), "%H:%M:%S"), Global_active_power = as.numeric(as.character(dat$Global_active_power)), Global_reactive_power = as.numeric(as.character(dat$Global_reactive_power)), Global_intensity = as.numeric(as.character(dat$Global_intensity)), Sub_metering_1 = as.numeric(as.character(dat$Sub_metering_1)), Sub_metering_2 = as.numeric(as.character(dat$Sub_metering_2)), Sub_metering_3 = as.numeric(as.character(dat$Sub_metering_3)));
  dat$Date <- weekdays(dat$Date, abbreviate=TRUE);
    
  # PLOT 1: HIST OF GLOBAL ACTIVE POWER
  opar <- par(mar=c(4.5,4.5,2,1));
  with(dat, hist(Global_active_power, col="red", main="", xlab=""));
  title(main="Global Active Power");
  title(xlab="Global Active Power (kilowatts)");
  par(opar);
  
  # SAVE PLOT
  png("./EDA_out/plot1.png", width=480, height=480);
  opar <- par(mar=c(4.5,4.5,2,1));
  with(dat, hist(Global_active_power, col="red", main="", xlab=""));
  title(main="Global Active Power");
  title(xlab="Global Active Power (kilowatts)");
  par(opar);
  dev.off();
  
}