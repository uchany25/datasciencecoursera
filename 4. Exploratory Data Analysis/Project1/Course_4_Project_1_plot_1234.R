Sys.setlocale("LC_TIME", "English")

df <- data.table::fread("household_power_consumption.txt")

df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")

df <- df[df$Date == "2007-02-01" | df$Date == "2007-02-02",]

df$date_time = as.POSIXct(paste(df$Date, df$Time), format ="%Y-%m-%d %H:%M:%S")

# Plot 1
png(filename = file.path(getwd(), "plot1.png"), width = 480, height = 480, units = "px")
hist(as.numeric(df$Global_active_power), col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()


# Plot 2
png(filename = file.path(getwd(), "plot2.png"), width = 480, height = 480, units = "px")
plot(df$date_time, df$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt ="n")
desired_dates <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"), format = "%Y-%m-%d")
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)
dev.off()

# Plot 3
png(filename = file.path(getwd(), "plot3.png"), width = 480, height = 480, units = "px")
plot(df$date_time, df$Sub_metering_1, type = "l", xaxt = "n", xlab = "", ylab = "Energy sub metering")
lines(df$date_time, df$Sub_metering_2, type = "l", col = "red", xaxt = "n")
lines(df$date_time, df$Sub_metering_3, type = "l", col = "blue", xaxt = "n")
legend("topright", col = c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),lty=c(1,1), lwd=c(1,1), cex = 0.6)
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)
dev.off()


# plot 4
png(filename = file.path(getwd(), "plot4.png"), width = 480, height = 480, units = "px")
par(mfcol = c(2,2), mar = c(4,4,1,2))
plot(df$date_time, df$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt ="n")
desired_dates <- as.POSIXct(c("2007-02-01", "2007-02-02", "2007-02-03"), format = "%Y-%m-%d")
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)

plot(df$date_time, df$Sub_metering_1, type = "l", xaxt = "n", xlab = "", ylab = "Energy sub metering")
lines(df$date_time, df$Sub_metering_2, type = "l", col = "red", xaxt = "n")
lines(df$date_time, df$Sub_metering_3, type = "l", col = "blue", xaxt = "n")
legend("topright", col = c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),lty=c(1,1), lwd=c(1,1), cex = 0.5)
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)

plot(df$date_time, df$Voltage, type = "l", xaxt = "n", ylab = "Voltage", xlab = "datetime")
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)

plot(df$date_time, df$Global_reactive_power, type = "l", xaxt = "n", ylab = "Global_reactive_power", xlab = "datetime")
axis.POSIXct(side = 1, x = desired_dates, format = "%a", at = desired_dates)

dev.off()
