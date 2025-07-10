path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
              , destfile = paste(path, "dataFiles.zip", sep = "/"))
unzip(zipfile = "dataFiles.zip")

NEI <- readRDS(file.path(getwd(), "summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(), "Source_Classification_Code.rds"))

### 1 total Emissions for each years
df_plot1 <- tapply(NEI$Emissions, NEI$year, sum)
df_plot1 <- data.frame(year = names(df_plot1), Emissions = as.numeric(df_plot1)) #1st method to get a total emissions data.frame
df_plot1 <- aggregate(Emissions ~ year, data = NEI, sum) #2nd method to get a total Emissions data.frame

png("Course4_Project2_plot1.png")

par(mfrow = c(1,2), oma = c(0, 0, 5, 0), mar = c(5, 5, 0, 2))
barplot(df_plot1[["Emissions"]], names = df_plot1[,"year"], xlab = "Year", ylab = "Total Emissions")

with(df_plot1, plot(year, Emissions, type = "p", pch = 19, xlab = "Year", ylab = "Total Emissions", xaxt = "n", col = "blue", cex = 1.3))
with(df_plot1, lines(year, Emissions, lwd = 2, col = "red"))
axis(side = 1, at = df_plot1$year)

title(main = "Total PM2.5 emission", outer = TRUE)

dev.off()

### 2 Total Emissions in Baltimore City, Maryland

df_plot2 <- aggregate(Emissions ~ year, data = NEI[NEI$fips == "24510", ], sum)

png("Course4_Project2_plot2.png")

par(mfrow = c(1,2), oma = c(0, 0, 5, 0), mar = c(5, 5, 0, 2))

barplot(df_plot2$Emissions, names = df_plot2[["year"]], xlab = "Year", ylab = "Total Emissions")
with(df_plot2, plot(year, Emissions, pch = 19, type = "o", cex = 1.2, xlab = "Year", ylab = "Total Emissions"))
title(main = "Total Emissions in Balitmore City, Maryland", outer = T)

dev.off()

### 3 Four types total emissions in Baltimore City
library(ggplot2)
df_plot3 <- NEI[NEI$fips == "24510",]

png("Course4_Project2_plot3.png")

ggplot(data = df_plot3, aes(factor(year), Emissions, fill = type))+geom_col()+facet_grid(.~type)+
        guides(fill=FALSE)+theme_bw()+
        labs(x="year", y=expression("Total PM"[2.5]*" Emission")) + 
        labs(title=("Total Emissions, Baltimore City by Source Type"))

dev.off()

### 4 Emissions from coal combustion-related sources changed from 1999-2008
SCC_combcol <- SCC[grepl("Combustion", SCC$SCC.Level.One, ignore.case = TRUE) & (grepl("Coal", SCC$SCC.Level.Two, ignore.case = TRUE) | grepl("Coal", SCC$SCC.Level.Three, ignore.case = TRUE) | grepl("Coal", SCC$SCC.Level.Four, ignore.case = TRUE)),]
df_plot4 <- merge(NEI, SCC_combcol, by = "SCC")[, c("SCC", "Emissions", "year")]

png("Course4_Project2_plot4.png")

ggplot(df_plot4, aes(factor(year), Emissions/10^5))+geom_col()+labs(x = "Year", y = expression("Total PM"[2.5]*" Emission"))+
        labs(title = "Emissions from coal combustion-related sources changed from 1999-2008")

dev.off()

### 5 Emissions from motor vehicle sources changed from 1999–2008 in Baltimore City
NEI_Baltimore <- NEI[NEI$fips == "24510",]
vehicle_source <- paste((unique(SCC$SCC.Level.Two)[c(9, 11, 12, 13, 16, 60, 68, 72, 76, 111)]), collapse = "|")
SCC_mvs <- SCC[grepl(vehicle_source, SCC$SCC.Level.Two),]
df_plot5 <- merge(NEI_Baltimore, SCC_mvs, by = "SCC")[,c("SCC", "Emissions", "year")]

png("Course4_Project2_plot5.png")

ggplot(df_plot5, aes(x = factor(year), y = Emissions))+geom_col()+labs(x = "Year", y = "Total Emissions")+
        labs(title = ("Emissions from motor vehicle sources changed from 1999–2008 in Baltimore City"))

dev.off()

### 6 Baltimore City Vs. Los Angeles County, Compare emissions from motor vehicle sources 
NEI_Baltimore_LA <- NEI[NEI$fips %in% c("24510", "06037"),]
NEI_Baltimore_LA$City <- ifelse(NEI_Baltimore_LA$fips == "24510", "Baltimore City", "Los Angeles")
vehicle_source <- paste((unique(SCC$SCC.Level.Two)[c(9, 11, 12, 13, 16, 60, 68, 72, 76, 111)]), collapse = "|")
SCC_mvs <- SCC[grepl(vehicle_source, SCC$SCC.Level.Two),]
df_plot6 <- merge(NEI_Baltimore_LA, SCC_mvs, by = "SCC")[,c("City", "SCC", "Emissions", "year")]

png("Course4_Project2_plot6.png")

ggplot(df_plot6, aes(factor(year), Emissions/10^5, fill = City))+
        geom_col(aes(fill=year))+
        facet_grid(.~City)+
        labs(x = "Year", y = expression("Total PM"[2.5]* " Emissions"))+
        labs(title = "Baltimore City Vs. Los Angeles County, Compare emissions from motor vehicle sources")

ggplot(df_plot6, aes(factor(year), Emissions/10^5, fill = City))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Year", y = expression("Total PM"[2.5]* " Emissions"))+
  labs(title = "Baltimore City Vs. Los Angeles County, Compare emissions from motor vehicle sources")


dev.off()