##############################################################


library(tidyverse)
library(stats)
library(sqldf)
install.packages("data.table")

library(data.table)




SCC <-readRDS("Source_Classification_Code.rds")
names(SCC)
SCC_df<-SCC%>%
  select(SCC,Data.Category, EI.Sector, SCC.Level.One, SCC.Level.Two, SCC.Level.Three)


#dplyr::filter(SCC, pmatch("coal",EI.Sector))

SCC_df1<-SCC_df%>%
  filter (str_detect(EI.Sector, "Coal"))

NEI <-readRDS("summarySCC_PM25.rds")

NEI_PM2.5 <-NEI%>%
  na.omit(Emissions, year)

NEI_PM2.5_sums <-  aggregate(NEI_PM2.5$Emissions, by=list(Year=NEI_PM2.5$year), FUN=sum)
fit1

#create png here

#copy.png(Plot1.png)
png("plot1.png")
plot( NEI_PM2.5_sums$Year, NEI_PM2.5_sums$x, xlab = "Year", ylab="Total PM25 Particles (ppm)")
abline(lm(NEI_PM2.5_sums$Year~NEI_PM2.5_sums$x))
title("Total Emissions per year")
dev.off()

#################