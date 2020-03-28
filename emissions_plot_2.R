
#create new file here
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

NEI_PM2.5_by_fips <-NEI%>%
  na.omit(Emissions, year)%>%
  filter(fips=="24510")

NEI_PM2.5_by_fips_sums <-  aggregate(NEI_PM2.5_by_fips$Emissions, by=list(Year=NEI_PM2.5_by_fips$year), FUN=sum)



png("plot2.png")
plot( NEI_PM2.5_by_fips_sums$Year, NEI_PM2.5_by_fips_sums$x, xlab = "Year", ylab="Total PM25 Particles (ppm)")
abline(lm(NEI_PM2.5_by_fips_sums$Year ~ NEI_PM2.5_by_fips_sums$x))
title("Total Emissions per year for Baltimore")
dev.off()




#############################