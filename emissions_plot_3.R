
#Create new file here
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


NEI_PM2.5_by_fips <-NEI%>%
  na.omit(Emissions, year)%>%
  filter(fips=="24510")

NEI_PM2.5_by_fips_type <-aggregate( Emissions~type +year, data=NEI_PM2.5_by_fips, FUN=sum)
###create plot3.png 
png("Plot3.png")
g <-ggplot(data=NEI_PM2.5_by_fips_type, aes(x=as.factor(year), y=Emissions, col=type))+
  geom_point()+
  geom_smooth(method="lm", na.rm=TRUE, se=FALSE)+
  ggtitle("Pollution Type for Baltimore from 1999-2008")
g +facet_wrap(.~type)

dev.off()

#####################