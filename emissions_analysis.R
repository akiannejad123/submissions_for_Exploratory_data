library(tidyverse)
library(stats)
library(sqldf)
install.packages("data.table")

library(data.table)
setwd ("C:/Users/sherw/OneDrive/Desktop/Exploratory_data")

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


#create png here

#copy.png(Plot1.png)
dev.copy(plot1.png)
plot( as.numeric(NEI_PM2.5_sums$Year), NEI_PM2.5_sums$x, xlab = "Year", ylab="Total PM25 Particles (ppm)")
abline(lm(NEI_PM2.5_sums$Year ~ NEI_PM2.5_sums$x))
title("Total Emissions per year")
dev.off()

#################
#create new file here
NEI_PM2.5_by_fips <-NEI%>%
  na.omit(Emissions, year)%>%
  filter(fips=="24510")

NEI_PM2.5_by_fips_sums <-  aggregate(NEI_PM2.5_by_fips$Emissions, by=list(Year=NEI_PM2.5_by_fips$year), FUN=sum)



dev.copy("plot2.png")
plot( NEI_PM2.5_by_fips_sums$Year, NEI_PM2.5_by_fips_sums$x, xlab = "Year", ylab="Total PM25 Particles (ppm)")
abline(lm(NEI_PM2.5_by_fips_sums$Year ~ NEI_PM2.5_by_fips_sums$x))
title("Total Emissions per year for Baltimore")
dev.off()




#############################
#Create new file here

NEI_PM2.5_by_fips <-NEI%>%
  na.omit(Emissions, year)%>%
  filter(fips=="24510")

NEI_PM2.5_by_fips_type <-aggregate( Emissions~type +year, data=NEI_PM2.5_by_fips, FUN=sum)
###create plot3.png 

g <-ggplot(data=NEI_PM2.5_by_fips_type, aes(x=year, y=Emissions, col=type))+
  geom_point()+
  geom_smooth(method="lm", na.rm=TRUE, se=FALSE)
g +facet_wrap(.~type)
# be sure to add the lm line for each chart

# device.off 

#####################
NEI_SCC <-dplyr::inner_join(NEI, SCC_df1, by= "SCC")


NEI_SCC_Sum_by_type <-aggregate( Emissions~type +year+SCC.Level.Two, data=NEI_SCC, FUN=sum)
################
#create png
plot4<-ggplot(NEI_SCC_Sum_by_type, aes(year, Emissions, group=SCC.Level.Two))+
    geom_point(aes(fill=type))

plot4 + facet_wrap(.~ SCC.Level.Two)
#device off
#################
SCC_df2 <-SCC_df%>%
  filter(Data.Category =="Onroad")

NEI_SCC_2 <-dplyr::inner_join(NEI, SCC_df2, by= "SCC")


NEI_SCC_2_Baltamore <-NEI_SCC_2%>%
  filter(fips=="24510")

NEI_SCC_2_Baltamore_2<-NEI_SCC_2%>%
  separate(SCC.Level.Two, c("Road_type", "Vehicles", "Fuel_Type"))
NEI_SCC_2_Baltamore_3 <-NEI_SCC_2_Baltamore_2%>%
  separate(SCC.Level.Three, c("Duty_Type","Type", "Fuel", "Class", "Abbreviation"))
NEI_SCC_2_Baltamore_3<-NEI_SCC_2_Baltamore_3%>%
  select(-EI.Sector,-Type, -Abbreviation)
NEI_SCC_2_Baltamore_3$Class[is.na(NEI_SCC_2_Baltamore_3$Class)] <- as.character(NEI_SCC_2_Baltamore_3$Duty_Type[is.na(NEI_SCC_2_Baltamore_3$Class)])  

NEI_SCC_2_Baltamore_3_Sums <-aggregate(Emissions~year+Fuel_Type+Class, data=NEI_SCC_2_Baltamore_3, FUN=sum)
NEI_SCC_2_Baltamore_Totals <-aggregate(Emissions~year+Fuel_Type, data=NEI_SCC_2_Baltamore_3_Sums, FUN=sum)

fit_1 <- lm(formula= year, Fuel_Type + Emissions, data=NEI_SCC_2_Baltamore_3_Sums)

#create png 
plot5<-ggplot(data=NEI_SCC_2_Baltamore_3_Sums, aes(year, Emissions, group=Class))+
  geom_point( aes(col=Class) )+
  geom_point(data=NEI_SCC_2_Baltamore_Totals, aes(y=Emissions,x=year, group=Fuel_Type))+
  #scale_x_log10() + scale_y_log10()+
  geom_smooth(method="lm", se=FALSE)
  plot5+facet_grid(.~Fuel_Type)
  
  #device off 
  
  #######################################
  
  NEI_SCC_2_fips_compare <-NEI_SCC_2%>%
    filter(fips=="24510"| fips=="06037")
  
  NEI_SCC_2_compare_2<-NEI_SCC_2_fips_compare%>%
    separate(SCC.Level.Two, c("Road_type", "Vehicles", "Fuel_Type"))
  NEI_SCC_2_compare_3 <-NEI_SCC_2_compare_2%>%
    separate(SCC.Level.Three, c("Duty_Type","Type", "Fuel", "Class", "Abbreviation"))
  NEI_SCC_2_compare_3<-NEI_SCC_2_compare_3%>%
    select(-EI.Sector,-Type, -Abbreviation)
  NEI_SCC_2_compare_3$Class[is.na(NEI_SCC_2_compare_3$Class)] <- as.character(NEI_SCC_2_compare_3$Duty_Type[is.na(NEI_SCC_2_compare_3$Class)])  
  
  
  
  
  
  NEI_SCC_2_fips_3_Sums <-aggregate(Emissions~year+Fuel_Type+Class+fips, data=NEI_SCC_2_compare_3, FUN=sum)
  NEI_SCC_2_fips_Totals <-aggregate(Emissions~year+Fuel_Type+fips, data=NEI_SCC_2_fips_3_Sums, FUN=sum)
  
  
  
plot6 <- ggplot(data=NEI_SCC_2_fips_3_Sums, aes( as.factor(year), Emissions, group=Fuel_Type))+
  geom_point(aes(col=fips))
  #scale_x_log10() + scale_y_log10()+
  plot6+ facet_grid(.~Class)