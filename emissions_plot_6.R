#######################################
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

NEI <-readRDS("summarySCC_PM25.rds")

NEI_PM2.5 <-NEI%>%
  na.omit(Emissions, year)

SCC_df2 <-SCC_df%>%
  filter(Data.Category =="Onroad")

NEI_SCC_2 <-dplyr::inner_join(NEI, SCC_df2, by= "SCC")

NEI_SCC_2_fips_compare <-NEI_SCC_2%>%
  filter(fips=="24510"| fips=="06037")

NEI_SCC_2_compare_2<-NEI_SCC_2_fips_compare%>%
  separate(SCC.Level.Two, c("Road_type", "Vehicles", "Fuel_Type"))
NEI_SCC_2_compare_3 <-NEI_SCC_2_compare_2%>%
  separate(SCC.Level.Three, c("Duty_Type","Type", "Fuel", "Class", "Abbreviation"))
NEI_SCC_2_compare_3<-NEI_SCC_2_compare_3%>%
  select(-EI.Sector,-Type, -Abbreviation)
NEI_SCC_2_compare_3$Class[is.na(NEI_SCC_2_compare_3$Class)] <- as.character(NEI_SCC_2_compare_3$Duty_Type[is.na(NEI_SCC_2_compare_3$Class)])  

#NEI_SCC_2_compare_3$year<- as.factor(NEI_SCC_2_compare_3$year)


NEI_SCC_2_fips_3_Sums <-aggregate(Emissions~year+Fuel_Type+Class+fips, data=NEI_SCC_2_compare_3, FUN=sum)

NEI_SCC_2_fips_Totals <-aggregate(Emissions~year+Fuel_Type+fips, data=NEI_SCC_2_fips_3_Sums, FUN=sum)


png("plot6.png") 
plot6 <- ggplot(data=NEI_SCC_2_fips_3_Sums, aes(as.factor(year), Emissions, group=Fuel_Type))+
  geom_point(aes(col=Class))+
  labs(ylab="Years")+
  #scale_x_log10() + scale_y_log10()+
  
  labs(title="Comparision of Vehicles Emissions of LA and Baltimore Counties",
       x="Year")
plot6+ facet_grid(vars(Fuel_Type), vars(fips))
dev.off()