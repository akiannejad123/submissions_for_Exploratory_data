

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

#fit_1 <- lm(formula= year, Fuel_Type + Emissions, data=NEI_SCC_2_Baltamore_3_Sums)

png("plot5.png")
plot5<-ggplot(data=NEI_SCC_2_Baltamore_3_Sums, aes(year, Emissions, group=Class))+
  geom_point( aes(col=Class) )+
  geom_point(data=NEI_SCC_2_Baltamore_Totals, aes(y=Emissions,x=year, group=Fuel_Type))+
  #scale_x_log10() + scale_y_log10()+
  geom_smooth(method="lm", se=FALSE)+
  labs(title="Comparision of Vehicles Emissisions for  Baltimore Counties",
  x="Year")
  plot5+facet_grid(.~Fuel_Type)
  
dev.off()
  
 