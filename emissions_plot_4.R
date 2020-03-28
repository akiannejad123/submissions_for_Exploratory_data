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


NEI_SCC <-dplyr::inner_join(NEI, SCC_df1, by= "SCC")


NEI_SCC_Sum_by_type <-aggregate( Emissions~type +year+SCC.Level.Two, data=NEI_SCC, FUN=sum)

NEI_SCC_Sum_by_type$year <-as.factor(NEI_SCC_Sum_by_type$year)
png("plot4.png")
plot4<-ggplot(NEI_SCC_Sum_by_type, aes(year, Emissions, group=SCC.Level.Two))+
  geom_point(aes(color=type))+
  labs(title="Emissions by Type for Baltimore County",
       x="Year")
 plot4 + facet_wrap(.~ SCC.Level.Two)
dev.off()

#################