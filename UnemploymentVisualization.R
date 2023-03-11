library(tidyverse)
library(readxl)
library(ggpubr)
library(rworldmap)
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(raster)
library(grid)
library(plotly)
library(viridis)
library(data.table)
#Import required datasets
JobExcel1<-read_excel("C:\\Users\\Sai Aishwarya\\Downloads\\Jobs_excel\\JobsEXCEL.xlsx")
edu_basic<-read_excel("C:\\Users\\Sai Aishwarya\\Downloads\\unemployment_basic.xls")
edu_inter<-read_excel("C:\\Users\\Sai Aishwarya\\Downloads\\unemployment_intermediate.xls")
edu_adv<-read_excel("C:\\Users\\Sai Aishwarya\\Downloads\\unemployment_advanced.xls")

names(JobExcel1)[1]<-"Country_Name"
names(JobExcel1)[2]<-"Country_Code"
names(JobExcel1)[3]<-"Indicator"
names(JobExcel1)[30]<-"Year_2016"
names(JobExcel1)[31]<-"Trend"

#Filter regions for time series
world_regions<-c("EAS","MEA","SSF","ECS","SAS","WLD","NAC","LCN")
WorldRegions_df<- JobExcel1 %>% filter(Country_Code %in% world_regions)
WorldRegions_df<-WorldRegions_df %>% filter(Indicator == 'Unemployment, total (% of total labor force) (modeled ILO estimate)')
WorldRegions_df <- WorldRegions_df[-c(2,3,4,31,32,33,34)]
names(WorldRegions_df)[27]<-"2016"
headers <- WorldRegions_df$Country_Name
WorldRegions_df <- as.data.frame(t(WorldRegions_df[,-1]))
colnames(WorldRegions_df) <- headers
WorldRegions_df$myfactor <- factor(row.names(WorldRegions_df))
#WorldRegions_df <- WorldRegions_df[-c(9)]
names(WorldRegions_df)[9]<-"Year"

#Filter out regions from countries
not_countries<-c("EAP","EAS","ECA","ECS","HIC","HPC","LDC","LIC","LMC","LMY","MEA","MIC","MNA","UMC","WLD")
JobExcel1<- JobExcel1 %>% filter(!Country_Code %in% not_countries)
View(JobExcel1)

#Basic education dataset
names(edu_basic)[1]<-"Country_Name"
names(edu_basic)[2]<-"Country_Code"
names(edu_basic)[3]<-"Indicator"
names(edu_basic)[61]<-"Edu_Basic"
edu_basic <-edu_basic[-c(1, 2, 3), ] 
edu_basic<- edu_basic %>% dplyr::select(Country_Code, Edu_Basic)
edu_basic <- edu_basic[!rowSums(is.na(edu_basic)) > 0,]
View(edu_basic)

#Intermediate education dataset
names(edu_inter)[1]<-"Country_Name"
names(edu_inter)[2]<-"Country_Code"
names(edu_inter)[3]<-"Indicator"
names(edu_inter)[61]<-"Edu_Intermediate"
edu_inter <-edu_inter[-c(1, 2, 3), ] 
edu_inter<- edu_inter %>% dplyr::select(Country_Code, Edu_Intermediate)
edu_inter <- edu_inter[!rowSums(is.na(edu_inter)) > 0,]
View(edu_inter)

#Advanced education dataset
names(edu_adv)[1]<-"Country_Name"
names(edu_adv)[2]<-"Country_Code"
names(edu_adv)[3]<-"Indicator"
names(edu_adv)[64]<-"Edu_Advanced"
edu_adv <-edu_adv[-c(1, 2, 3), ] 
edu_adv<- edu_adv %>% dplyr::select(Country_Code, Edu_Advanced)
edu_adv <- edu_adv[!rowSums(is.na(edu_adv)) > 0,]
View(edu_adv)

#Employment dataset
JobExcel<-JobExcel1 %>% filter(Indicator == 'Unemployment, total (% of total labor force) (modeled ILO estimate)')
JobExcel<- JobExcel %>% dplyr::select(Country_Name,Country_Code, Year_2016)
JobExcel <- JobExcel[!rowSums(is.na(JobExcel)) > 0,]
names(JobExcel)[3]<-"Unemployment_rate"
View(JobExcel)

#Merge education and unemployment data
edu_merged <- merge(JobExcel, edu_basic, by="Country_Code")
edu_merged <- merge(edu_merged, edu_inter, by="Country_Code")
edu_merged <- merge(edu_merged, edu_adv, by="Country_Code")
View(edu_merged)


#Stack Bar Chart
target <- c("Norway", "UK","North America","Switzerland","Sweden","Canada","Ireland","Italy","Armenia","Spain","Greece","South Africa")
edu_merged1<- edu_merged %>% filter(Country_Name %in% target)
vline <- function(x = 0, color = "grey") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}
fig1 <- plot_ly(edu_merged, x = ~edu_merged1$Country_Name,type = 'bar', colors = 'rgb(216,179,101)') %>% 
  layout(xaxis = list(categoryorder = "total ascending"))
fig1 <- fig1 %>% add_trace(y = ~edu_merged1$Edu_Basic, name = 'Edu_Basic',
                           marker = list(color = 'rgb(252,141,89)',
                                         line = list(color = 'rgb(8,48,107)', width = 0.5)))
fig1 <- fig1 %>% add_trace(y = ~edu_merged1$Edu_Intermediate, name = 'Edu_Intermediate',
                           marker = list(color = 'rgb(255,255,191)',
                                         line = list(color = 'rgb(8,48,107)', width = 0.5)))
fig1 <- fig1 %>% add_trace(y = ~edu_merged1$Edu_Advanced, name = 'Edu_Advanced',
                           marker = list(color = 'rgb(145,191,219)',
                                         line = list(color = 'rgb(8,48,107)', width = 0.5)))
fig1 <- fig1 %>% layout(title = "Level of Education among unemployed across various countries",xaxis = list(title = 'Country',tickangle = -90), yaxis = list(title = 'Education level of Unemployed'), barmode = 'stack')
fig1



#Scatter Plot
gdp_data<-JobExcel1 %>% filter(Indicator == 'GDP per capita, PPP (constant 2011 international $)')
gdp_data<-gdp_data%>% dplyr::select(Country_Code, Year_2016)
gdp_data <- gdp_data[!rowSums(is.na(gdp_data)) > 0,]
names(gdp_data)[2]<-"GDP"
gdp_data$GDP_Level =
  case_when(gdp_data$GDP <= 10095 ~ "Low", 
            (gdp_data$GDP > 10096 & gdp_data$GDP<22695) ~ "Average",
            gdp_data$GDP >= 22696 ~ "High")
View(gdp_data)

gdp_merged <- merge(JobExcel, gdp_data, by="Country_Code")
View(gdp_merged)
ggplot(gdp_merged, aes(x=GDP, y=Unemployment_rate, color = GDP_Level)) +
  ggtitle("Unemployment rate in countries by GDP per capita")+xlab("GDP per capita")+ylab("Unemployment rate")+
  geom_point(alpha = 0.6, size =1.8)+ theme_bw()+guides(col=guide_legend(title="GDP category"))+
  geom_smooth(se=FALSE, fullrange=TRUE, size=0.5) + scale_colour_viridis_d()#+ geom_point(aes(size=Literacy),alpha = 0.5)



#Choropleath for world unemployment rate
world_map <- map_data("world")
world_map <- subset(world_map, region!="Antarctica")

gg <- ggplot(JobExcel)
gg <- gg + geom_map(dat=world_map, map = world_map, aes(map_id=region), 
                    fill="grey", color="#7f7f7f", size=0.25)
gg <- gg + geom_map(map = world_map, aes(map_id = Country_Name, fill = Unemployment_rate), size=0.25)
gg <- gg + expand_limits(x = world_map$long, y = world_map$lat)
gg <- gg + labs(x="", y="", title="World unemployment rate in 2016")
gg <- gg + theme(panel.grid=element_blank(), panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank(), axis.text=element_blank())
gg <- gg + theme(legend.position="top")+ borders("world",colour = "white",) 
gg<-gg + scale_fill_viridis(direction = -1)+guides(col=guide_legend(title="Unemployment rate"))
gg


#Time series line chart for different regions
ts<-ggplot()+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = World, group = 1, colour = "black"), size=1.8) + scale_color_manual(labels = c("Sub-Saharan Africa","South Asia","North America","East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East & North Africa","World"), values = c("#4575B4", "#FEE090","#D73027","#F46D45","#FDAE61","#ABD9E9","#74ADD1","black"))+guides(col=guide_legend(title="World Regions"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `East Asia & Pacific`, group = 1, colour="#D73027"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `Europe & Central Asia`, group = 1, colour="#F46D45"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `Latin America & Caribbean`, group = 1, colour="#FDAE61"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `Middle East & North Africa`, group = 1, colour="#FEE090"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `North America`, group = 1, colour="#ABD9E9"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `South Asia`, group = 1, colour="#74ADD1"))
ts<-ts+geom_line(data=WorldRegions_df, mapping=aes(x=Year, y = `Sub-Saharan Africa`, group = 1, colour="#4575B4"))+ geom_smooth(se=FALSE)
ts<-ts+ theme_bw()+ggtitle("World Unemployment rate trend from 1991 - 2016")+ylab("Unemployment rate")+scale_x_discrete(breaks = seq(1991, 2016, by=5)) +geom_vline(aes(xintercept = "2008"),linetype = 4, colour = "black")+annotate(geom="text", x=3, y=30, label="2008 Economic recession",color="white")
ts
