library(sf)
library(here)
library(tmap)
library(tmaptools)
library(tidyverse)
library(stringr)
library(spdep)
library("plm")
library('splm')
library(hrbrthemes)
library(viridis)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(janitor)
library(fs)
library(xtable)
library(Matrix)
library(reshape)
coviddata <- read.csv('utla_covid.csv')
coviddata$date <- as.Date(coviddata$date)
data <- coviddata%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')
coviddataengland <- read.csv('utla_england.csv')
coviddataengland$date <- as.Date(coviddataengland$date)
dataengland <- coviddataengland%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')

Regiondata <- read.csv('Local_Authority_District_to_Region.csv')
UTLAMap <- read_sf(dsn = '.', layer = "Counties_and_Unitary_Authorities__December_2019__Boundaries_UK_BUC")

data1 <- data%>%
  merge(
    .,Regiondata,by.x = 'areaCode',
    by.y = 'LAD19CD'
  )
data1england <- dataengland%>%
  merge(
    .,Regiondata,by.x = 'areaCode',
    by.y = 'LAD19CD'
  )

peakdata <- data1england %>%
  filter(date == '2020-12-28')
UTLAMap <- UTLAMap%>%
  merge(.,peakdata, by.x = 'ctyua19cd',
        by.y = 'areaCode',
        no.dups = TRUE)
##we found that Rustland has some null values, and 0056 dosen't have neighbourhoods
UTLAMap1 <- UTLAMap%>%
  filter(!ctyua19cd == 'E06000017')%>%
  filter(!ctyua19cd == 'E06000046')
coordsW <- UTLAMap1%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW)
LWard_nb <- UTLAMap1 %>%
  poly2nb(., queen=T)
knn_wards <-coordsW %>%
  knearneigh(., k=4)
LWard_knn <- knn_wards %>%
  knn2nb()

## plot peak data in england
UTLALondon <- UTLAMap %>%
  filter(str_detect(`ctyua19cd`,"^E09"))
tmap_mode("plot")
qtm(UTLAMap, fill = 'cumCasesBySpecimenDate')

qtm(UTLALondon, fill = 'cumCasesBySpecimenDate')

## Figure1
plot(LWard_nb, st_geometry(coordsW), col="red")

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")
W <- nb2listw(LWard_nb1, glist = NULL, style = "C",zero.policy=TRUE)

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

##load mobility data the newest date is 12-27, not 12-28
mobilitydata <- read.csv('Region_Mobility.csv')
mobilitydata$date <- as.Date(mobilitydata$date)
mobilitydata <- mobilitydata%>%
  filter(date >='2020-09-01'& date <= '2020-12-27')
##draw heatmap to see the covid cases evolution trend of each area
heatmap <- data1england %>%
  dplyr::mutate(
    Month = lubridate::month(date),
    Monthtag = factor(Month,levels = as.character(9:12),
                      labels=c("Sep","Oct","Nov","Dec"),ordered = TRUE))

heatdata <- heatmap %>%
  group_by(`areaName`)%>%
  mutate(norm = (newCasesByPublishDate-min(newCasesByPublishDate))/(max(newCasesByPublishDate)-min(newCasesByPublishDate)))
mobilty_covid <- heatdata%>%
  merge(.,mobilitydata,by=c('areaName','date'),no.dups=TRUE)
mobilty_covid1<- mobilty_covid%>%
  merge(.,y =UTLAMap[ ,c('ctyua19cd','long','lat','geometry')],by.x='areaCode',by.y='ctyua19cd')

Region = unique(heatdata$RGN19NM)

heatmapdata <-ddply(data, c("date","areaCode"),summarize,.())

lims=c(as.Date('2020-09-01'),as.Date('2020-12-28'))
plotlist=list()
for (i in 1:9){
   gg<- ggplot(filter(heatdata,RGN19NM == Region[i]), aes(date, areaName,fill=norm)) + geom_tile(aes(fill=norm))  + scale_fill_distiller(palette = "Spectral") +  scale_x_date(limits=lims,breaks =as.Date(c("2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b") +labs(x =NULL, y = NULL,title=sprintf("%s",Region[i]))+
     coord_fixed(ratio=5,expand = TRUE)+
     theme(axis.ticks=element_blank())+
     theme(axis.text.y=element_text(size=6,hjust=1))+
     theme(axis.text.x = element_text(size=6))+
     theme(panel.border = element_blank())+
     theme(plot.title=element_text(hjust =0,size=12))+
     theme(legend.title = element_blank())+
     theme(legend.title.align=1)+
     theme(legend.text=element_text(size=6))+
     theme(legend.position = 'bottom')+
     theme(legend.key.size=unit(0.2,'cm'))+
     theme(legend.key.width=unit(1,"cm"))
  plotlist[[i]] = gg
}
ggarrange(plotlist = plotlist,ncol=3,nrow=3)%>%
  ggexport(filename='final_1228_england.pdf',height =15,width=15)

normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}
standard <- function(x){
  return(sqrt(sum((x-mean(x))^2/(length(x)-1))))
}

baselinedata <- heatdata%>%
  filter(`date` == as.Date('2020-09-01'))
baselinedata <-baselinedata%>%
  rename(
    cumCasesBySpecimenDate_base = cumCasesBySpecimenDate,
    cumDeathsByDeathDate_base=cumDeathsByDeathDate,
    cumDeathsByPublishDate_base = cumDeathsByPublishDate,
    newCasesByPublishDate_base = newCasesByPublishDate
  )


####woc please don't load plyr after dplyr
## the relation between new cases and mobility--spatial autogression model


nlspd = mobilty_covid%>%
  select(areaCode,date,newCasesByPublishDate,newCasesByPublishDateChangePercentage,retail_and_recreation_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline,parks_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,RGN19NM)
#since there are many null values for mobility data before 0918,we will set the start data as 0918
nlspd <- nlspd%>%
  filter(date >='2020-09-18'& date <= '2020-12-27')
#we will see how many 
sapply(nlspd,function(x)sum(is.na(x)))
## As we mentioned before, we will remove 17&46
nlspd <- nlspd%>%
  filter(!areaCode=='E06000017')%>%
  filter(!areaCode=='E06000046')
#we will see how many 
sapply(nlspd,function(x)sum(is.na(x)))
# we find that residential percent has about 71 null values while the parks has 1292 null values, so we will remove parks percent change from our independent variables.
nlspd[is.na(nlspd)] <- 0
Z[is.na(Z)] <- 0
#change names of Z 
name =baselinedata['areaCode']
name <- name%>%
  filter(!areaCode=='E06000017')%>%
  filter(!areaCode=='E06000046')

colnames(Z) <- c(name$areaCode)
rownames(Z) <-c(name$areaCode)

## the two standard specifications (SEM and SAR) one with FE
## and the other with RE:
## fixed effects panel with spatial errors


nlspd4 <- nlspd%>%
  mutate(area = as.factor(nlspd$RGN19NM))
summary(nlspd4)


##model1:fixed model 
fm <- newCasesByPublishDate ~ retail_and_recreation_percent_change_from_baseline + transit_stations_percent_change_from_baseline + workplaces_percent_change_from_baseline+residential_percent_change_from_baseline+area-1
SARmodel3 <- spml(fm, data=nlspd4,listw=mat2listw(Z), model ='random',lag=TRUE,spatial.error="none")
fm_fixed <- newCasesByPublishDate ~ retail_and_recreation_percent_change_from_baseline + transit_stations_percent_change_from_baseline + workplaces_percent_change_from_baseline+residential_percent_change_from_baseline
SARmodel_fixed <- spml(fm_fixed, data=nlspd4,listw=mat2listw(Z), model ='within',lag=TRUE,spatial.error="none")
SARmodel_fixed1 <- spml(fm_fixed, data=nlspd4,listw=mat2listw(Z2), model ='within',lag=TRUE,spatial.error="b")
##model2: fixed model:use new cases rate as the dependent variable
fm_fixed_rate <- newCasesByPublishDateChangePercentage ~ retail_and_recreation_percent_change_from_baseline + transit_stations_percent_change_from_baseline + workplaces_percent_change_from_baseline+residential_percent_change_from_baseline+grocery_and_pharmacy_percent_change_from_baseline
SARmodel_fixed_rate <- spml(fm_fixed_rate, data=nlspd4,listw=mat2listw(Z), model ='within',lag=TRUE,spatial.error="b")
SARmodel_random_rate <- spml(fm_fixed_rate, data=nlspd4,listw=mat2listw(Z), model ='random',lag=TRUE,spatial.error="none")

summary(SARmodel3)
summary(SARmodel_fixed1)
summary(SARmodel_fixed_rate)
summary(SARmodel_random_rate)

##From the result of spatial panel model, we can see all kinds of mobility are significant.

## Next part we will focus on the effects of lockdown. The above findings suggest that mobility trends can really affect the new covid cases.
## This motivates us to study if lockdown can really help decrease new cases.
# 2.1 This part we will remove rustland since the absence of data. 
## during this period, people will decrease time for recreation, and time in residence will increase
lockdowncoviddata <- mobilty_covid%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')%>%
  filter(!areaCode=='E06000017')%>%
  group_by(RGN19NM,date)%>%
  summarise(cumCases = sum(cumCasesBySpecimenDate),newCases = sum(newCasesByPublishDate))
library(RColorBrewer)
ggplot(lockdowncoviddata,aes( x= date, y = newCases))+
  geom_line(aes(color = RGN19NM),size =1)+
  scale_color_manual(values = c('#a6cee3',
    '#1f78b4',
    '#b2df8a',
    '#33a02c',
    '#fb9a99',
    '#e31a1c',
    '#fdbf6f',
    '#ff7f00',
    '#cab2d6'))+
  theme_minimal()
ggplot(lockdowncoviddata,aes( x= date, y = newCases))+
  geom_line(aes(color = RGN19NM),size =0.8)+
  scale_color_manual(values = c("#bea33c",
                                "#5c388b",
                                "#6ca24d",
                                "#ca74c6",
                                "#46c19a",
                                "#b2457c",
                                "#b86838",
                                "#6d80d8",
                                "#b8444e"))+
  theme_minimal()

ggplot(lockdowncoviddata,aes( x= date, y = newCases))+
  geom_line(aes(color = RGN19NM),size =0.8)+
  scale_color_manual(values = c("#ca586e",
                                "#4aab83",
                                "#c74fb1",
                                "#7ea342",
                                "#7d67d0",
                                "#c18b41",
                                "#678dcd",
                                "#cb5336",
                                "#b76ea7"))+
  theme_minimal()
ggplot(lockdownmobilitydata,aes( x= date, y = averetail))+
  geom_line(aes(color = RGN19NM),size =1)+
  scale_color_manual(values = c('#e41a1c',
    '#377eb8',
    '#4daf4a',
    '#984ea3',
    '#ff7f00',
    '#ffff33',
    '#a65628',
    '#f781bf',
    '#999999'))+
  theme_minimal()

lockdowndata[is.na(lockdowndata)] <- 0
lockdownmobilitydata <- mobilty_covid%>%
  filter(date >= '2020-09-18'&date<='2020-12-28')%>%
  filter(!areaCode=='E06000017')
brewer.pal(9, "BrBG")
lockdownmobilitydata[is.na(lockdownmobilitydata)] <- 0
lockdownmobilitydata <- lockdownmobilitydata%>%
  group_by(RGN19NM,date)%>%
  summarise(averetail = mean(retail_and_recreation_percent_change_from_baseline),avegrocery=mean(grocery_and_pharmacy_percent_change_from_baseline),avetransit=mean(transit_stations_percent_change_from_baseline),averesid=mean(residential_percent_change_from_baseline),averwork=mean(workplaces_percent_change_from_baseline))
## plot 4 plots of Sep Oct Nov of each region's covid cases, death cases,new cases. I think different regions have different peaks. For example, London has highest cases in Dec
##from this plot, we can see the absolute number of regions, while the heatmap can tell us the peak of each region
## plot the new cases before lockdown, during lockdown and after lockdown by MAP AND frequency plot

##plot the mobility before and after lockdown'

##Third Part: focus on london boroughs, why some areas have far few cases than others(Camden)
