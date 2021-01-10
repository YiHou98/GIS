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
library(plyr)
library(dplyr)
library(lubridate)
library(ggpubr)
library(janitor)
library(fs)
library(xtable)
library(Matrix)
library(dplyr)
library(reshape)
library(RColorBrewer)
library(lubridate)
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


plot(LWard_nb, st_geometry(coordsW), col="red")

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")
Z <- listw2mat(Lward.queens_weight)

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

plotlist=list()
for (i in 1:9){
  gg<- ggplot(filter(heatdata,RGN19NM == Region[i]), aes(date, areaName,fill=norm)) + geom_tile(aes(fill=norm))  + scale_fill_distiller(palette = "Spectral") +  scale_x_date(limits=lims,breaks =as.Date(c("2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b") +labs(x =NULL, y = NULL,title=sprintf("%s",Region[i]))+
    coord_fixed(ratio=5,expand = TRUE)+
    theme(axis.ticks=element_blank())+
    theme(axis.text.y=element_text(size=9,hjust=1))+
    theme(axis.text.x = element_text(size=9))+
    theme(panel.border = element_blank())+
    theme(plot.title=element_text(hjust =0,size=15))+
    theme(legend.title = element_blank())+
    theme(legend.title.align=1)+
    theme(legend.text=element_text(size=9))+
    theme(legend.position = 'bottom')+
    theme(legend.key.size=unit(0.2,'cm'))+
    theme(legend.key.width=unit(1,"cm"))
  plotlist[[i]] = gg
}

ggarrange(plotlist = plotlist,ncol=3,nrow=3)%>%
  ggexport(filename='final_1228_england2.pdf',height =15,width=15)
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

baselinedata <- heatdata%>%
  filter(`date` == as.Date('2020-09-01'))
baselinedata <-baselinedata%>%
  dplyr::rename(
    cumCasesBySpecimenDate_base = cumCasesBySpecimenDate,
    cumDeathsByDeathDate_base=cumDeathsByDeathDate,
    cumDeathsByPublishDate_base = cumDeathsByPublishDate,
    newCasesByPublishDate_base = newCasesByPublishDate
  )



## the relation between new cases and mobility--spatial autogression model
nlspd = mobilty_covid%>%
  select(areaCode,date,newCasesByPublishDate,newCasesByPublishDateChangePercentage,retail_and_recreation_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline,parks_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,RGN19NM)
#since there are many null values for mobility data before 0918,we will set the start data as 0918
nlspd <- nlspd%>%
  filter(date >='2020-09-18'& date <= '2020-12-27')
## As we mentioned before, we will remove 17&46
nlspd <- nlspd%>%
  filter(!areaCode=='E06000017')%>%
  filter(!areaCode=='E06000046')
#we will see how many 
sapply(nlspd,function(x)sum(is.na(x)))
# we find that residential percent has about 71 null values while the parks has 1292 null values, so we will remove parks percent change from our independent variables.
nlspd[is.na(nlspd)] <- 0

#change names of Z 
name =baselinedata['areaCode']
name <- name%>%
  filter(!areaCode=='E06000017')%>%
  filter(!areaCode=='E06000046')

colnames(Z) <- c(name$areaCode)
rownames(Z) <-c(name$areaCode)


## spatial panel model

modeldata <- nlspd%>%
  mutate(area = as.factor(nlspd$RGN19NM))

modeldata <-modeldata%>%
  dplyr::rename(
    retail_recreation = retail_and_recreation_percent_change_from_baseline,
    transit_stations=transit_stations_percent_change_from_baseline,
    workplaces = workplaces_percent_change_from_baseline,
    residential = residential_percent_change_from_baseline,
    grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline
  )
modeldata$area = as.factor(modeldata$area)

summary(modeldata)
new_cases <- scale(modeldata$newCasesByPublishDate)
modeldata <- modeldata%>%
  mutate(new_cases = new_cases)

##model1:random model 
fm_fixed_norm<- new_cases ~ retail_recreation + transit_stations + workplaces+grocery_pharmacy +residential
SARmodel_fixed_norm<- spml(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), model ='random',lag=TRUE,spatial.error="none")
##model2: fixed model:use new cases rate as the dependent variable

fm_fixed_rate <- newCasesByPublishDateChangePercentage ~ retail_recreation + transit_stations + workplaces+grocery_pharmacy +residential
SARmodel_fixed_rate <- spml(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), model ='within',lag=FALSE,spatial.error="b")

summary(SARmodel_fixed_norm)
summary(SARmodel_fixed_rate)
##Hausman test
print(hausman_panel<-phtest(fm_fixed_norm, data=modeldata))
print(hausman_panel<-phtest(fm_fixed_rate, data=modeldata))
##Hausman spatial test
print(spat_hausman_ML_SEM<-sphtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), spatial.model = "error", method="ML"))
print(spat_hausman_ML_SAR<-sphtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z),spatial.model = "lag", method="ML"))
print(spat_hausman_ML_SEM<-sphtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), spatial.model = "error", method="ML"))
print(spat_hausman_ML_SAR<-sphtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z),spatial.model = "lag", method="ML"))
## robust LM test
slmtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), test="lml",model="within")
slmtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), test="lme",model="within")
slmtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), test="rlml",model="within")
slmtest(fm_fixed_rate, data=modeldata,listw=mat2listw(Z), test="rlme",model="within")
slmtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), test="lml",model="random")
slmtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), test="lme",model="random")
slmtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), test="rlml",model="random")
slmtest(fm_fixed_norm, data=modeldata,listw=mat2listw(Z), test="rlme",model="random")


## Regional differences of COVID-19 trends and mobility patterns
## This part we will remove rustland since the absence of data. 
lockdowncoviddata <- mobilty_covid%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')%>%
  filter(!areaCode=='E06000017')%>%
  group_by(RGN19NM,date)%>%
  summarise(cumCases = sum(cumCasesBySpecimenDate),newCases = sum(newCasesByPublishDate))

lockdowncoviddata <- lockdowncoviddata%>%
  dplyr::rename(Region = RGN19NM)

##plot covid data
region_list<- list(c('North East','North West','Yorkshire and The Humber'),c('East Midlands','West Midlands','South West'),c('East of England','South East','London'))

for (i in 1:3){
  gg<- ggplot(lockdowncoviddata[lockdowncoviddata$Region %in% region_list[[i]],],aes( x= date, y = newCases))+
    geom_line(aes(color = Region),size =0.8)+
    scale_x_date(breaks =as.Date(c("2020-09-01","2020-10-01","2020-11-01","2020-12-01")),date_labels = "%b")+
    labs(y = "New Cases")+
    ylim(0,12000)+
    geom_vline(xintercept= as.Date('2020-11-05'),linetype=4,color = 'red')+
    geom_vline(xintercept= as.Date('2020-12-02'),linetype=4,color = 'red')+
    scale_color_manual(values = c('#e41a1c',
                                  '#377eb8',
                                  '#4daf4a'
                                  ))+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_blank())+
    theme(legend.text=element_text(size=16))+
    theme(legend.position="bottom")
  regionlist[[i]] = gg
}
regionlist[2]
for (i in 1:3){
  gg<- ggplot(lockdowncoviddata[lockdowncoviddata$Region %in% region_list[[i]],],aes( x= date, y = cumCases))+
    geom_line(aes(color = Region),size =1)+
    scale_x_date(breaks =as.Date(c("2020-09-01","2020-10-01","2020-11-01","2020-12-01")),date_labels = "%b")+
    labs(y = "Cumulative Cases")+
    ylim(0,3.7e+05)+
    geom_vline(xintercept= as.Date('2020-11-05'),linetype=4,color = 'red')+
    geom_vline(xintercept= as.Date('2020-12-02'),linetype=4,color = 'red')+
    scale_color_manual(values = c('#e41a1c',
                                  '#377eb8',
                                  '#4daf4a'
    ))+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_blank())+
    theme(legend.text=element_text(size=14))+
    theme(legend.position="bottom")
  regionlist[[i+3]] = gg
}
regionlist[6]
ggarrange(plotlist = regionlist,ncol=3,nrow=2,labels=c('(a)','(b)','(c)','(d)','(e)','(f)'))%>%
  ggexport(filename='final_regionlist3.pdf',height =12,width=18)


##plot mobility data
mobilitylist = list()

lockdownmobilitydata <- mobilty_covid%>%
  filter(date >= '2020-09-18'&date<='2020-12-28')%>%
  filter(!areaCode=='E06000017')
lockdownmobilitydata[is.na(lockdownmobilitydata)] <- 0
lockdownmobilitydata <- lockdownmobilitydata%>%
  group_by(RGN19NM,date)%>%
  summarise(averetail = mean(retail_and_recreation_percent_change_from_baseline),avegrocery=mean(grocery_and_pharmacy_percent_change_from_baseline),avetransit=mean(transit_stations_percent_change_from_baseline),averesid=mean(residential_percent_change_from_baseline),averwork=mean(workplaces_percent_change_from_baseline))


mobilitylist= list()

col_names=colnames(lockdownmobilitydata)
col_names = col_names[3:7]
lockdownmobilitydata$date = lockdownmobilitydata$date-7
mobility_list<- list('retail_recreation','transit_stations','residential','workplaces','grocery_pharmacy')
for (i in col_names){
  gg<- ggplot(lockdownmobilitydata,aes_string( x= lockdownmobilitydata$date, y = i))+
    geom_line(aes(color = RGN19NM),size =1)+
    scale_x_date(breaks =as.Date(c("2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b")+
    geom_vline(xintercept= as.Date('2020-11-05'),linetype=4,color = 'red')+
    geom_vline(xintercept= as.Date('2020-12-02'),linetype=4,color = 'red')+
    scale_color_manual(values = c('#e41a1c',
                                  '#377eb8',
                                  '#4daf4a',
                                  '#984ea3',
                                  '#ff7f00',
                                  '#ffff33',
                                  '#a65628',
                                  '#f781bf',
                                  '#999999'
    ))+
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),size=20),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_blank())+
    theme(legend.text=element_text(size=18))+
    theme(legend.position="bottom")
  mobilitylist[[i]] = gg
}
mobilitylist[[1]]
ggarrange(plotlist=mobilitylist,ncol = 2,nrow=3,labels=c('(a)','(b)','(c)','(d)','(e)'),font.label = list(size=20,color='black',face='bold'),legend = c('right'),common.legend = TRUE)%>%
  ggexport(filename='final_mobilitylist2.pdf',height =20,width=20)
