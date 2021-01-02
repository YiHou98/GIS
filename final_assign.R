library(sf)
library(here)
library(tmap)
library(tmaptools)
library(tidyverse)
library(stringr)
library(spdep)
library("plm")
library('splm')
coviddata <- read.csv('utla_covid.csv')
coviddata$date <- as.Date(coviddata$date)
data <- coviddata%>%
  filter(date >='2020-08-01'& date <= '2020-12-24')
coviddataengland <- read.csv('utla_england.csv')
coviddataengland$date <- as.Date(coviddataengland$date)
dataengland <- coviddataengland%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')


data
df1 =unique(data1$areaName)
write.csv(df1,'areaname.csv')
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

Regiondata <- read.csv('Local_Authority_District_to_Region.csv')
peakdata <- data1england %>%
  filter(date == '2020-12-28')
UTLAMap <- read_sf(dsn = '.', layer = "Counties_and_Unitary_Authorities__December_2019__Boundaries_UK_BUC")


coordsW1 <- UTLAMap%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW)
LWard_nb1 <- UTLAMap %>%
  poly2nb(., queen=T)
knn_wards1 <-coordsW %>%
  knearneigh(., k=4)
LWard_knn1 <- knn_wards1 %>%
  knn2nb()
plot(LWard_nb, st_geometry(coordsW), col="red")
Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C",zero.policy=TRUE)
W <- nb2listw(LWard_nb1, glist = NULL, style = "C",zero.policy=TRUE)
?nb2listw
Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")
LWard_nb1 <- LonWardProfiles %>%
  poly2nb(., queen=T)
knn_wards <-coordsW1 %>%
  knearneigh(., k=4)
LWard_knn1 <- knn_wards %>%
  knn2nb()
plot(LWard_nb, st_geometry(coordsW), col="red")
Lward.queens_weight1 <- LWard_nb1 %>%
  nb2listw(., style="C",empty)
?nb2listw
UTLAMap <- UTLAMap%>%
  merge(.,baselinedata, by.x = 'ctyua19cd',
        by.y = 'areaCode',
        no.dups = TRUE)



mobilty_covid1<- mobilty_covid%>%
  merge(.,y =UTLAMap[ ,c('ctyua19cd','long','lat','geometry')],by.x='areaCode',by.y='ctyua19cd')
tmap_mode("plot")

qtm(UTLAMap, fill = 'cumCasesBySpecimenDate')
UTLALondon <- UTLAMap %>%
  filter(str_detect(`ctyua19cd`,"^E09"))

qtm(UTLALondon, fill = 'cumCasesBySpecimenDate')

#built heatmap
library(hrbrthemes)
library(viridis)
library(plotly)
library(ggplot2)
library(dplyr)

library(lubridate)
heatmap <- data1england %>%
  dplyr::mutate(
    Month = lubridate::month(date),
    Monthtag = factor(Month,levels = as.character(9:12),
                      labels=c("Sep","Oct","Nov","Dec"),ordered = TRUE))
ggplot(aes( fill ='newCasesByPublishDate' )) + 
  geom_tile(colour = "white") + 
  facet_grid(MonthTag) + 
  scale_fill_gradient(low="red", high="yellow") +
  labs(x="Month", y=NULL)
?scale_x_date
heatmapdata <-ddply(data, c("date","areaCode"),summarize,.())
lims1=c(as.Date('2020-09-01'),as.Date('2020-12-28'))
lims1

df = unique(heatdata3$RGN19NM)
df[1]
regionlist <- heatdata3 %>%
  group_by('RGN19NM')
plotlist=list()
for (i in 1:9){
   gg<- ggplot(filter(heatdata3,RGN19NM == df[i]), aes(date, areaName,fill=norm)) + geom_tile(aes(fill=norm))  + scale_fill_distiller(palette = "Spectral") +  scale_x_date(limits=lims1,breaks =as.Date(c("2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b") +labs(x =NULL, y = NULL,title=sprintf("%s",df[i]))+
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
plotlist[1]
pdf("test2.pdf")
?ggexport
library(ggpubr)
ggarrange(plotlist = plotlist,ncol=3,nrow=3)%>%
  ggexport(filename='final_1228_england.pdf',height =15,width=15)
plotlist[1]
pdf("test2.pdf")
print
dev.off()

ggsave("arrange1.pdf", arrangeGrob(grobs = cclist))
grid.draw(cclist)
cclist[['ncol']] <-2
do.call(grid.arrange,cclist)
library(gridExtra)
ggsave(file="whatever.pdf",cclist )
p <- ggplot(heatdata3, aes(date, areaName)) + geom_tile(color = 'white',size = 0.1)  + scale_fill_distiller(palette = "Spectral") +  scale_x_date(limits=lims,breaks =as.Date(c("2020-08-15","2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b") + xlab("Date") + ylab("") +
  coord_fixed(ratio=2.5)+
  facet_wrap(~RGN19NM,ncol=2)+
  theme(axis.text.y = element_text(size =2.5,angle=0,hjust=1),axis.text.x = element_text(size =3),axis.title = element_text(size = 4)) 






q <- ggplot(heatdata4, aes(date, areaName)) + geom_tile(aes(fill=norm))  + scale_fill_distiller(palette = "Spectral") +  scale_x_date(limits=lims,breaks =as.Date(c("2020-08-15","2020-09-15","2020-10-15","2020-11-15","2020-12-15")),date_labels = "%b") + xlab("Date") + ylab("") +
  coord_fixed(ratio=2.5)+
  theme(axis.text.y = element_text(size =2.5,angle=0,hjust=1),axis.text.x = element_text(size =3),axis.title = element_text(size = 4)) 

q
?ggplot
pdf("heatmap3.pdf")
grid.arrange(cclist)
dev.off()
ggplot(heatmap, aes(Monthtag, areaName)) + geom_tile(aes(fill=newCasesByPublishDate))
library(heatmaply)
heatmaply(
  normalize(matrix),
  xlab = "Date",
  ylab = "areaName",
  main = "raw",
  colors = "Spectral",
  k_col = 2,
  k_row = 2
  
)
?as.matrix
data("mtcars")
heatdata3[which('date' == as.Date('2020-08-01'))]

library(reshape)
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}
standard <- function(x){
  return(sqrt(sum((x-mean(x))^2/(length(x)-1))))
}
heatselect <- heatmap %>%
  select()
matrix <- cast(heatmap, areaCode ~ date,value ='newCasesByPublishDate' )
heatdata3 <- heatmap %>%
  group_by(`areaName`)%>%
  mutate(norm = (newCasesByPublishDate-min(newCasesByPublishDate))/(max(newCasesByPublishDate)-min(newCasesByPublishDate)))
baselinedata <- heatdata3%>%
  filter(`date` == as.Date('2020-09-01'))
baselinedata <-baselinedata%>%
  rename(
    cumCasesBySpecimenDate_base = cumCasesBySpecimenDate,
    cumDeathsByDeathDate_base=cumDeathsByDeathDate,
    cumDeathsByPublishDate_base = cumDeathsByPublishDate,
    newCasesByPublishDate_base = newCasesByPublishDate
  )
typeof(baselinedata$date)
heatdata3 <- heatdata3%>%
  merge(.,y=baselinedata[ ,c('areaCode','cumCasesBySpecimenDate_base','cumDeathsByDeathDate_base','cumDeathsByPublishDate_base','newCasesByPublishDate_base')],by = 'areaCode')
heatdata_filter <- heatdata3[!(grepl('Scotland',heatdata3$RGN19NM)|grepl('Northern Ireland',heatdata3$RGN19NM)), ]
mobilitydata <- read.csv('Region_Mobility.csv')
mobilitydata$date <- as.Date(mobilitydata$date)
mobilitydata <- mobilitydata%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')
mobilty_covid <- heatdata3%>%
  merge(.,mobilitydata,by=c('areaName','date'),no.dups=TRUE)
## day lag =14
mobilitydata14 <- read.csv('Region_Mobility 14.csv')
mobilitydata14$date <- as.Date(mobilitydata14$date)
mobilitydata14 <- mobilitydata14%>%
  filter(date >='2020-09-01'& date <= '2020-12-28')
mobilty_covid14 <- heatdata3%>%
  merge(.,mobilitydata14,by=c('areaName','date'),no.dups=TRUE)
?merge

heatdata4 <- heatmap %>%
  group_by(`areaName`)%>%
  mutate(norm = (newCasesByPublishDate-mean(newCasesByPublishDate))/(max(newCasesByPublishDate)-min(newCasesByPublishDate)))
?group_by
####woc please don't load plyr after dplyr
typeof(heatdata3$date)
?mutate
write.csv(heatdata3,'heatdataengland.csv')
getwd()                                                                       
?select
detach(package:plyr)
### effect of lockdown on different regions
### spatial autogression model
library(janitor)
library(fs)
extradata <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)
#add the extra data too
LonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles, 
            by = c("GSS_CODE" = "New code"))
LonWardProfiles <- LonWardProfiles %>%
  clean_names()

LonWardProfiles <- LonWardProfiles%>%
  left_join(., 
            extradata, 
            by = c("gss_code" = "Wardcode"))%>%
  clean_names()
Londonwards<-dir_info(here::here("", 
                                 "statistical-gis-boundaries-london", 
                                 "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()
#print some of the column names
LonWardProfiles%>%
  names()%>%
  tail(., n=10)
LWard_nb1 <- LonWardProfiles %>%
  poly2nb(., queen=T)
Lward.queens_weight <- LWard_nb1 %>%
  nb2listw(., style="C")
p <- ggplot(LonWardProfiles, 
            aes(x=unauth_absence_schools11, 
                y=average_gcse_capped_point_scores_2014))
p + geom_point(aes(colour = inner_outer)) 
isitfactor <- LonWardProfiles %>%
  dplyr::select(inner_outer)%>%
  summarise_all(class)

isitfactor
LonWardProfiles<- LonWardProfiles %>%
  mutate(inner_outer=as.factor(inner_outer))
library("plm")
library('splm')
coordsW1 <- LonWardProfiles%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW1)
library(spdep)
library(xtable)
nlspd = mobilty_covid%>%
  select(areaCode,date,newCasesByPublishDate,newCasesByPublishDateChangePercentage,retail_and_recreation_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline,parks_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,RGN19NM)
nlspd <- nlspd%>%
  filter(date >='2020-09-18'& date <= '2020-12-28')
nlspd14 <-mobilty_covid14%>%
  select(areaCode,date,newCasesByPublishDate,newCasesByPublishDateChangePercentage,retail_and_recreation_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline,parks_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,RGN19NM)
nlspd14 <- nlspd14%>%
  filter(date >='2020-09-18'& date <= '2020-12-28')
nlspd4[is.na(nlspd4)] <- 0
Z[is.na(Z)] <- 0

tbl=xtable(nlspd1)
library(Matrix)

W <- mat2listw(as(B, "dgTMatrix"))
Z <- listw2mat(Lward.queens_weight)
W <-listw2mat(Lward.knn_4_weight)
Z1 <- listw2mat(Lward.queens_weight1)
Z2 <- listw2mat(Lward.knn_4_weight)
all.equal(W$neighbours, LWard_nb, check.attributes=FALSE)
all.equal(attr(W$neighbours, "region.id"), attr(W$neighbours, "region.id"))


summary(SARmodel3)
summary(SARmodel_fixed1)
summary(SARmodel_fixed_rate)
name =baselinedata['areaCode']
name2 <- name%>%
  filter(!areaCode=='E06000017')
nlspd14 <- nlspd14%>%
  filter(!areaCode=='E06000017')


colnames(Z2) <- c(name1$areaCode)
rownames(Z2) <-c(name1$areaCode)
fm11 <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
## the two standard specifications (SEM and SAR) one with FE
## and the other with RE:
## fixed effects panel with spatial errors
fespaterr <- spml(fm11, data = Produc, listw = mat2listw(usaww),
                  model="within", spatial.error="b", Hess = FALSE)
typeof(Produc$year)
write.csv(nlspd2,'clean2.csv')
arealist <- pdata.frame(UTLAMap)
write.csv(arealist,'arealist.csv')
area =UTLAMap$ctyua19cd
arealist=peakdata$areaCode
sum14 = nlspd14%>%
  group_by(areaCode)%>%
  summarise(count=n())
unique(nlspd2$areaCode)
###delete E600046&17

nlspd4 <- nlspd%>%
  mutate(area = as.factor(nlspd$RGN19NM))
summary(nlspd4)
###check how many null values of each column of nlspd4
##we find that parks_percent has too many null values
nlspd4[is.na(nlspd4)] <- 0
nlspd14 = nlspd4%>%
  select(!date)
Z[is.na(Z)] <- 0
sapply(nlspd4,function(x)sum(is.na(x)))
library(xtable)
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
## effects of lockdown
## during this period, people will decrease time for recreation, and time in residence will increase
lockdowndata <- mobilty_covid%>%
  filter(date >='2020-10-29'& date <= '2020-12-28')
lockdowndata[is.na(lockdowndata)] <- 0
lockdowndata <- lockdowndata%>%
  filter(!areaCode=='E06000017')%>%
  group_by(RGN19NM,date)%>%
  summarise(averetail = mean(retail_and_recreation_percent_change_from_baseline),avegrocery=mean(grocery_and_pharmacy_percent_change_from_baseline),avetransit=mean(transit_stations_percent_change_from_baseline),averesid=mean(residential_percent_change_from_baseline),averwork=mean(workplaces_percent_change_from_baseline))
## plot 4 plots of Sep Oct Nov of each region's covid cases, death cases,new cases. I think different regions have different peaks. For example, London has highest cases in Dec
##from this plot, we can see the absolute number of regions, while the heatmap can tell us the peak of each region
## plot the new cases before lockdown, during lockdown and after lockdown by MAP AND frequency plot

##plot the mobility before and after lockdown'

