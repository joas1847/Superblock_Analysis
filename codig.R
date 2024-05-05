library(dplyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(stringr)
library(ggmap)
library(lattice)
library(RColorBrewer)
library(readxl)
options(digits=10)
#DATA IMPORT

setwd("C:/Users/jordi/OneDrive/Escritorio/JORDI/4t/TFG/datasets")
datasets = list.files(pattern="*.csv")
for (i in 1:length(datasets)) {
  if (i <= 5) {
    assign(datasets[i], read.csv(datasets[i],fileEncoding = "latin1"))
  } 
  else {
    assign(datasets[i], read.csv(datasets[i], fileEncoding = "UTF-8"))
  }
}
accidents2015 <- read.csv("C:/Users/jordi/OneDrive/Escritorio/JORDI/4t/TFG/accidents2015.csv", encoding="latin1",sep=";")
data_frames <- Filter(is.data.frame, mget(ls()))
new_names <- gsub("\\.csv$", "", names(data_frames))
names(data_frames) <- new_names
attach(data_frames)
rm(list=ls())


##PUT TOGHETHER DATAFRAMES
#sense latitud
names(accidents2022)[10]="Any"
names(accidents2020)[12]="Any"
names(accidents2019)[12]="Any"
colnames(accidents2021)=colnames(accidents2022)
colnames(accidents2010)=colnames(accidents2018[-c(26:27)])
colnames(accidents2011)=colnames(accidents2018[-c(26:27)])
colnames(accidents2012)=colnames(accidents2018[-c(26:27)])
colnames(accidents2013)=colnames(accidents2018[-c(26:27)])
colnames(accidents2014)=colnames(accidents2018[-c(26:27)])
colnames(accidents2015)=colnames(accidents2018[-c(26:27)])


accidents2022$Nom_barri[accidents2022$Nom_barri == "el Poble-sec"]= "el Poble Sec"
accidents2021$Nom_barri[accidents2021$Nom_barri == "el Poble-sec"]= "el Poble Sec"
accidents2020$Nom_barri[accidents2020$Nom_barri == "el Poble-sec"]= "el Poble Sec"
accidents2019$Nom_barri[accidents2019$Nom_barri == "el Poble-sec"]= "el Poble Sec"
accidents2018$Nom_barri[accidents2018$Nom_barri == "el Poble-sec"]= "el Poble Sec"
accidents2017$Nom_barri[accidents2017$Nom_barri == "el Poble-sec"]= "el Poble Sec"



## CREATE THE LATITUD AND LONGITUDE COORDINATES

accidents2015$Coordenada_UTM_Y=gsub(",",".",accidents2015$Coordenada_UTM_Y)
accidents2015$Coordenada_UTM_X=gsub(",",".",accidents2015$Coordenada_UTM_X)
accidents2015$Coordenada_UTM_X=as.numeric(accidents2015$Coordenada_UTM_X)
accidents2015$Coordenada_UTM_Y=as.numeric(accidents2015$Coordenada_UTM_Y)


ed50_proj <- CRS("+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0")
wgs84_proj <- CRS("+proj=longlat +datum=WGS84")

ed5015_coords <- cbind(accidents2015$Coordenada_UTM_Y,accidents2015$Coordenada_UTM_X)
ed5015_sp <- SpatialPoints(ed5015_coords, proj4string = ed50_proj)

wgs8415_sp <- spTransform(ed5015_sp, wgs84_proj)
wgs8415_coords <- coordinates(wgs8415_sp)

accidents2015$Latitud<- wgs8415_coords[,2]
accidents2015$Longitud  <- wgs8415_coords[,1]


accidents2014$Coordenada_UTM_Y=gsub(",",".",accidents2014$Coordenada_UTM_Y)
accidents2014$Coordenada_UTM_X=gsub(",",".",accidents2014$Coordenada_UTM_X)
accidents2014$Coordenada_UTM_X=as.numeric(accidents2014$Coordenada_UTM_X)
accidents2014$Coordenada_UTM_Y=as.numeric(accidents2014$Coordenada_UTM_Y)
accidents2014=accidents2014[-1,]

ed5014_coords <- cbind(accidents2014$Coordenada_UTM_Y,accidents2014$Coordenada_UTM_X)
ed5014_sp <- SpatialPoints(ed5014_coords, proj4string = ed50_proj)

wgs8414_sp <- spTransform(ed5014_sp, wgs84_proj)
wgs8414_coords <- coordinates(wgs8414_sp)

accidents2014$Latitud=wgs8414_coords[, 2]
accidents2014$Longitud <- wgs8414_coords[, 1]

accidents2013$Coordenada_UTM_Y=gsub(",",".",accidents2013$Coordenada_UTM_Y)
accidents2013$Coordenada_UTM_X=gsub(",",".",accidents2013$Coordenada_UTM_X)
accidents2013$Coordenada_UTM_X=as.numeric(accidents2013$Coordenada_UTM_X)
accidents2013$Coordenada_UTM_Y=as.numeric(accidents2013$Coordenada_UTM_Y)
accidents2013=accidents2013[-1,]

ed5013_coords <- cbind(accidents2013$Coordenada_UTM_Y,accidents2013$Coordenada_UTM_X)
ed5013_sp <- SpatialPoints(ed5013_coords, proj4string = ed50_proj)

wgs8413_sp <- spTransform(ed5013_sp, wgs84_proj)
wgs8413_coords <- coordinates(wgs8413_sp)

accidents2013$Latitud=wgs8413_coords[, 2]
accidents2013$Longitud <- wgs8413_coords[, 1]

accidents2012$Coordenada_UTM_Y=gsub(",",".",accidents2012$Coordenada_UTM_Y)
accidents2012$Coordenada_UTM_X=gsub(",",".",accidents2012$Coordenada_UTM_X)
accidents2012$Coordenada_UTM_X=as.numeric(accidents2012$Coordenada_UTM_X)
accidents2012$Coordenada_UTM_Y=as.numeric(accidents2012$Coordenada_UTM_Y)
accidents2012=accidents2012[-1,]

ed5012_coords <- cbind(accidents2012$Coordenada_UTM_Y,accidents2012$Coordenada_UTM_X)
ed5012_sp <- SpatialPoints(ed5012_coords, proj4string = ed50_proj)

wgs8412_sp <- spTransform(ed5012_sp, wgs84_proj)
wgs8412_coords <- coordinates(wgs8412_sp)

accidents2012$Latitud=wgs8412_coords[, 2]
accidents2012$Longitud<- wgs8412_coords[, 1]

accidents2011$Coordenada_UTM_Y=gsub(",",".",accidents2011$Coordenada_UTM_Y)
accidents2011$Coordenada_UTM_X=gsub(",",".",accidents2011$Coordenada_UTM_X)
accidents2011$Coordenada_UTM_X=as.numeric(accidents2011$Coordenada_UTM_X)
accidents2011$Coordenada_UTM_Y=as.numeric(accidents2011$Coordenada_UTM_Y)
accidents2011=accidents2011[-1,]

ed5011_coords <- cbind(accidents2011$Coordenada_UTM_Y,accidents2011$Coordenada_UTM_X)
ed5011_sp <- SpatialPoints(ed5011_coords, proj4string = ed50_proj)

wgs8411_sp <- spTransform(ed5011_sp, wgs84_proj)
wgs8411_coords <- coordinates(wgs8411_sp)

accidents2011$Latitud=wgs8411_coords[, 2]
accidents2011$Longitud<- wgs8411_coords[, 1]

accidents2010$Coordenada_UTM_Y=gsub(",",".",accidents2010$Coordenada_UTM_Y)
accidents2010$Coordenada_UTM_X=gsub(",",".",accidents2010$Coordenada_UTM_X)
accidents2010$Coordenada_UTM_X=as.numeric(accidents2010$Coordenada_UTM_X)
accidents2010$Coordenada_UTM_Y=as.numeric(accidents2010$Coordenada_UTM_Y)
accidents2010=accidents2010[-1,]

ed5010_coords <- cbind(accidents2010$Coordenada_UTM_Y,accidents2010$Coordenada_UTM_X)
ed5010_sp <- SpatialPoints(ed5010_coords, proj4string = ed50_proj)

wgs8410_sp <- spTransform(ed5010_sp, wgs84_proj)
wgs8410_coords <- coordinates(wgs8410_sp)

accidents2010$Latitud=wgs8410_coords[, 2]
accidents2010$Longitud<- wgs8410_coords[, 1]



listdf=list(accidents2022,accidents2021,accidents2020,accidents2019,accidents2018,accidents2017,accidents2016,accidents2015,accidents2014,accidents2013,accidents2012,accidents2011,accidents2010)
a=lapply(listdf,function(x){
  x=x |> 
    select(Numero_expedient,Nom_barri,Nom_districte,Nom_carrer,Any,Numero_victimes,Numero_morts,Numero_lesionats_lleus,Numero_lesionats_greus,Latitud,Longitud)
  return(x)  
})
grouped=data.frame(Reduce(rbind, a)) 
grouped=grouped[!grepl("Nk Any", grouped$Any, ignore.case = TRUE), ]

grouped=as_tibble(grouped)
grouped$Numero_morts=as.numeric(grouped$Numero_morts)
grouped$Numero_victimes=as.numeric(grouped$Numero_victimes)
grouped$Numero_lesionats_lleus=as.numeric(grouped$Numero_lesionats_lleus)
grouped$Numero_lesionats_greus=as.numeric(grouped$Numero_lesionats_greus)
grouped$prepost=ifelse(grouped$Any %in% c("2010","2011","2012","2013","2014","2015","2016"),"2010-2016","2017-2022")



#TIDE THE ENVIROMENT
objects<- c("grouped")
rm(list=setdiff(ls(), objects))



### GROUP ACCIDENTS AND TREATMENT ASSIGNMENT

pre=grouped |> 
  filter(Any%in%c("2010","2011","2012","2013","2014","2015","2016"))  
post= grouped |> 
  filter(Any%in%c("2017","2018","2019","2020","2021","2022"))  

grouped=grouped |> 
  filter(Nom_barri!="Desconegut")
 

accgrouped=grouped |> 
  filter(Nom_barri!="Desconegut") |> 
  group_by(Nom_barri,Any) |> 
  summarise(count=n())
accgrouped$Any=as.numeric(accgrouped$Any)

##AGREGGATE CONTROL VARIABLES
pob <- read_excel("C:/Users/jordi/Downloads/contV2.xls", sheet = "Poblacio")
pob=pivot_longer(pob, cols=c(colnames(pob[2:14])), names_to="Any", values_to= "Poblacio")
pob$Any=as.numeric(pob$Any)
pob$barris[pob$barris == "la Marina del Prat Vermell - Zona Franca"]= "la Marina del Prat Vermell"
pob$barris[pob$barris == "el Poble Sec - Parc Montjuïc"]= "el Poble Sec"




numvehicles <- read_excel("C:/Users/jordi/Downloads/contV2.xls", sheet = "vehicles")
vehicles=pivot_longer(numvehicles, cols=c(colnames(numvehicles[2:14])), names_to="Any", values_to= "Vehicles")
vehicles$Any=as.numeric(vehicles$Any)
vehicles$barris[vehicles$barris == "la Marina del Prat Vermell - Zona Franca"]= "la Marina del Prat Vermell"
vehicles$barris[vehicles$barris == "el Poble Sec - Parc Montjuïc"]= "el Poble Sec"


final=accgrouped |> 
  right_join(pob,by=c("Any","Nom_barri"="barris")) |> 
  right_join(vehicles,by=c("Any","Nom_barri"="barris")) |> 
  mutate(DID=ifelse(Any>=2017 & Nom_barri=="Sant Antoni",1,ifelse(Any>=2017 & Nom_barri=="el Parc i la Llacuna del Poblenou",1,0)))  
victimes=grouped |> 
  group_by(Any,Nom_barri) |>
  summarise(greus=sum(Numero_lesionats_greus),lleus=sum(Numero_lesionats_lleus),morts=sum(Numero_morts))
victimes$Any=as.numeric(victimes$Any)
final=final |> 
  left_join(victimes,by=c("Any","Nom_barri"))
#EXPORT TABLE TO CSV AND MAKE THE ANALYSIS IN STATA
write.csv(final, "C:\\Users\\jordi\\OneDrive\\Escritorio\\JORDI\\4t\\TFG\\tfgdata.csv", row.names=FALSE)

#GENERATE DATASETS TO DRAW PLOTS
asaa=grouped |>
  group_by(Any) |> 
  summarise(count=n())
  


victimeshist=grouped |> 
  group_by(Any) |>
  summarise(greus=sum(Numero_lesionats_greus),lleus=sum(Numero_lesionats_lleus),morts=sum(Numero_morts))

greusheat=grouped |> 
  filter(Numero_lesionats_greus>0)

santantoni=grouped |> 
  group_by(Any) |> 
  filter(Nom_barri=="Sant Antoni") |> 
  summarise(numvicsa=sum(Numero_lesionats_greus))
  
#EXPORT DF TO PNGf

library(gridExtra)
df=final[1:10,]
png("final.png",height=480,width=1000)
p=tableGrob(df)
pp=grid.arrange(p)
dev.off()


#DESCRIPTIVE ANALYSIS

ggplot(asaa,aes(Any,count))+
  geom_bar(stat="identity",fill="skyblue2")+
  geom_hline(aes(yintercept=mean(count[1:7]),linetype="Pre-illes 2010-16"),color="blue")+
  geom_hline(aes(yintercept=mean(count[8:13]),linetype="Post-illes 2017-22"),color="red")+
  scale_linetype_manual(name = "Mitjana", values=c(2,2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue"))))+
  labs(y="Número morts",title="Sinistralitat en la ciutat de Barcelona")+
  theme_classic()
ggplot(victimeshist,aes(Any,morts))+
  geom_bar(stat="identity",fill="skyblue2")+
  geom_hline(aes(yintercept=mean(morts[1:7]),linetype="Pre-illes 2010-16"),color="blue")+
  geom_hline(aes(yintercept=mean(morts[8:13]),linetype="Post-illes 2017-22"),color="red")+
  scale_linetype_manual(name = "Mitjana", values=c(2,2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue"))))+
  labs(y="Número morts",title="Sinistralitat en la ciutat de Barcelona")+
  theme_classic()
ggplot(accgrouped,aes((as.character(Any)), count))+
  geom_violin()+
  labs(x="Any",y="Número accidents")
mean(asaa$count[8:13])  
ggplot(victimeshist,aes(Any,greus))+
  geom_bar(stat="identity",fill="skyblue2")+
    geom_hline(aes(yintercept=mean(greus[1:7]),linetype="Pre-illes 2010-16"),color="red")+
    geom_hline(aes(yintercept=mean(greus[8:13]),linetype="Post-illes 2017-22"),color="blue")+
    scale_linetype_manual(name = "Mitjana", values=c(2,2),
                          guide = guide_legend(override.aes = list(color = c("blue", "red"))))+
    labs(y="Número de víctimes greus",title="Sinistralitat en la ciutat de Barcelona")+
  theme_classic()

    
ggplot(final,aes(Any,count))+
  geom_violin()



grouped$poble=ifelse(grouped$Any=="2017",2017,ifelse(grouped$Any=="2018",2018,ifelse(grouped$Any=="2019",2019,ifelse(grouped$Any=="2020",2020,ifelse(grouped$Any=="2021",2021,ifelse(grouped$Any=="2022",2022,2010))))))

mapbcncomplet=get_stamenmap(bbox=c(left=2.0747,bottom=41.3657,right=2.2508,top=41.4390),maptype = "terrain", zoom=13)
ggmap(mapbcncomplet,extent = "device",legend="none")+
  stat_density2d(data=pre,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
  guides(alpha=FALSE) +
  facet_wrap(~Any)

ggmap(mapbcncomplet,extent = "device",legend="none")+
  stat_density2d(data=post,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
  guides(alpha=FALSE)+
  facet_wrap(~Any)+
  labs(title="Evolució accidents Barcelona")


poblenou=get_stamenmap(bbox=c(left=2.17,bottom=41.39,right=2.21,top=41.41),maptype = "terrain", zoom=15)
ggmap(poblenou,extent = "device",legend="none")+
  stat_density2d(data=grouped,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
  facet_wrap(~prepost)+
  guides(alpha=FALSE)+
  labs(title="Concentració accidents zona Poblenou - Diagonal")

santantoni=get_stamenmap(bbox=c(left=2.1505,bottom=41.3741,right=2.1725,top=41.3832),maptype = "terrain", zoom=16)
ggmap(santantoni,extent = "device",legend="none")+
  stat_density2d(data=greusheat,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
  facet_wrap(~prepost)+
  guides(alpha=FALSE)+
  labs(title="Concentració lesionats greus")

poblenouzoom=get_stamenmap(bbox=c(left=2.1855,bottom=41.3979,right=2.2075,top=41.4071),maptype = "terrain", zoom=16)
ggmap(poblenouzoom,extent = "device",legend="none")+
  stat_density2d(data=grouped,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
  facet_wrap(~prepost)+
  guides(alpha=FALSE)+
  labs(title="Concentració lesionats greus")
ggmap(poblenouzoom)+
  geom_hex(data=grouped ,aes(x=Longitud, y=Latitud),bins=15)+
  facet_wrap(~prepost)
hostafrancs=get_stamenmap(bbox=c(left=2.1144,bottom=41.3701,right=2.1584,top=41.3884),maptype = "terrain", zoom=15)
ggmap(hostafrancs)+
  geom_point(data=grouped ,aes(x=Longitud, y=Latitud,color=poble))+
  facet_wrap(~prepost)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  labs(color="Any")

ggmap(hostafrancs,extent = "device",legend="none")+
    stat_density2d(data=grouped,  aes(x=Longitud, y=Latitud, fill=..level.., alpha=..level..), geom="polygon")+
    scale_fill_gradientn(colours=rev(brewer.pal(8, "Spectral")))+
    facet_wrap(~prepost)+
    guides(alpha=FALSE)+
    labs(title="Concentració lesionats greus")

ggmap(santantoni)+
  geom_point(data=grouped ,aes(x=Longitud, y=Latitud,color=poble))+
  facet_wrap(~prepost)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(color="Any")
ggmap(poblenouzoom)+
  geom_point(data=grouped ,aes(x=Longitud, y=Latitud,color=poble))+
  facet_wrap(~prepost)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(color="Any")
unique(grouped$Nom_barri)

horta=get_stamenmap(bbox=c(left=2.1524,bottom=41.4290,right=2.1671,top=41.4394),maptype = "terrain", zoom=15)
ggmap(horta)+
  geom_point(data=grouped ,aes(x=Longitud, y=Latitud,color=poble))+
  facet_wrap(~prepost)+
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank())+
  labs(color="Any")

finalreg=final |> 
  select(-treated) |> 
  mutate(any=ifelse(Any>=2017,1,0)) |> 
  mutate(barris=ifelse(Nom_barri %in% c("el Parc i la Llacuna del Poblenou","Sant Antoni"),1,0)) |> 
  mutate(did=any*barris)

fit=lm(count~did+Poblacio+i.any+barris,data=finalreg)
summary(fit)
