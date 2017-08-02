library(tidyverse)
library(readxl)
library(rgdal)
library(sf)




#ESRI GCS_North_American_1983
NAD83<-CRS("+proj=longlat +datum=NAD83") 

one<-st_read('L:/Public/Milstead_Lakes/GIS/NHDPlus/NHDPlus01/Hydrography/NHDWaterbody.shp')
two<-st_read('L:/Public/Milstead_Lakes/GIS/NHDPlus/NHDPlus02/Hydrography/NHDWaterbody.shp') 
three<-read_sf('L:/Public/Milstead_Lakes/GIS/NHDPlus/NHDPlus03/Hydrography/NHDWaterbody.shp') 
six<-read_sf('L:/Public/Milstead_Lakes/GIS/NHDPlus/NHDPlus06/Hydrography/NHDWaterbody.shp') 

ec_sf<-rbind(one,two,three,six)%>%mutate(id=REACHCODE)%>%select(-REACHCODE)

ts1<-mutate(ts,id=as.character(id))

ec_sf<-left_join(ec_sf,ts1)

table(ec02$WBRchCd%in%ec_sf$id) #F=43 T=41523


#state map
state<-map_data('state')


ggplot(state,aes(x=long,y=lat))+
  geom_polygon(aes(group=group),fill=NA,colour="black")+
  geom_sf(data = ec_sf, colour = "blue", fill = NA)


ggplot(ec_sf) +
  geom_sf(aes(fill = ts02))



+
  geom_point(data=lakes_dd,aes(x=long,y=lat,color=factor(Cat)),size=3.0)+  #cat variable & size
  scale_color_manual(values=Colors,                                                  #colors
                     name="",
                     breaks=Breaks,                            #cat breaks
                     labels=Labels)+                                                      #cat labels
  coord_map("albers", lat2 = 45.5, lat1 = 29.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.spacing=unit(c(0,0,0,0),"in"),
        plot.margin=unit(c(0,0,0,0),"in"),
        plot.background = element_blank(),
        legend.position = c(0.5,0),
        legend.direction="horizontal", 
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank())+
  ggtitle(Title)+                                                                      #title
  theme(plot.title = element_text(lineheight=4.8, face="bold",hjust = 0.5))  






#function to map cats
mapIt<-function(Cat,Colors,Breaks,Labels,Title){
  gg<-ggplot(state,aes(x=long,y=lat))+
    geom_polygon(aes(group=group),fill=NA,colour="black")+
    geom_point(data=lakes_dd,aes(x=long,y=lat,color=factor(Cat)),size=3.0)+  #cat variable & size
    scale_color_manual(values=Colors,                                                  #colors
                       name="",
                       breaks=Breaks,                            #cat breaks
                       labels=Labels)+                                                      #cat labels
    coord_map("albers", lat2 = 45.5, lat1 = 29.5) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.spacing=unit(c(0,0,0,0),"in"),
          plot.margin=unit(c(0,0,0,0),"in"),
          plot.background = element_blank(),
          legend.position = c(0.5,0),
          legend.direction="horizontal", 
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title=element_blank())+
    ggtitle(Title)+                                                                      #title
    theme(plot.title = element_text(lineheight=4.8, face="bold",hjust = 0.5))  
  return(gg)
}

library(sf)
library(tidyverse)
library(viridis)
library(rvest)

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  scale_fill_viridis("Area") +
  ggtitle("Area of counties in North Carolina") +
  theme_bw()

 nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggplot(nc) +
    geom_sf(aes(fill = AREA))
  
  # If not supplied, coord_sf() will take the CRS from the first layer
  # and automatically transform all other layers to use that CRS. This
  # ensures that all data will correctly line up
  nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
  ggplot() +
    geom_sf(data = nc) +
    geom_sf(data = nc_3857, colour = "red", fill = NA)
  
  # You can also use layers with x and y aesthetics: these are
  # assumed to already be in the common CRS.
  ggplot(nc) +
    geom_sf() +
    annotate("point", x = -80, y = 35, colour = "red", size = 4)
  
  # Thanks to the power of sf, ageom_sf nicely handles varying projections
  # setting the aspect ratio correctly.
  library(maps)
  world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
  ggplot() + geom_sf(data = world1)
  
  world2 <- sf::st_transform(
    world1,
    "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
  )
  ggplot() + geom_sf(data = world2)
}


STATE_ABBR