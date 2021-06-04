library(readxl)
transfer <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "900")

transfer$ID <- seq.int(nrow(transfer))
transfer$ID <- factor(transfer$ID)
library(tidyr)

transfer<- transfer[,c("ID","교통수단1","교통수단2", "교통수단3", "교통수단4")]
transfer$value<- 1

keycol <- "교통수단"
valuecol <- "교통수단_value"
gathercols <- c("교통수단1", "교통수단2", "교통수단3","교통수단4")

data_long <- gather(transfer, keycol, valuecol, gathercols)
data_long <- data_long[order(data_long$ID),]
data_long<-data_long[!(data_long$valuecol=="값없음"),]

bus_count<- table(data_long$ID)
bus_count<- as.data.frame(bus_count)
names(bus_count)<- c("ID", "환승")

xx <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "900")
xx<- xx[1:10,]
xx$ID <- seq.int(nrow(xx))

act<- merge(x=xx,y=bus_count, all.x = TRUE, by = "ID")
act$환승<- act$환승-1

#......................................................
final_D <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "900")
final_D<- final_D[1:10,]

final_D$ID <- seq.int(nrow(final_D))
final_D$ID <- factor(final_D$ID)
library(tidyr)

final_D<- final_D[,c("ID","하차정류장명1","하차정류장명2", "하차정류장명3", "하차정류장명4")]

keycol <- "하차정류장명"
valuecol <- "하차정류장명_value"
gathercols <- c("하차정류장명1","하차정류장명2", "하차정류장명3", "하차정류장명4")

D_long <- gather(final_D, keycol, valuecol, gathercols)
D_long<-D_long[!(D_long$valuecol=="(null)"),]
xxx <- D_long[order(D_long$ID,D_long$keycol),]
library(dplyr)
D_long<- D_long %>% group_by(ID) %>% slice(n())
D_long<- D_long[,c("ID", "valuecol")]
names(D_long)<- c("ID", "마지막정류장")

new_2<- merge(x=act,y=D_long, all.x = TRUE, by = "ID")

#.........................................................
DD <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "900")

DD$ID <- seq.int(nrow(DD))
DD$ID <- factor(DD$ID)
library(tidyr)

DD<- DD[,c("ID","승차정류장ID1","승차정류장ID2","승차정류장ID3","승차정류장ID4",
           "하차정류장ID1","하차정류장ID2", "하차정류장ID3", "하차정류장ID4")]

keycol1 <- "승차정류장ID"
valuecol1 <- "승차정류장ID_value"
gathercols1 <- c("승차정류장ID1","승차정류장ID2","승차정류장ID3","승차정류장ID4")

keycol2 <- "하차정류장ID"
valuecol2 <- "하차정류장ID_value"
gathercols2 <- c("하차정류장ID1","하차정류장ID2", "하차정류장ID3", "하차정류장ID4")


D_long1 <- gather(DD, keycol1, valuecol1, gathercols1)
D_long1<- D_long1[,-c(2,3,4,5)]
D_long1<-D_long1[!(D_long1$valuecol1=="~"),]
xxx1 <- D_long1[order(D_long1$ID,D_long1$keycol1),]

D_long2 <- gather(DD, keycol2, valuecol2, gathercols2)
D_long2<- D_long2[,-c(2,3,4,5)]
D_long2<-D_long2[!(D_long2$valuecol2=="~"),]
xxx2 <- D_long2[order(D_long2$ID,D_long2$keycol2),]

B1 <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/BUSSTOP(세종).xlsx")
B2 <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/버스정류장정보(대전).xlsx")
names(B1)<- c("ID","stop_name","lat","lon")
names(B2)<- c("ID","stop_name","lat","lon")
library(plyr)
library(dplyr)
B<- rbind.fill(B1, B2)  
names(B)[1]<- "valuecol1"

new_3 <- merge(x=xxx1,y=B, all.x = TRUE, by = "valuecol1")
new_3 <- new_3[order(new_3$ID,new_3$keycol1),]
new_3 <- new_3[complete.cases(new_3), ]
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(sf)
#install.packages("mapview")
library(mapview)
# install.packages("maps")
library(maps)
library(sf)
library(dplyr)
library(stringr)
library(sp)

chi_dat<- as.data.table(new_3)
coordinates(chi_dat)<- c("lon","lat")
crs.geo1<- CRS("+proj=lonlat")
proj4string(chi_dat) = crs.geo1
# plot(chi_dat, pch=20, col="steelblue")

#making map of sejong city using google map
#install.packages("tidyverse")

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')

locations_sf<- st_as_sf(new_3, coords = c("lon","lat"), crs=4326)


library(XML)
library(bitops)
library(RCurl)
latlon2ft <- function(origin,destination){
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  # ft <- distance*3.28084 # FROM METER TO FEET
  return(distance)
}

latlon2ft(origin='37.193489,-121.07395',destination='37.151616,-121.046586')

#install.packages("sos")
require("sos")
findFn("drive_time")

# install.packages("osrm")
library(osrm)
distancias <-osrmTable(loc = new_3[1:3, c("ID","lon","lat")])
distancias$
#install.packages("reshape2")
library(reshape2)
bb <- dcast(new_3, ID ~ keycol1, value.var="lat")





#finding euclidean distance
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
} #returns rough KM distance

dist<- earth.dist(127.3437,36.36265,127.2977,36.48264)


#install.packages("geodist") #finding the euclidean distance using cheap, haversine, geodesic, vincenty
bb<- geodist::geodist(c("127.34368", "36.36265"),c("127.2977367", "36.4826387"), measure = "cheap")





names(B)[1]<- "valuecol2"
new_4<- merge(x=xxx2,y=B, all.x = TRUE, by = "valuecol2")
new_4 <- new_4[order(new_4$ID,new_4$keycol2),]

new_5<- left_join(new_3,new_4,by="ID",all=TRUE)
new_5$seq <- seq.int(nrow(new_5))

















