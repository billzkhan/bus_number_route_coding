library(plyr)
library(dplyr)
library(readxl)
library(tidyr)

#now taking steps to add long lat values
sejong  <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/BUSSTOP(세종).xlsx")
daejeon <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/버스정류장정보(대전).xlsx")
names(sejong)<- c("ID","stop_name","lat","lon")
names(daejeon)<- c("ID","stop_name","lat","lon")
sejong<- sejong %>% distinct(stop_name, .keep_all = TRUE)
daejeon<- daejeon %>% distinct(stop_name, .keep_all = TRUE)
daejeon<- daejeon[!(daejeon$stop_name %in% sejong$stop_name),]
sej_dae2<- rbind.fill(sejong, daejeon)  
names(sej_dae2)[1]<- "valuecol1"

#loading data of long and lat...........
sej_dae  <- read.csv("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/latlon_impstop.csv")
names(sej_dae)<- c("stop_name","lat","lon")
sej_dae$valuecol1<- "fake" 
sej_dae<- sej_dae[,c(4,1,2,3)]

#combining the two latlon files
sej_dae<- rbind(sej_dae2,sej_dae)
sej_dae<- sej_dae %>% distinct(stop_name, .keep_all = TRUE)


#loading the actual bus data for 1000.............
bus_data <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "1002")
#adding unique id of each person
bus_data$ID <- seq.int(nrow(bus_data))
bus_data$ID <- factor(bus_data$ID)
#taking out the data to convert it into long data from wide data format
bus_data_n<- bus_data[,c("ID","승차정류장명1","승차정류장명2","승차정류장명3","승차정류장명4",
                         "하차정류장명1","하차정류장명2", "하차정류장명3", "하차정류장명4")]
bus_data_n1<- bus_data[,c("ID","승차정류장ID1","승차정류장ID2","승차정류장ID3","승차정류장ID4")]
bus_data_n2<- bus_data[,c("ID","하차정류장ID1","하차정류장ID2", "하차정류장ID3", "하차정류장ID4")]

keycol1 <- "승차정류장명"
valuecol1 <- "승차정류장명_value"
gathercols1 <- c("승차정류장명1","승차정류장명2","승차정류장명3","승차정류장명4")

keycol2 <- "하차정류장명"
valuecol2 <- "하차정류장명_value"
gathercols2 <- c("하차정류장명1","하차정류장명2", "하차정류장명3", "하차정류장명4")

keycol3 <- "승차정류장ID"
valuecol3 <- "승차정류장ID_value"
gathercols3 <- c("승차정류장ID1","승차정류장ID2","승차정류장ID3","승차정류장ID4")
D_long3 <- gather(bus_data_n1, keycol3, valuecol3, gathercols3)
D_long3 <- D_long3[order(D_long3$ID,D_long3$keycol3),]

keycol4 <- "승차정류장ID"
valuecol4 <- "승차정류장ID_value"
gathercols4 <- c("하차정류장ID1","하차정류장ID2", "하차정류장ID3", "하차정류장ID4")
D_long4 <- gather(bus_data_n2, keycol4, valuecol4, gathercols4)
D_long4 <- D_long4[order(D_long4$ID,D_long4$keycol4),]

#gathering the enter and out data separately to combine later
D_long1 <- gather(bus_data_n, keycol1, valuecol1, gathercols1)
D_long1<- D_long1[,-c(2,3,4,5)]
D_long1 <- D_long1[order(D_long1$ID,D_long1$keycol1),]

D_long2 <- gather(bus_data_n, keycol2, valuecol2, gathercols2)
D_long2<- D_long2[,-c(2,3,4,5)]
D_long2 <- D_long2[order(D_long2$ID,D_long2$keycol2),]

#combining the ids and name 
D_long1 <- cbind(D_long1,D_long3)
D_long1 <- D_long1[,-c(4,5)]

D_long2 <- cbind(D_long2,D_long4)
D_long2 <- D_long2[,-c(4,5)]

names(D_long1)[3]<-"stop_name" 
names(D_long2)[3]<-"stop_name"

D_long1<- merge(D_long1, sej_dae, all.x = TRUE, by= "stop_name")
D_long1<- D_long1[,-5]
D_long1<- D_long1[order(D_long1$ID, D_long1$keycol1),]


D_long2<- merge(D_long2, sej_dae, all.x = TRUE, by= "stop_name")
D_long2<- D_long2[,-5]
D_long2<- D_long2[order(D_long2$ID, D_long2$keycol2),]

#making total data to make calculations on by combining the enter and out data
new_3<- cbind(D_long1, D_long2)
new_3<- new_3[,-8]
#removing the empty trip fields of each person
new_4<- new_3[!(new_3$valuecol3=="~" & new_3$valuecol4=="~"),]
#removing the persons whos trip ends at ENTER only
new_44<- new_4[(new_4$valuecol4=="~"),]
new_5<- new_4[!(new_4$ID %in% new_44$ID),]

#checking for stopnames whose long lat is not available.........needs to be added in self made improved longlat
sapply(new_5, function(x) sum(is.na(x)))
c1<- unique(new_5$stop_name[which(is.na(new_5$lat))])
c2<- unique(new_5$stop_name.1[which(is.na(new_5$lat.1))])
c1002<- as.data.frame(union(c1,c2))


#calculation for extracting the desired O and D only
hi<-  new_5 %>% group_by(ID) %>% slice(c(1,n()))
hi2<- hi %>% group_by(ID) %>% slice(1)
hi2<- hi2[,c(1,2,3,4,5,6)]
hi2<- hi2[order(hi2$ID, hi2$keycol1),]
hi3<- hi %>% group_by(ID) %>% slice(n())
hi3<- hi3[,c(2,7,8,9,10,11)]
hi3<- hi3[order(hi3$ID, hi3$keycol2),]

origin_stops<- c("시청","유성온천","정부청사","노은", "탄방", "한밭대학교", "반석역3번출구","용문",
                 "보람동(호려울마을)", "국책연구단지북측")
final_stops<- c("소담동(새샘마을)","세종시청.교육청.시의회","국책연구단지북측","보람동(해들마을)"
                ,"세종우체국(보람초등학교)", "반석역3번출구","시청","유성온천","정부청사","노은",
                "탄방", "한밭대학교")

hi4<- cbind(hi2,hi3)
hi4<- hi4 %>% filter((stop_name %in% origin_stops))
hi4<- hi4 %>% filter((stop_name.1 %in% final_stops))
hi4<- hi4[,-7]
names(hi4)[2]<- "ID"

new_6<- new_5[(new_5$ID %in% hi4$ID),]
new_6<- new_6[order(new_6$ID, new_6$keycol1),]
#removing trips only with same bus stop ID for ENTER and OUT
new_6<- new_6[!(new_6$valuecol3 == new_6$valuecol4),]
sapply(new_6, function(x) sum(is.na(x)))
empty<- new_6[which(is.na(new_6$lat)),]

#.........finding total euclidean distance on total data cleaned.....................................
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
# new_6<- new_6[,-8]
#finding the total distance
new_6$dist<- earth.dist(new_6$lon,new_6$lat,new_6$lon.1,new_6$lat.1)
output<- aggregate(dist~ID, sum, data=new_6)

#finding the distance between O and final destination
output2<- new_6 %>% group_by(ID) %>% slice(c(1,n()))
output21<- output2 %>% group_by(ID) %>% slice(1)
output22<- output2 %>% group_by(ID) %>% slice(n())
OD_dist<- earth.dist(output21$lon,output21$lat,output22$lon.1,output22$lat.1)
OD_dist<- cbind(output$ID,OD_dist)
OD_dist<- as.data.frame(OD_dist)
names(OD_dist)[1]<- "ID"
output2<- OD_dist
#combining the total distance and OD distance to find the curvature
output3<- cbind(output,output2)
output3<- output3[,-3]
output3$굴곡도<- output3$dist/output3$OD_dist
#finding the last stop of each person
x<- new_6 %>% group_by(ID) %>% slice(n())
x<- x[,c(2,7,9)]
output3<- cbind(output3,x)
output3<- output3[,-5]
#finding total transfers of each person
bus_count<- table(new_6$ID)
bus_count<- as.data.frame(bus_count)
names(bus_count)<- c("ID", "환승")
bus_count$환승<- bus_count$환승-1
bus_count<- bus_count[bus_count$환승>=0,]
output3<- cbind(output3,bus_count)
output3<- output3[,-7]
final<- merge(bus_data,output3, all.y=TRUE, by= "ID")
final <- final[order(final$ID),]

#bus that starts
write.csv(final, "D:/BILAL/bus data_990.1000.1001.1002.1004.1005/output1002_imp.csv")
#install.packages("geodist") #finding the euclidean distance using cheap, haversine, geodesic, vincenty
# bb<- geodist::geodist(new_6[,c(5,6)],new_6[,c(11,12)], measure = "cheap")

# final1<- final[!(final$stop_name_2==final$승차정류장명1),] #to account for the loop route
#some values of OD are more than total distance, ideally it is not possible incase of triangle. to remove that effect
#we apply condition i.e, OD==total distance or, OD> total distance, OD!< total distance
# hi3<- subset(final, (!ID %in% final1$ID))


# #counting the number of trips made
# trip1<- sum(final$환승+1) 
# trip2<- sum(final1$환승+1) 
# trip3<- sum(final3$환승+1) 
# 
# 
# write.csv(final3, "D:/BILAL/bus data_990.1000.1001.1002.1004.1005/output990_star.csv")







