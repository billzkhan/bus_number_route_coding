#try for 굴곡도
#loading data of long and lat...........
sejong  <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/BUSSTOP(세종).xlsx")
daejeon <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/버스정류장정보(대전).xlsx")
names(sejong)<- c("ID","stop_name","lat","lon")
names(daejeon)<- c("ID","stop_name","lat","lon")
library(plyr)
library(dplyr)
sej_dae<- rbind.fill(sejong, daejeon)  
names(sej_dae)[1]<- "valuecol1"

#loading the actual bus data for 990........
library(readxl)
bus_data <- read_excel("D:/BILAL/bus data_990.1000.1001.1002.1004.1005/노선별 승하차 자료(1002 1001).xlsx", sheet = "990")
#adding unique id of each person
bus_data$ID <- seq.int(nrow(bus_data))
bus_data$ID <- factor(bus_data$ID)
library(tidyr)
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

#merging with lonlat data 
names(sej_dae)[1]<-"ID_1"
names(D_long1)[4]<-"ID_1"
names(D_long2)[4]<-"ID_1"
new_1 <- merge(x=D_long1,y=sej_dae, all.x = TRUE, by= "ID_1")
new_1 <- new_1[order(new_1$ID,new_1$keycol1),]
names(new_1)[6]<- "In_lat"
names(new_1)[7]<- "In_lon"

new_2 <- merge(x=D_long2,y=sej_dae, all.x = TRUE, by = "ID_1")
new_2 <- new_2[order(new_2$ID,new_2$keycol2),]
names(new_2)[1]<- "ID_2"
names(new_2)[6]<- "Out_lat"
names(new_2)[7]<- "Out_lon"

#making total data to make calculations on by combining the enter and out data
new_3<- cbind(new_1, new_2)
new_3<- new_3[,-c(5,9,12)]
#removing the empty trip fields of each person
new_4<- new_3[!(new_3$ID_1=="~" & new_3$ID_2=="~"),]
#removing the persons whos trip ends at ENTER only
new_44<- new_4[(new_4$ID_2=="~"),]
new_5<- new_4[!(new_4$ID %in% new_44$ID),]
#removing the persons whose stops have no long lat value
new_55<- new_5[rowSums(is.na(new_5)) > 0, ] 
new_6<- new_5[!(new_5$ID %in% new_55$ID),]
#removing people making loop route trips
names(new_6)[4] <- "stop_name_1"
names(new_6)[9] <- "stop_name_2"
# new_66<- new_6[(new_6$stop_name_1==new_6$stop_name_2),]
# new_6<- new_6[!(new_6$ID %in% new_66$ID),]


# checking total nas in the final data...make sure no na is there
sapply(new_6, function(x) sum(is.na(x)))
# hi<- new_6[(new_6$ID_1 == new_6$ID_2),] 
#removing trips only with same bus stop ID for ENTER and OUT
new_6<- new_6[!(new_6$ID_1 == new_6$ID_2),] 

hi<- new_6 %>% group_by(ID) %>% slice(c(1,n()))
hi2<- hi %>% group_by(ID) %>% slice(1)
hi2<- hi2[,c(1,2,3,4)]
hi3<- hi %>% group_by(ID) %>% slice(n())
hi3<- hi3[,c(2,7,8,9)]
hi4<- cbind(hi2,hi3)
hi4<- hi4[hi4$stop_name_1==hi4$stop_name_2,]
hi4<- hi4[,-5]
names(hi4)[2]<-"ID" 
new_6<- new_6[!(new_6$ID %in% hi4$ID),]

# hi2<- new_6[!(new_6$ID_1 == new_6$ID_2),] 
# hi3<- subset(new_6, (!ID %in% hi2$ID))

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
new_6$dist<- earth.dist(new_6$In_lon,new_6$In_lat,new_6$Out_lon,new_6$Out_lat)
output<- aggregate(dist~ID, sum, data=new_6)

#finding the distance between O and final destination
output2<- new_6 %>% group_by(ID) %>% slice(c(1,n()))
output21<- output2 %>% group_by(ID) %>% slice(1)
output22<- output2 %>% group_by(ID) %>% slice(n())
OD_dist<- earth.dist(output21$In_lon,output21$In_lat,output22$Out_lon,output22$Out_lat)
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
#bus that starts
write.csv(final, "D:/BILAL/bus data_990.1000.1001.1002.1004.1005/output990.csv")
#install.packages("geodist") #finding the euclidean distance using cheap, haversine, geodesic, vincenty
# bb<- geodist::geodist(new_6[,c(5,6)],new_6[,c(11,12)], measure = "cheap")

# final1<- final[!(final$stop_name_2==final$승차정류장명1),] #to account for the loop route
#some values of OD are more than total distance, ideally it is not possible incase of triangle. to remove that effect
#we apply condition i.e, OD==total distance or, OD> total distance, OD!< total distance
# hi3<- subset(final, (!ID %in% final1$ID))

final3<- final1[!(final1$OD_dist<final1$dist),]

#counting the number of trips made
trip1<- sum(final$환승+1) 
trip2<- sum(final1$환승+1) 
trip3<- sum(final3$환승+1) 


write.csv(final3, "D:/BILAL/bus data_990.1000.1001.1002.1004.1005/output990_star.csv")





origin_stops<- c("정부청사","시청","유성온천","탄방","충남대학교","한밭대학교","월평","노은","용문","갈마","서대전네거리"
                 ,"월드컵경기장")  

final_stops<- c("오송역","도담동","정부세종청사북측","정부세종청사남측","다정동","새롬동.나성동","한솔동","세종고속시외버스터미널(지상)"
                ,"세종고속시외버스터미널(지하)")






