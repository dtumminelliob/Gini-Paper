####Packages####
require(stringr)
require(lubridate)
require(QuantPsyc)

####Data sources####
##311##
CRM<-read.csv("C:/Users/dobrien/Google Drive/BARI Research Team Data Library/CRM 2015/Data/Main Database 2010-2015.csv")
CRM<-CRM[!duplicated(CRM$CASE_ENQUIRY_ID),]
CRM_2011<-CRM[year(CRM$OPEN_DT)==2011,]


##911##
CAD1<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jan-Jun 2011.csv')
CAD2<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jul-Dec 2011.csv')
CAD<-rbind(CAD1,CAD2)
rm(CAD1,CAD2)

##Geographies, Merging####
streets<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/roads_updated.csv')
tracts<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/Tracts_Boston_2010_BARI CSV.csv')
properties<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/Properties.2017.csv')
names(properties)
IDConnector<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/IDConnector.2017.csv')
names(IDConnector)
parcels<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/LandParcels.2017.csv')
names(parcels)

mean(CRM_2011$parcel_num[!is.na(CRM_2011$X) & !is.na(CRM_2011$parcel_num)] %in% IDConnector$parcel_num)
mean(CAD$SAM_ID[!is.na(CAD$X) & !is.na(CAD$SAM_ID)] %in% IDConnector$Property_ID)

CRM_2011<-merge(CRM_2011[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)

####Counts####
Add_CRM<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2011,sum)
Add_CAD<-aggregate(cbind(SocDis,PrivateConflict,Violence,Guns)~Land_Parcel_ID,data=CAD,sum)

####Create whole####
##Addresses##
addresses<-merge(parcels[!is.na(parcels$Land_Parcel_ID),c(1:7,24)],Add_CRM,by='Land_Parcel_ID',all.x=TRUE)
addresses<-merge(addresses,Add_CAD,by='Land_Parcel_ID',all.x=TRUE)
rm(Add_CRM,Add_CAD)

addresses_temp<-addresses[c(1,9:14)]
addresses_temp[is.na(addresses_temp)]<-0

addresses<-merge(addresses[1:8],addresses_temp,by='Land_Parcel_ID',all.x=TRUE)
names(addresses)[8]<-'TLID'
rm(addresses_temp)

test<-model.matrix(~LU,addresses)
addresses<-cbind(addresses,data.frame(test))
addresses$LUA<-addresses$X.Intercept.-apply(addresses[16:31],1,sum)
rm(test)

names(addresses)
addresses<-addresses[c(1:14,32,16:31)]


####Merge w roads, tracts####
addresses<-merge(addresses,streets[c(3,24)],by='TLID',all.x=TRUE)
addresses<-merge(addresses,tracts[c(8,10)],by='CT_ID_10',all.x=TRUE)

names(addresses)
addresses$numUnits<-ifelse(!is.na(addresses$numUnits),addresses$numUnits,addresses$num_properties)
addresses<-addresses[c(1:3,5:33)]

####finish streets, tracts####
streets_parcels<-aggregate(numUnits~TLID,data=addresses,FUN=sum)
streets<-merge(streets,streets_parcels,by='TLID')
rm(streets_parcels)

test<-model.matrix(~as.factor(New_Cluster),streets)
streets<-cbind(streets,data.frame(test))
names(streets)
streets$New_Cluster.1<-streets$X.Intercept.-apply(streets[32:37],1,sum)
streets<-streets[c(1:30,38,32:37)]
names(streets)[32:37]<-paste("New_Cluster",2:7,sep='.')

test<-model.matrix(~Type,tracts)
tracts<-cbind(tracts,data.frame(test))
names(tracts)
tracts$TypeD<-tracts$X.Intercept.-apply(tracts[18:20],1,sum)
tracts<-tracts[c(1:16,21,18:20)]