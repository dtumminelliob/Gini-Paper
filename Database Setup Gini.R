####Packages####
require(stringr)
require(lubridate)

####Data sources####
##311##
CRM<-read.csv("C:/Users/dobrien/Documents/Research/Boston-Radcliffe/CRM Data/CRM 8.2014/v. 2015/Main Database 3.2010-4.2015.csv")
CRM<-CRM[!duplicated(CRM$CASE_ENQUIRY_ID),]
CRM_2011<-CRM[year(CRM$OPEN_DT)==2011,]


##911##
CAD1<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/911 Calls/CAD 2015/2010-14 Full CAD, Jan-Jun 2011.csv')
CAD2<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/911 Calls/CAD 2015/2010-14 Full CAD, Jul-Dec 2011.csv')
CAD<-rbind(CAD1,CAD2)
rm(CAD1,CAD2)
CAD$PrivateConflict<-ifelse(CAD$TYPE=='BEIP',1,CAD$PrivateConflict)

##Geographies, Merging####
streets<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Geographical Infrastructure/Geographical Infrastructure v. 2014_ Final Folder/Roads/2015 Updates/Roads_2015_BARI CSV.csv')
tracts<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Geographical Infrastructure/Geographical Infrastructure v. 2014_ Final Folder/Tracts/Tracts_Boston_2010_BARI.csv')
properties<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Geographical Infrastructure/Geographical Infrastructure v. 2017/Properties.2017.csv')
names(properties)
IDConnector<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Geographical Infrastructure/Geographical Infrastructure v. 2017/IDConnector.2017.csv')
names(IDConnector)
parcels<-read.csv('C:/Users/dobrien/Documents/Research/Boston-Radcliffe/Geographical Infrastructure/Geographical Infrastructure v. 2017/LandParcels.2017.csv')
names(parcels)

mean(CRM_2011$parcel_num[!is.na(CRM_2011$X) & !is.na(CRM_2011$parcel_num)] %in% IDConnector$parcel_num)
mean(CAD$SAM_ID[!is.na(CAD$X) & !is.na(CAD$SAM_ID)] %in% IDConnector$Property_ID)

CRM_2011<-merge(CRM_2011[c(1:13,23:31)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CAD<-merge(CAD[c(1:20,31:40)],IDConnector[2:3],by.x="SAM_ID",by.y="Property_ID",all.x=TRUE)
CAD<-merge(CAD,properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CAD$Land_Parcel_ID<-ifelse(CAD$PROP_TYPE=='I',NA,CAD$Land_Parcel_ID)

####Counts####
Add_CRM<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2011,sum)
Add_CAD<-aggregate(cbind(SocDis,PrivateConflict,Violence,Guns)~Land_Parcel_ID,data=CAD,sum)

####Create whole####
##Addresses##
addresses<-merge(parcels[!is.na(parcels$Land_Parcel_ID),c(1:7,26)],Add_CRM,by='Land_Parcel_ID',all.x=TRUE)
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
addresses<-merge(addresses,streets[c(1,23)],by='TLID',all.x=TRUE)
addresses<-merge(addresses,tracts[c(8,10)],by='CT_ID_10',all.x=TRUE)

names(addresses)
addresses$numUnits<-ifelse(!is.na(addresses$numUnits),addresses$numUnits,addresses$num_properties)
addresses<-addresses[c(1:3,5:33)]

####finish streets, tracts####
streets_parcels<-aggregate(numUnits~TLID,data=addresses,FUN=sum)
streets<-merge(streets,streets_parcels,by='TLID')
rm(streets_parcels)

test<-model.matrix(~Zoning,streets)
streets<-cbind(streets,data.frame(test))
names(streets)
streets$ZoningComm<-streets$X.Intercept.-apply(streets[26:29],1,sum)
streets<-streets[c(1:24,30,26:29)]

test<-model.matrix(~Type,tracts)
tracts<-cbind(tracts,data.frame(test))
names(tracts)
tracts$TypeD<-tracts$X.Intercept.-apply(tracts[18:20],1,sum)
tracts<-tracts[c(1:16,21,18:20)]
