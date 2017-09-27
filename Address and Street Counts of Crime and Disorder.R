####Packages####
require(stringr)
require(lubridate)
require(QuantPsyc)

####Data sources####
##311##
CRM<-read.csv("C:/Users/dobrien/Google Drive/BARI Research Team Data Library/CRM 2015/Data/Main Database 2010-2015.csv")
CRM<-CRM[!duplicated(CRM$CASE_ENQUIRY_ID),]
CRM_2011<-CRM[year(CRM$OPEN_DT)==2011,]
CRM_2012<-CRM[year(CRM$OPEN_DT)==2012,]
CRM_2013<-CRM[year(CRM$OPEN_DT)==2013,]
CRM_2014<-CRM[year(CRM$OPEN_DT)==2014,]
CRM_2015<-CRM[year(CRM$OPEN_DT)==2015,]

##911##
CAD1_2011<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jan-Jun 2011.csv')
CAD2_2011<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jul-Dec 2011.csv')
CAD_2011<-rbind(CAD1_2011,CAD2_2011)
rm(CAD1_2011,CAD2_2011)

CAD1_2012<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jan-Jun 2012.csv')
CAD2_2012<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jul-Dec 2012.csv')
CAD_2012<-rbind(CAD1_2012,CAD2_2012)
rm(CAD1_2012,CAD2_2012)

CAD1_2013<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jan-Jun 2013.csv')
CAD2_2013<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/2010-14 Full CAD, Jul-Dec 2013.csv')
CAD_2013<-rbind(CAD1_2013,CAD2_2013)
rm(CAD1_2013,CAD2_2013)


##Geographies, Merging####
properties<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/Properties.2017.csv')
names(properties)
IDConnector<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/IDConnector.2017.csv')
names(IDConnector)
parcels<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/LandParcels.2017.csv')
names(parcels)

mean(CRM_2011$parcel_num[!is.na(CRM_2011$X) & !is.na(CRM_2011$parcel_num)] %in% IDConnector$parcel_num)
mean(CAD$SAM_ID[!is.na(CAD$X) & !is.na(CAD$SAM_ID)] %in% IDConnector$Property_ID)

CRM_2011<-merge(CRM_2011[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CRM_2012<-merge(CRM_2012[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CRM_2013<-merge(CRM_2013[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CRM_2014<-merge(CRM_2014[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CRM_2015<-merge(CRM_2015[c(1:12,22:30)],properties[c(1,16:24)],by='parcel_num',all.x=TRUE)

CAD_2011<-merge(CAD_2011[c(1:20,31:41)],IDConnector[2:3],by.x="SAM_ID",by.y="Property_ID",all.x=TRUE)
CAD_2011<-merge(CAD_2011,properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CAD_2011$Land_Parcel_ID<-ifelse(CAD_2011$PROP_TYPE=='I',NA,CAD_2011$Land_Parcel_ID)
CAD_2012<-merge(CAD_2012[c(1:20,31:41)],IDConnector[2:3],by.x="SAM_ID",by.y="Property_ID",all.x=TRUE)
CAD_2012<-merge(CAD_2012,properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CAD_2012$Land_Parcel_ID<-ifelse(CAD_2012$PROP_TYPE=='I',NA,CAD_2012$Land_Parcel_ID)
CAD_2013<-merge(CAD_2013[c(1:20,31:41)],IDConnector[2:3],by.x="SAM_ID",by.y="Property_ID",all.x=TRUE)
CAD_2013<-merge(CAD_2013,properties[c(1,16:24)],by='parcel_num',all.x=TRUE)
CAD_2013$Land_Parcel_ID<-ifelse(CAD_2013$PROP_TYPE=='I',NA,CAD_2013$Land_Parcel_ID)

####Counts####
Add_CRM_2011<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2011,sum)
names(Add_CRM_2011)[2:3]<-paste(names(Add_CRM_2011)[2:3],'2011',sep='_')
Add_CRM_2012<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2012,sum)
names(Add_CRM_2012)[2:3]<-paste(names(Add_CRM_2012)[2:3],'2012',sep='_')
Add_CRM_2013<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2013,sum)
names(Add_CRM_2013)[2:3]<-paste(names(Add_CRM_2013)[2:3],'2013',sep='_')
Add_CRM_2014<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2014,sum)
names(Add_CRM_2014)[2:3]<-paste(names(Add_CRM_2014)[2:3],'2014',sep='_')
Add_CRM_2015<-aggregate(cbind(PrivateNeglect,PublicDenig)~Land_Parcel_ID,data=CRM_2015,sum)
names(Add_CRM_2015)[2:3]<-paste(names(Add_CRM_2015)[2:3],'2015',sep='_')
Add_CRM<-merge(Add_CRM_2011,Add_CRM_2012,by='Land_Parcel_ID',all=TRUE)
Add_CRM<-merge(Add_CRM,Add_CRM_2013,by='Land_Parcel_ID',all=TRUE)
Add_CRM<-merge(Add_CRM,Add_CRM_2014,by='Land_Parcel_ID',all=TRUE)
Add_CRM<-merge(Add_CRM,Add_CRM_2015,by='Land_Parcel_ID',all=TRUE)

Add_CAD_2011<-aggregate(cbind(SocDis,PrivateConflict,Violence,Guns)~Land_Parcel_ID,data=CAD_2011,sum)
names(Add_CAD_2011)[2:5]<-paste(names(Add_CAD_2011)[2:5],'2011',sep='_')
Add_CAD_2012<-aggregate(cbind(SocDis,PrivateConflict,Violence,Guns)~Land_Parcel_ID,data=CAD_2012,sum)
names(Add_CAD_2012)[2:5]<-paste(names(Add_CAD_2012)[2:5],'2012',sep='_')
Add_CAD_2013<-aggregate(cbind(SocDis,PrivateConflict,Violence,Guns)~Land_Parcel_ID,data=CAD_2013,sum)
names(Add_CAD_2013)[2:5]<-paste(names(Add_CAD_2013)[2:5],'2013',sep='_')
Add_CAD<-merge(Add_CAD_2011,Add_CAD_2012,by='Land_Parcel_ID',all=TRUE)
Add_CAD<-merge(Add_CAD,Add_CAD_2013,by='Land_Parcel_ID',all=TRUE)



####Create whole####
##Addresses##
Add_CRM<-merge(parcels[!is.na(parcels$Land_Parcel_ID),c(1:7,24)],Add_CRM,by='Land_Parcel_ID',all.x=TRUE)
names(Add_CRM)
addresses_temp<-Add_CRM[c(1,9:18)]
addresses_temp[is.na(addresses_temp)]<-0
Add_CRM<-merge(Add_CRM[1:8],addresses_temp,by='Land_Parcel_ID',all.x=TRUE)
names(Add_CRM)[8]<-'TLID'
rm(addresses_temp)

Add_CAD<-merge(parcels[!is.na(parcels$Land_Parcel_ID),c(1:7,24)],Add_CAD,by='Land_Parcel_ID',all.x=TRUE)
names(Add_CAD)
addresses_temp<-Add_CAD[c(1,9:20)]
addresses_temp[is.na(addresses_temp)]<-0
Add_CAD<-merge(Add_CAD[1:8],addresses_temp,by='Land_Parcel_ID',all.x=TRUE)
names(Add_CAD)[8]<-'TLID'
rm(addresses_temp)

write.csv(Add_CRM,'C:/Users/dobrien/Google Drive/BARI Research Team Data Library/CRM 2015/Data/Working Data/Addresses Disorder Reports.csv')
write.csv(Add_CAD,'C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/Addresses Crime Events.csv')

####Streets####
streets<-read.csv('C:/Users/dobrien/Google Drive/BARI Research Team Data Library/GeoInfrastructure2017/roads_updated.csv')
parcels$numUnits<-ifelse(!is.na(parcels$numUnits),parcels$numUnits,parcels$num_properties)
streets_parcels<-aggregate(numUnits~TLID_1,data=parcels,FUN=sum)
streets<-merge(streets,streets_parcels,by.x='TLID',by.y='TLID_1')
rm(streets_parcels)

names(Add_CAD)
names(Add_CRM)
streets_CRM<-aggregate(x = Add_CRM[c(9:18)], by=list(Add_CRM$TLID), FUN=sum)
streets_CAD<-aggregate(x = Add_CAD[c(9:20)], by=list(Add_CRM$TLID), FUN=sum)

write.csv(streets_CRM,'C:/Users/dobrien/Google Drive/BARI Research Team Data Library/CRM 2015/Data/Working Data/Streets Disorder Reports.csv')
write.csv(streets_CAD,'C:/Users/dobrien/Google Drive/BARI Research Team Data Library/911 Database Working Folder/Full Record-Level CAD, 2010-2014/As of September 2016/Streets Crime Events.csv')