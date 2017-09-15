##packages####
require(reldist)
require(plyr)
require(sqldf)
require(ggplot2)
require(QuantPsyc)
options(scipen=999)

##functions####
gini_prime<-function(x) {
  max(length(x)/sum(x),1)*(gini(x)-1)+1
}

jcknf_se<-function(x) {
  sqrt(var(x)*(length(x)-1)/length(x))/sqrt(length(x))
}

pois_test<-function(x,y){
  pois_test_v<-gini_prime(rpois(y,x))
  for (i in 2:10000) {
    pois_test_v[i]<-gini_prime(rpois(y,x))
  }
  mean(pois_test_v,na.rm = TRUE)
}

pois_test_thrshld<-function(x,y,z){
  pois_test_v<-gini_prime(rpois(y,x))
  for (i in 2:10000) {
    pois_test_v[i]<-gini_prime(rpois(y,x))
  }
  mean(pois_test_v>z,na.rm = TRUE)
}


or_comp<-function(x,y){
  (x/(1-x))/(y/(1-y))
}


##initial descriptives####
apply(addresses[9:14],2,sum)
apply(addresses[9:14],2,mean)
apply(addresses[9:14],2,sd)
apply(addresses[9:14],2,min)
apply(addresses[9:14],2,max)
apply(addresses[9:14],2,gini)
apply(addresses[9:14],2,gini_prime)

#addresses_jcknf<-data.frame(addresses[[1]])
#for (j in 9:14) {
#print(j)
#  for (i in 1:nrow(addresses)) {
#    print(i)
#  addresses_jcknf[i,(j-5)]<-gini_prime(addresses[-i,j])
#  }
#}
#apply(addresses_jcknf[2:7],2,jcknf_se)


addresses_means<-apply(addresses[9:14],2,mean)
for (i in 1:length(addresses_means)) {
  print(pois_test(addresses_means[i],nrow(addresses)))
}

for (i in 1:length(addresses_means)) {
  print(pois_test_thrshld(addresses_means[i],nrow(addresses),gini_prime(addresses[[i+8]])))
}



##streets##
streets_gini<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns)~TLID,FUN = sum,data=addresses)
summary(streets_gini)
names(streets_gini)


apply(streets_gini[2:7],2,mean)
apply(streets_gini[2:7],2,sd)
apply(streets_gini[2:7],2,min)
apply(streets_gini[2:7],2,max)
apply(streets_gini[2:7],2,gini)
apply(streets_gini[2:7],2,gini_prime)

streets_means<-apply(streets_gini[2:7],2,mean)
for (i in 1:length(streets_means)) {
  print(pois_test(streets_means[i],nrow(streets)))
}

for (i in 1:length(streets_means)) {
  print(pois_test_thrshld(streets_means[i],nrow(streets),gini_prime(streets_gini[[i+1]])))
}


##tracts##
tracts_nc<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns)~CT_ID_10,FUN = sum,data=addresses)

apply(tracts_nc[2:7],2,mean)
apply(tracts_nc[2:7],2,sd)
apply(tracts_nc[2:7],2,min)
apply(tracts_nc[2:7],2,max)
apply(tracts_nc[2:7],2,gini)
apply(tracts_nc[2:7],2,gini_prime)

tracts_means<-apply(tracts_nc[2:7],2,mean)
for (i in 1:length(tracts_means)) {
  print(pois_test(tracts_means[i],nrow(tracts)))
}

for (i in 1:length(tracts_means)) {
  print(pois_test_thrshld(tracts_means[i],nrow(tracts),gini_prime(tracts_nc[[i+1]])))
}


##planning districts##
pds_nc<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns)~BRA_PD,FUN = sum,data=addresses)

apply(pds_nc[2:7],2,mean)
apply(pds_nc[2:7],2,sd)
apply(pds_nc[2:7],2,min)
apply(pds_nc[2:7],2,max)
apply(pds_nc[2:7],2,gini)
apply(pds_nc[2:7],2,gini_prime)

pds_means<-apply(pds_nc[2:7],2,mean)
for (i in 1:length(pds_means)) {
  print(pois_test(pds_means[i],nrow(pds_nc)))
}

##address adjustments####
addresses$PrivateNeglect_adj<-addresses$PrivateNeglect - 2.71828^(-4.5826 + .0175*addresses$numUnits  + 2.8167*addresses$LUA + .1313*addresses$LUC - 0.3402*addresses$LUCC + 1.5803*addresses$LUCD - .7858*addresses$LUCL + .9893*addresses$LUE + 2.2447*addresses$LUEA + .4051*addresses$LUI + 1.161*addresses$LUR2 + 1.7925*addresses$LUR3 + 2.4143*addresses$LUR4 + 2.1203*addresses$LURC - .2362*addresses$LURL)
addresses$PrivateNeglect_adj<-ifelse(addresses$PrivateNeglect_adj>0, addresses$PrivateNeglect_adj,0)
addresses$PrivateNeglect_adj<-ifelse(addresses$PrivateNeglect_adj>0 & addresses$PrivateNeglect_adj<1, 1, addresses$PrivateNeglect_adj)
summary(addresses$PrivateNeglect_adj)

addresses$PublicDenig_adj<-addresses$PublicDenig - 2.71828^(-4.5898 + .0089*addresses$numUnits + 1.314*addresses$LUA + 1.310*addresses$LUC + 1.8087*addresses$LUCC + .8722*addresses$LUCD - .1526*addresses$LUCL + .6928*addresses$LUE + .5769*addresses$LUEA + 1.4126*addresses$LUI + .3316*addresses$LUR2 + .7315*addresses$LUR3 + 1.0346*addresses$LUR4 + 1.3289*addresses$LURC - 1.0781*addresses$LURL)
addresses$PublicDenig_adj<-ifelse(addresses$PublicDenig_adj>0, addresses$PublicDenig_adj,0)
addresses$PublicDenig_adj<-ifelse(addresses$PublicDenig_adj>0 & addresses$PublicDenig_adj<1, 1, addresses$PublicDenig_adj)
summary(addresses$PublicDenig_adj)

addresses$SocDis_adj<-addresses$SocDis - 2.71828^(-6.5079 + .0084*addresses$numUnits + 1.664*addresses$LUA + 1.8301*addresses$LUC + 2.182*addresses$LUCC + 1.2192*addresses$LUCD - .9555*addresses$LUCL + 1.2815*addresses$LUE + 1.79*addresses$LUEA + .0993*addresses$LUI + .4709*addresses$LUR2 + .6951*addresses$LUR3 + 1.126*addresses$LUR4 + 1.4065*addresses$LURC - 3.0381*addresses$LURL)
addresses$SocDis_adj<-ifelse(addresses$SocDis_adj>0, addresses$SocDis_adj,0)
addresses$SocDis_adj<-ifelse(addresses$SocDis_adj>0 & addresses$SocDis_adj<1, 1, addresses$SocDis_adj)
summary(addresses$SocDis_adj)

addresses$PrivateConflict_adj<-addresses$PrivateConflict - 2.71828^(-3.551 + .0117*addresses$numUnits + 2.222*addresses$LUA + .7653*addresses$LUC + 1.13*addresses$LUCC + 1.2878*addresses$LUCD - 1.1243*addresses$LUCL + .6944*addresses$LUE + 1.9415*addresses$LUEA - .145*addresses$LUI + .7316*addresses$LUR2 + 1.2407*addresses$LUR3 + 1.767*addresses$LUR4 + 1.7185*addresses$LURC - 2.1306*addresses$LURL)
addresses$PrivateConflict_adj<-ifelse(addresses$PrivateConflict_adj>0, addresses$PrivateConflict_adj,0)
addresses$PrivateConflict_adj<-ifelse(addresses$PrivateConflict_adj>0 & addresses$PrivateConflict_adj<1, 1, addresses$PrivateConflict_adj)
summary(addresses$PrivateConflict_adj)

addresses$Violence_adj<-addresses$Violence - 2.71828^(-3.832 + .0148*addresses$numUnits + 2.1826*addresses$LUA + 1.7978*addresses$LUC + 1.5667*addresses$LUCC + .8956*addresses$LUCD - .3382*addresses$LUCL + 1.7448*addresses$LUE + 1.8088*addresses$LUEA + .4922*addresses$LUI + .3847*addresses$LUR2 + .9444*addresses$LUR3 + 1.3608*addresses$LUR4 + 1.7821*addresses$LURC - 2.0514*addresses$LURL)
addresses$Violence_adj<-ifelse(addresses$Violence_adj>0, addresses$Violence_adj,0)
addresses$Violence_adj<-ifelse(addresses$Violence_adj>0 & addresses$Violence_adj<1, 1, addresses$Violence_adj)
summary(addresses$Violence_adj)

addresses$Guns_adj<-addresses$Guns - 2.71828^(-7.1985 + .0157*addresses$numUnits + 1.2338*addresses$LUA + 1.006*addresses$LUC + 1.1379*addresses$LUCC + .8029*addresses$LUCD - 1.9674*addresses$LUCL + .5622*addresses$LUE + 1.5316*addresses$LUEA + .7206*addresses$LUI + .3475*addresses$LUR2 + .5982*addresses$LUR3 + .9278*addresses$LUR4 + 1.2479*addresses$LURC - 1.5965*addresses$LURL)
addresses$Guns_adj<-ifelse(addresses$Guns_adj>0, addresses$Guns_adj,0)
addresses$Guns_adj<-ifelse(addresses$Guns_adj>0 & addresses$Guns_adj<1, 1, addresses$Guns_adj)
summary(addresses$Guns_adj)

##street adjustments##
streets$PrivateNeglect_exp_dev<-2.71828^((streets$Length-95.33)*(.0014)-.0169*streets$numUnits+.3083*streets$Main+.3081*streets$Cluster1+.1671*streets$Cluster2+.2233*streets$Cluster3+.2288*streets$Cluster5+.086*streets$Cluster6+.4981*streets$Cluster7)
streets$PublicDenig_exp_dev<-2.71828^((streets$Length-95.33)*(.0002)-.0132*streets$numUnits+.2668*streets$Main+.3512*streets$Cluster1+.1402*streets$Cluster2+.4464*streets$Cluster3+.0407*streets$Cluster5+.5341*streets$Cluster6+.6258*streets$Cluster7)
streets$SocDis_exp_dev<-2.71828^((streets$Length-95.33)*(.0015)-.0254*streets$numUnits+.7398*streets$Main+.6007*streets$Cluster1+.2372*streets$Cluster2+1.246*streets$Cluster3+.5462*streets$Cluster5+.2437*streets$Cluster6+1.2491*streets$Cluster7)
streets$PrivateConflict_exp_dev<-2.71828^((streets$Length-95.33)*(.0023)-.0275*streets$numUnits+.2505*streets$Main+.2439*streets$Cluster1+.1055*streets$Cluster2+.653*streets$Cluster3+.4059*streets$Cluster5-.1145*streets$Cluster6+.3063*streets$Cluster7)
streets$Violence_exp_dev<-2.71828^((streets$Length-95.33)*(.0029)-.0324*streets$numUnits+.5017*streets$Main+.3967*streets$Cluster1+.5117*streets$Cluster2+.6414*streets$Cluster3+.1179*streets$Cluster5+.1427*streets$Cluster6+.6646*streets$Cluster7)
streets$Guns_exp_dev<-2.71828^((streets$Length-95.33)*(.0031)-.0353*streets$numUnits+.5434*streets$Main+.9608*streets$Cluster1+.2899*streets$Cluster2+1.1227*streets$Cluster3+1.1195*streets$Cluster5-.1246*streets$Cluster6+.9543*streets$Cluster7)
addresses<-merge(addresses,streets[c(1,30:35)],by='TLID',all.x=TRUE)
addresses$PrivateNeglect_adj_street<-addresses$PrivateNeglect-((addresses$PrivateNeglect-addresses$PrivateNeglect_adj)*addresses$PrivateNeglect_exp_dev)
addresses$PrivateNeglect_adj_street<-ifelse(addresses$PrivateNeglect_adj_street>0, addresses$PrivateNeglect_adj_street,0)
addresses$PrivateNeglect_adj_street<-ifelse(addresses$PrivateNeglect_adj_street>0 & addresses$PrivateNeglect_adj_street<1, 1, addresses$PrivateNeglect_adj_street)

addresses$PublicDenig_adj_street<-addresses$PublicDenig-((addresses$PublicDenig-addresses$PublicDenig_adj)*addresses$PublicDenig_exp_dev)
addresses$PublicDenig_adj_street<-ifelse(addresses$PublicDenig_adj_street>0, addresses$PublicDenig_adj_street,0)
addresses$PublicDenig_adj_street<-ifelse(addresses$PublicDenig_adj_street>0 & addresses$PublicDenig_adj_street<1, 1, addresses$PublicDenig_adj_street)

addresses$SocDis_adj_street<-addresses$SocDis-((addresses$SocDis-addresses$SocDis_adj)*addresses$SocDis_exp_dev)
addresses$SocDis_adj_street<-ifelse(addresses$SocDis_adj_street>0, addresses$SocDis_adj_street,0)
addresses$SocDis_adj_street<-ifelse(addresses$SocDis_adj_street>0 & addresses$SocDis_adj_street<1, 1, addresses$SocDis_adj_street)

addresses$PrivateConflict_adj_street<-addresses$PrivateConflict-((addresses$PrivateConflict-addresses$PrivateConflict_adj)*addresses$PrivateConflict_exp_dev)
addresses$PrivateConflict_adj_street<-ifelse(addresses$PrivateConflict_adj_street>0, addresses$PrivateConflict_adj_street,0)
addresses$PrivateConflict_adj_street<-ifelse(addresses$PrivateConflict_adj_street>0 & addresses$PrivateConflict_adj_street<1, 1, addresses$PrivateConflict_adj_street)

addresses$Violence_adj_street<-addresses$Violence-((addresses$Violence-addresses$Violence_adj)*addresses$Violence_exp_dev)
addresses$Violence_adj_street<-ifelse(addresses$Violence_adj_street>0, addresses$Violence_adj_street,0)
addresses$Violence_adj_street<-ifelse(addresses$Violence_adj_street>0 & addresses$Violence_adj_street<1, 1, addresses$Violence_adj_street)

addresses$Guns_adj_street<-addresses$Guns-((addresses$Guns-addresses$Guns_adj)*addresses$Guns_exp_dev)
addresses$Guns_adj_street<-ifelse(addresses$Guns_adj_street>0, addresses$Guns_adj_street,0)
addresses$Guns_adj_street<-ifelse(addresses$Guns_adj_street>0 & addresses$Guns_adj_street<1, 1, addresses$Guns_adj_street)

##tract adjustments##
tracts$PrivateNeglect_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(.000209)+(tracts$HU100-1531)*(-.00029)-.0952*tracts$TypeI+.0448*tracts$TypeP-.8061*tracts$TypeD)
tracts$PublicDenig_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(-.000205)+(tracts$HU100-1531)*(.000593)+.456*tracts$TypeI-.3264*tracts$TypeP+.4805*tracts$TypeD)
tracts$SocDis_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(.000021)+(tracts$HU100-1531)*(.00009)+.2236*tracts$TypeI-.2486*tracts$TypeP+.2619*tracts$TypeD)
tracts$PrivateConflict_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(.00019)+(tracts$HU100-1531)*(-.000304)-.117*tracts$TypeI+.15*tracts$TypeP-.1819*tracts$TypeD)
tracts$Violence_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(.00012)+(tracts$HU100-1531)*(-.000138)-.0848*tracts$TypeI-.134*tracts$TypeP-.0373*tracts$TypeD)
tracts$Guns_exp_dev_tract<-2.71828^((tracts$POP100-3466)*(.000362)+(tracts$HU100-1531)*(-.000775)-.6608*tracts$TypeI-.2789*tracts$TypeP-.6502*tracts$TypeD)
names(tracts)

addresses<-merge(addresses,tracts[c(8,21:26)],by='CT_ID_10',all.x=TRUE)
addresses$PrivateNeglect_adj_tract<-addresses$PrivateNeglect-((addresses$PrivateNeglect-addresses$PrivateNeglect_adj_street)*addresses$PrivateNeglect_exp_dev_tract)
addresses$PrivateNeglect_adj_tract<-ifelse(addresses$PrivateNeglect_adj_tract>0, addresses$PrivateNeglect_adj_tract,0)
addresses$PrivateNeglect_adj_tract<-ifelse(addresses$PrivateNeglect_adj_tract>0 & addresses$PrivateNeglect_adj_tract<1, 1, addresses$PrivateNeglect_adj_tract)

addresses$PublicDenig_adj_tract<-addresses$PublicDenig-((addresses$PublicDenig-addresses$PublicDenig_adj_street)*addresses$PublicDenig_exp_dev_tract)
addresses$PublicDenig_adj_tract<-ifelse(addresses$PublicDenig_adj_tract>0, addresses$PublicDenig_adj_tract,0)
addresses$PublicDenig_adj_tract<-ifelse(addresses$PublicDenig_adj_tract>0 & addresses$PublicDenig_adj_tract<1, 1, addresses$PublicDenig_adj_tract)

addresses$SocDis_adj_tract<-addresses$SocDis-((addresses$SocDis-addresses$SocDis_adj_street)*addresses$SocDis_exp_dev_tract)
addresses$SocDis_adj_tract<-ifelse(addresses$SocDis_adj_tract>0, addresses$SocDis_adj_tract,0)
addresses$SocDis_adj_tract<-ifelse(addresses$SocDis_adj_tract>0 & addresses$SocDis_adj_tract<1, 1, addresses$SocDis_adj_tract)

addresses$PrivateConflict_adj_tract<-addresses$PrivateConflict-((addresses$PrivateConflict-addresses$PrivateConflict_adj_street)*addresses$PrivateConflict_exp_dev_tract)
addresses$PrivateConflict_adj_tract<-ifelse(addresses$PrivateConflict_adj_tract>0, addresses$PrivateConflict_adj_tract,0)
addresses$PrivateConflict_adj_tract<-ifelse(addresses$PrivateConflict_adj_tract>0 & addresses$PrivateConflict_adj_tract<1, 1, addresses$PrivateConflict_adj_tract)

addresses$Violence_adj_tract<-addresses$Violence-((addresses$Violence-addresses$Violence_adj_street)*addresses$Violence_exp_dev_tract)
addresses$Violence_adj_tract<-ifelse(addresses$Violence_adj_tract>0, addresses$Violence_adj_tract,0)
addresses$Violence_adj_tract<-ifelse(addresses$Violence_adj_tract>0 & addresses$Violence_adj_tract<1, 1, addresses$Violence_adj_tract)

addresses$Guns_adj_tract<-addresses$Guns-((addresses$Guns-addresses$Guns_adj_street)*addresses$Guns_exp_dev_tract)
addresses$Guns_adj_tract<-ifelse(addresses$Guns_adj_tract>0, addresses$Guns_adj_tract,0)
addresses$Guns_adj_tract<-ifelse(addresses$Guns_adj_tract>0 & addresses$Guns_adj_tract<1, 1, addresses$Guns_adj_tract)

names(addresses)
addresses<-addresses[c(1:38,45:50,57:62)]

##construct streets based on addresses####
streets_nc<-sqldf("select TLID, 
                 sum(PrivateNeglect) as PrivateNeglect, 
                 sum(PublicDenig) as PublicDenig,
                 sum(SocDis) as SocDis,
                 sum(PrivateConflict) as PrivateConflict,
                 sum(Violence) as Violence,
                 sum(Guns) as Guns,
                 sum(PrivateNeglect_adj) as PrivateNeglect_adj, 
                 sum(PublicDenig_adj) as PublicDenig_adj,
                 sum(SocDis_adj) as SocDis_adj,
                 sum(PrivateConflict_adj) as PrivateConflict_adj,
                 sum(Violence_adj) as Violence_adj,
                 sum(Guns_adj) as Guns_adj,
                 sum(PrivateNeglect_adj_street) as PrivateNeglect_adj_street, 
                 sum(PublicDenig_adj_street) as PublicDenig_adj_street,
                 sum(SocDis_adj_street) as SocDis_adj_street,
                 sum(PrivateConflict_adj_street) as PrivateConflict_adj_street,
                 sum(Violence_adj_street) as Violence_adj_street,
                 sum(Guns_adj_street) as Guns_adj_street,
                 sum(PrivateNeglect_adj_tract) as PrivateNeglect_adj_tract, 
                 sum(PublicDenig_adj_tract) as PublicDenig_adj_tract,
                 sum(SocDis_adj_tract) as SocDis_adj_tract,
                 sum(PrivateConflict_adj_tract) as PrivateConflict_adj_tract,
                 sum(Violence_adj_tract) as Violence_adj_tract,
                 sum(Guns_adj_tract) as Guns_adj_tract,
                 count(CT_ID_10) as addresses
                 from addresses
                 where TLID != 'NA'
                 group by TLID")
names(streets_nc)
streets_nc[27:32]<-streets_nc[2:7]/streets_nc$addresses
names(streets_nc)[27:32]<-paste(names(streets_nc)[2:7],'peradd',sep='_')
names(streets_nc)

streets_gini<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                              PrivateNeglect_adj, PublicDenig_adj, SocDis_adj, PrivateConflict_adj, Violence_adj, Guns_adj)~TLID,FUN = gini_prime,data=addresses)
summary(streets_gini)
names(streets_gini)

names(streets_gini)[2:13]<-paste(names(streets_gini)[2:13],'gini',sep='_')
##summary(streets_gini[streets_gini$TLID %in% streets_nc$TLID[streets_nc$PrivateConflict>5 & streets_nc$addresses>5],'PrivateConflict_gini'])
streets_gini<-merge(streets_gini,streets_nc,by='TLID',all.x=TRUE)
names(streets_gini)

####street-level ginis####
ggplot(data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,], aes(x=PrivateNeglect, y=PrivateNeglect_gini)) + geom_point()


summary(lm(PrivateNeglect_gini~PrivateNeglect+addresses,data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,]))
.6941+5*.0088-5*.0021
lm.beta(lm(PrivateNeglect_gini~PrivateNeglect+addresses,data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,]))
summary(lm(PrivateNeglect_adj_gini~PrivateNeglect+addresses,data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,]))
.6878+5*.0092-5*.0022
lm.beta(lm(PrivateNeglect_adj_gini~PrivateNeglect+addresses,data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,]))

summary(lm(PublicDenig_gini~PublicDenig+addresses,data=streets_gini[streets_gini$PublicDenig>5 & streets_gini$addresses>5,]))
.5958+5*.0083-5*.0055
lm.beta(lm(PublicDenig_gini~PublicDenig+addresses,data=streets_gini[streets_gini$PublicDenig>5 & streets_gini$addresses>5,]))
summary(lm(PublicDenig_adj_gini~PublicDenig+addresses,data=streets_gini[streets_gini$PublicDenig>5 & streets_gini$addresses>5,]))
.5927+5*.0084-5*.0056
lm.beta(lm(PublicDenig_adj_gini~PublicDenig+addresses,data=streets_gini[streets_gini$PublicDenig>5 & streets_gini$addresses>5,]))

summary(lm(SocDis_gini~SocDis+addresses,data=streets_gini[streets_gini$SocDis>5 & streets_gini$addresses>5,]))
.7423+5*.003-5*.0057
lm.beta(lm(SocDis_gini~SocDis+addresses,data=streets_gini[streets_gini$SocDis>5 & streets_gini$addresses>5,]))
summary(lm(SocDis_adj_gini~SocDis+addresses,data=streets_gini[streets_gini$SocDis>5 & streets_gini$addresses>5,]))
.7419+5*.0034-5*.0057
lm.beta(lm(SocDis_adj_gini~SocDis+addresses,data=streets_gini[streets_gini$SocDis>5 & streets_gini$addresses>5,]))

summary(lm(PrivateConflict_gini~PrivateConflict+addresses,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))
.5804+5*.0166-5*.0066
lm.beta(lm(PrivateConflict_gini~PrivateConflict+addresses,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))
summary(lm(PrivateConflict_adj_gini~PrivateConflict+addresses,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))
.5603+5*.0175-5*.0067
lm.beta(lm(PrivateConflict_adj_gini~PrivateConflict+addresses,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))

summary(lm(Violence_gini~Violence+addresses,data=streets_gini[streets_gini$Violence>5 & streets_gini$addresses>5,]))
.7241+5*.0058-5*.0060
lm.beta(lm(Violence_gini~Violence+addresses,data=streets_gini[streets_gini$Violence>5 & streets_gini$addresses>5,]))
summary(lm(Violence_adj_gini~Violence+addresses,data=streets_gini[streets_gini$Violence>5 & streets_gini$addresses>5,]))
.7157+5*.0062-5*.0063
lm.beta(lm(Violence_adj_gini~Violence+addresses,data=streets_gini[streets_gini$Violence>5 & streets_gini$addresses>5,]))

summary(lm(Guns_gini~Guns+addresses,data=streets_gini[streets_gini$Guns>4 & streets_gini$addresses>4,]))
.545+5*.0266-5*.0044
lm.beta(lm(Guns_gini~Guns+addresses,data=streets_gini[streets_gini$Guns>4 & streets_gini$addresses>4,]))
summary(lm(Guns_adj_gini~Guns+addresses,data=streets_gini[streets_gini$Guns>4 & streets_gini$addresses>4,]))
.5451+5*.0266-5*.0043
lm.beta(lm(Guns_adj_gini~Guns+addresses,data=streets_gini[streets_gini$Guns>4 & streets_gini$addresses>4,]))

##compare to expected ginis##
require(reshape2)
melted<-melt(streets_gini[c(38:44)],id.vars=c("addresses"))
names(melted)
melted<-melted[c(1,3)]
melted<-melted[!duplicated(melted) & melted$addresses>5 & melted$value*melted$addresses>5,]
View(melted)

for (i in 1:nrow(melted)) {
  melted$pois_exp[i]<-pois_test(melted$value[i],melted$addresses[i])
}

streets_gini<-merge(streets_gini,melted,by.x=c('addresses','PrivateNeglect_peradd'),by.y=c('addresses','value'),all.x=TRUE)
streets_gini<-merge(streets_gini,melted,by.x=c('addresses','PublicDenig_peradd'),by.y=c('addresses','value'),all.x=TRUE)
streets_gini<-merge(streets_gini,melted,by.x=c('addresses','SocDis_peradd'),by.y=c('addresses','value'),all.x=TRUE)
streets_gini<-merge(streets_gini,melted,by.x=c('addresses','PrivateConflict_peradd'),by.y=c('addresses','value'),all.x=TRUE)
streets_gini<-merge(streets_gini,melted,by.x=c('addresses','Violence_peradd'),by.y=c('addresses','value'),all.x=TRUE)
streets_gini<-merge(streets_gini,melted,by.x=c('addresses','Guns_peradd'),by.y=c('addresses','value'),all.x=TRUE)
names(streets_gini)[45:50]<-paste(names(streets_gini)[21:26],'pois_exp',sep='_')

summary(lm(PrivateNeglect_pois_exp~PrivateNeglect+addresses,data=streets_gini[streets_gini$PrivateNeglect>5 & streets_gini$addresses>5,]))
.2812+5*.0099-5*.0044
summary(lm(PublicDenig_pois_exp~PublicDenig+addresses,data=streets_gini[streets_gini$PublicDenig>5 & streets_gini$addresses>5,]))
.3535+5*.0039-5*.0056
summary(lm(SocDis_pois_exp~SocDis+addresses,data=streets_gini[streets_gini$SocDis>5 & streets_gini$addresses>5,]))
.3857+5*.0028-5*.0067
summary(lm(PrivateConflict_pois_exp~PrivateConflict+addresses,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))
.2806+5*.0123-5*.0059
summary(lm(Violence_pois_exp~Violence+addresses,data=streets_gini[streets_gini$Violence>5 & streets_gini$addresses>5,]))
.3736+5*.0024-5*.0062
summary(lm(Guns_pois_exp~Guns+addresses,data=streets_gini[streets_gini$Guns>4 & streets_gini$addresses>4,]))
.2416+5*.0251-5*.0095


##control for number of addresses on street####

summary(glm(PrivateNeglect_adj_street~addresses,data=streets_gini,family=poisson))
streets_gini$PrivateNeglect_adj_street_adds<-ifelse(residuals(glm(PrivateNeglect_adj_street~addresses,data=streets_gini,family=poisson))>=0,streets_gini$PrivateNeglect_adj_street-fitted.values(glm(PrivateNeglect_adj_street~addresses,data=streets_gini,family=poisson)),0)
streets_gini$PrivateNeglect_adj_street_adds<-ifelse(streets_gini$PrivateNeglect_adj_street_adds>0 & streets_gini$PrivateNeglect_adj_street_adds<1,1,streets_gini$PrivateNeglect_adj_street_adds)
summary(streets_gini$PrivateNeglect_adj_street_adds)
summary(streets_gini$PrivateNeglect_adj_street)

summary(glm(PublicDenig_adj_street~addresses,data=streets_gini,family=poisson))
streets_gini$PublicDenig_adj_street_adds<-ifelse(residuals(glm(PublicDenig_adj_street~addresses,data=streets_gini,family=poisson))>=0,streets_gini$PublicDenig_adj_street-fitted.values(glm(PublicDenig_adj_street~addresses,data=streets_gini,family=poisson)),0)
streets_gini$PublicDenig_adj_street_adds<-ifelse(streets_gini$PublicDenig_adj_street_adds>0 & streets_gini$PublicDenig_adj_street_adds<1,1,streets_gini$PublicDenig_adj_street_adds)
summary(streets_gini$PublicDenig_adj_street_adds)
summary(streets_gini$PublicDenig_adj_street)

summary(glm(SocDis_adj_street~addresses,data=streets_gini,family=poisson))
streets_gini$SocDis_adj_street_adds<-ifelse(residuals(glm(SocDis_adj_street~addresses,data=streets_gini,family=poisson))>=0,streets_gini$SocDis_adj_street-fitted.values(glm(SocDis_adj_street~addresses,data=streets_gini,family=poisson)),0)
streets_gini$SocDis_adj_street_adds<-ifelse(streets_gini$SocDis_adj_street_adds>0 & streets_gini$SocDis_adj_street_adds<1,1,streets_gini$SocDis_adj_street_adds)
summary(streets_gini$SocDis_adj_street_adds)
summary(streets_gini$SocDis_adj_street)

summary(glm(PrivateConflict_adj_street~addresses+numUnits,data=streets_gini,family=poisson))
streets_gini$PrivateConflict_adj_street_adds<-ifelse(residuals(glm(PrivateConflict_adj_street~addresses+numUnits,data=streets_gini,family=poisson))>=0,streets_gini$PrivateConflict_adj_street-fitted.values(glm(PrivateConflict_adj_street~addresses+numUnits,data=streets_gini,family=poisson)),0)
streets_gini$PrivateConflict_adj_street_adds<-ifelse(streets_gini$PrivateConflict_adj_street_adds>0 & streets_gini$PrivateConflict_adj_street_adds<1,1,streets_gini$PrivateConflict_adj_street_adds)
summary(streets_gini$PrivateConflict_adj_street_adds)
summary(streets_gini$PrivateConflict_adj_street)

summary(glm(Violence_adj_street~addresses,data=streets_gini,family=poisson))
streets_gini$Violence_adj_street_adds<-ifelse(residuals(glm(Violence_adj_street~addresses,data=streets_gini,family=poisson))>=0,streets_gini$Violence_adj_street-fitted.values(glm(Violence_adj_street~addresses,data=streets_gini,family=poisson)),0)
streets_gini$Violence_adj_street_adds<-ifelse(streets_gini$Violence_adj_street_adds>0 & streets_gini$Violence_adj_street_adds<1,1,streets_gini$Violence_adj_street_adds)
summary(streets_gini$Violence_adj_street_adds)
summary(streets_gini$Violence_adj_street)

summary(glm(Guns_adj_street~addresses,data=streets_gini,family=poisson))
streets_gini$Guns_adj_street_adds<-ifelse(residuals(glm(Guns_adj_street~addresses,data=streets_gini,family=poisson))>=0,streets_gini$Guns_adj_street-fitted.values(glm(Guns_adj_street~addresses,data=streets_gini,family=poisson)),0)
streets_gini$Guns_adj_street_adds<-ifelse(streets_gini$Guns_adj_street_adds>0 & streets_gini$Guns_adj_street_adds<1,1,streets_gini$Guns_adj_street_adds)
summary(streets_gini$Guns_adj_street_adds)
summary(streets_gini$Guns_adj_street)

####construct tracts based on streets####
streets_gini<-merge(streets_gini,streets[c('TLID','CT_ID_10')],by='TLID',all.x=TRUE)
tracts_gini_streets<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                                     PrivateNeglect_adj,PublicDenig_adj,SocDis_adj,PrivateConflict_adj,Violence_adj,Guns_adj,
                                     PrivateNeglect_adj_street,PublicDenig_adj_street,SocDis_adj_street,PrivateConflict_adj_street,Violence_adj_street,Guns_adj_street,
                                     PrivateNeglect_adj_tract,PublicDenig_adj_tract,SocDis_adj_tract,PrivateConflict_adj_tract,Violence_adj_tract,Guns_adj_tract,
                                     PrivateNeglect_adj_street_adds,PublicDenig_adj_street_adds,SocDis_adj_street_adds,PrivateConflict_adj_street_adds,Violence_adj_street_adds,Guns_adj_street_adds)~CT_ID_10,FUN = gini_prime,data=streets_gini)
names(tracts_gini_streets)[2:31]<-paste(names(tracts_gini_streets)[2:31],'gini',sep='_')
summary(tracts_gini_streets)

tracts_nc<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                           PrivateNeglect_adj,PublicDenig_adj,SocDis_adj,PrivateConflict_adj,Violence_adj,Guns_adj,
                           PrivateNeglect_adj_street,PublicDenig_adj_street,SocDis_adj_street,PrivateConflict_adj_street,Violence_adj_street,Guns_adj_street,
                           PrivateNeglect_adj_tract,PublicDenig_adj_tract,SocDis_adj_tract,PrivateConflict_adj_tract,Violence_adj_tract,Guns_adj_tract,
                           PrivateNeglect_adj_street_adds,PublicDenig_adj_street_adds,SocDis_adj_street_adds,PrivateConflict_adj_street_adds,Violence_adj_street_adds,Guns_adj_street_adds,!is.na(TLID),addresses)~CT_ID_10,FUN = sum,data=streets_gini)
names(tracts_nc)[32]<-'streets'

tracts_nc[34:39]<-tracts_nc[2:7]/tracts_nc$streets
summary(tracts_nc[34:39])
names(tracts_nc)[34:39]<-paste(names(tracts_nc)[2:7],'perstrt',sep='_')

####tract-level ginis####
tracts_gini_streets<-merge(tracts_gini_streets,tracts_nc,by='CT_ID_10',all.x=TRUE)
names(tracts_gini_streets)

summary(lm(PrivateNeglect_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
.6991+5*.001-5*.0002
lm.beta(lm(PrivateNeglect_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateNeglect_adj_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
.6931+5*.0011-5*.0002
lm.beta(lm(PrivateNeglect_adj_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateNeglect_adj_street_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
.6922+5*.001-5*.0002
lm.beta(lm(PrivateNeglect_adj_street_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateNeglect_adj_street_adds_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
.6164+5*.002-5*.0005
lm.beta(lm(PrivateNeglect_adj_street_adds_gini~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))

summary(lm(PublicDenig_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
.5017+5*.0027-5*.0005
lm.beta(lm(PublicDenig_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
summary(lm(PublicDenig_adj_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
.5005+5*.0027-5*.0005
lm.beta(lm(PublicDenig_adj_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
summary(lm(PublicDenig_adj_street_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
.5002+5*.0027-5*.0005
lm.beta(lm(PublicDenig_adj_street_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
summary(lm(PublicDenig_adj_street_adds_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
.4269+5*.0036-5*.0008
lm.beta(lm(PublicDenig_adj_street_adds_gini~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))

summary(lm(SocDis_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
.4768+5*.0061-5*.0009
lm.beta(lm(SocDis_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
summary(lm(SocDis_adj_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
.4762+5*.0061-5*.0009
lm.beta(lm(SocDis_adj_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
summary(lm(SocDis_adj_street_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
.476+5*.0061-5*.0009
lm.beta(lm(SocDis_adj_street_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
summary(lm(SocDis_adj_street_adds_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
.4333+5*.0066-5*.001
lm.beta(lm(SocDis_adj_street_adds_gini~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))

summary(lm(PrivateConflict_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
.5994+5*.0011-5*.0006
lm.beta(lm(PrivateConflict_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateConflict_adj_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
.591+5*.0011-5*.0007
lm.beta(lm(PrivateConflict_adj_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateConflict_adj_street_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
.5927+5*.0012-5*.0007
lm.beta(lm(PrivateConflict_adj_street_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
summary(lm(PrivateConflict_adj_street_adds_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
.449+5*.0025-5*.0012
lm.beta(lm(PrivateConflict_adj_street_adds_gini~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))

summary(lm(Violence_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
.6948+5*.0005+5*.0001
lm.beta(lm(Violence_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
summary(lm(Violence_adj_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
.6892+5*.0005+5*.0001
lm.beta(lm(Violence_adj_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
summary(lm(Violence_adj_street_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
.691+5*.0005+5*.0001
lm.beta(lm(Violence_adj_street_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
summary(lm(Violence_adj_street_adds_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
.6685+5*.0012+5*.0006
lm.beta(lm(Violence_adj_street_adds_gini~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))

summary(lm(Guns_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
.3596+5*.0112-5*.0011
lm.beta(lm(Guns_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
summary(lm(Guns_adj_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
.3593+5*.0112-5*.0011
lm.beta(lm(Guns_adj_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
summary(lm(Guns_adj_street_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
.3589+5*.0112-5*.0011
lm.beta(lm(Guns_adj_street_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
summary(lm(Guns_adj_street_adds_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))
.3357+5*.0113-5*.0011
lm.beta(lm(Guns_adj_street_adds_gini~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>4 & tracts_gini_streets$streets>4,]))

ggplot(data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,], aes(x=PrivateConflict, y=PrivateConflict_adj_street_adds_gini)) + geom_point()

##compare to expected ginis##
melted<-melt(tracts_gini_streets[c(62,64:69)],id.vars=c("streets"))
names(melted)
melted<-melted[c(1,3)]
melted<-melted[!duplicated(melted) & melted$streets>5 & melted$value*melted$streets>5,]
View(melted)

for (i in 1:nrow(melted)) {
  melted$pois_exp[i]<-pois_test(melted$value[i],melted$streets[i])
}

tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','PrivateNeglect_perstrt'),by.y=c('streets','value'),all.x=TRUE)
tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','PublicDenig_perstrt'),by.y=c('streets','value'),all.x=TRUE)
tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','SocDis_perstrt'),by.y=c('streets','value'),all.x=TRUE)
tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','PrivateConflict_perstrt'),by.y=c('streets','value'),all.x=TRUE)
tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','Violence_perstrt'),by.y=c('streets','value'),all.x=TRUE)
tracts_gini_streets<-merge(tracts_gini_streets,melted,by.x=c('streets','Guns_perstrt'),by.y=c('streets','value'),all.x=TRUE)
names(tracts_gini_streets)[70:75]<-paste(names(tracts_gini_streets)[39:44],'pois_exp',sep='_')

summary(lm(PrivateNeglect_pois_exp~PrivateNeglect+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateNeglect>5 & tracts_gini_streets$streets>5,]))
.3482+5*.0017-5*.0012
summary(lm(PublicDenig_pois_exp~PublicDenig+streets,data=tracts_gini_streets[tracts_gini_streets$PublicDenig>5 & tracts_gini_streets$streets>5,]))
.2604+5*.0023-5*.0011
summary(lm(SocDis_pois_exp~SocDis+streets,data=tracts_gini_streets[tracts_gini_streets$SocDis>5 & tracts_gini_streets$streets>5,]))
.2275+5*.004-5*.0014
summary(lm(PrivateConflict_pois_exp~PrivateConflict+streets,data=tracts_gini_streets[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5,]))
.3547+5*.0008-5*.0006
summary(lm(Violence_pois_exp~Violence+streets,data=tracts_gini_streets[tracts_gini_streets$Violence>5 & tracts_gini_streets$streets>5,]))
.3662+5*.0002-5*.0002
summary(lm(Guns_pois_exp~Guns+streets,data=tracts_gini_streets[tracts_gini_streets$Guns>5 & tracts_gini_streets$streets>5,]))
.1382+5*.008-5*.0012

##control for number of streets in tract####

#summary(glm(PrivateNeglect_adj_tract~streets,data=tracts_gini_streets,family=poisson))
summary(glm(PrivateNeglect_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))
tracts_gini_streets$PrivateNeglect_adj_tract_adds<-tracts_gini_streets$PrivateNeglect_adj_street_adds-fitted.values(glm(PrivateNeglect_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$PrivateNeglect_adj_street_adds-fitted.values(glm(PrivateNeglect_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$PrivateNeglect_adj_tract_adds)
summary(tracts_gini_streets$PrivateNeglect_adj_tract)

summary(glm(PublicDenig_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))
tracts_gini_streets$PublicDenig_adj_tract_adds<-tracts_gini_streets$PublicDenig_adj_street_adds-fitted.values(glm(PublicDenig_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$PublicDenig_adj_street_adds-fitted.values(glm(PublicDenig_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$PublicDenig_adj_tract_adds)
summary(tracts_gini_streets$PublicDenig_adj_tract)

summary(glm(SocDis_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))
tracts_gini_streets$SocDis_adj_tract_adds<-tracts_gini_streets$SocDis_adj_street_adds-fitted.values(glm(SocDis_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$SocDis_adj_street_adds-fitted.values(glm(SocDis_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$SocDis_adj_tract_adds)
summary(tracts_gini_streets$SocDis_adj_tract)

summary(glm(PrivateConflict_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))
tracts_gini_streets$PrivateConflict_adj_tract_adds<-tracts_gini_streets$PrivateConflict_adj_street_adds-fitted.values(glm(PrivateConflict_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$PrivateConflict_adj_street_adds-fitted.values(glm(PrivateConflict_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$PrivateConflict_adj_tract_adds)
summary(tracts_gini_streets$PrivateConflict_adj_tract)

#summary(glm(Violence_adj_tract~streets,data=tracts_gini_streets,family=poisson))
summary(glm(Violence_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))

tracts_gini_streets$Violence_adj_tract_adds<-tracts_gini_streets$Violence_adj_street_adds-fitted.values(glm(Violence_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$Violence_adj_street_adds-fitted.values(glm(Violence_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$Violence_adj_tract_adds)
summary(tracts_gini_streets$Violence_adj_tract)

summary(glm(Guns_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))
tracts_gini_streets$Guns_adj_tract_adds<-tracts_gini_streets$Guns_adj_street_adds-fitted.values(glm(Guns_adj_street_adds~streets,data=tracts_gini_streets,family=poisson))-min(tracts_gini_streets$Guns_adj_street_adds-fitted.values(glm(Guns_adj_street_adds~streets,data=tracts_gini_streets,family=poisson)))
summary(tracts_gini_streets$Guns_adj_tract_adds)
summary(tracts_gini_streets$Guns_adj_tract)




##tracts in city####
apply(tracts_gini_streets[c(39:68,76:81)],2,gini_prime)

####construct pds based on tracts####
tracts_gini_streets<-merge(tracts_gini_streets,tracts[c('BRA_PD','CT_ID_10')],by='CT_ID_10',all.x=TRUE)
pds_gini_tracts<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                                 PrivateNeglect_adj,PublicDenig_adj,SocDis_adj,PrivateConflict_adj,Violence_adj,Guns_adj,
                                 PrivateNeglect_adj_street,PublicDenig_adj_street,SocDis_adj_street,PrivateConflict_adj_street,Violence_adj_street,Guns_adj_street,
                                 PrivateNeglect_adj_tract,PublicDenig_adj_tract,SocDis_adj_tract,PrivateConflict_adj_tract,Violence_adj_tract,Guns_adj_tract,
                                 PrivateNeglect_adj_street_adds,PublicDenig_adj_street_adds,SocDis_adj_street_adds,PrivateConflict_adj_street_adds,Violence_adj_street_adds,Guns_adj_street_adds,
                                 PrivateNeglect_adj_tract_adds,PublicDenig_adj_tract_adds,SocDis_adj_tract_adds,PrivateConflict_adj_tract_adds,Violence_adj_tract_adds,Guns_adj_tract_adds)~BRA_PD,FUN = gini_prime,data=tracts_gini_streets)
names(pds_gini_tracts)[2:37]<-paste(names(pds_gini_tracts)[2:37],'gini',sep='_')
summary(pds_gini_tracts)

pds_nc<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                        PrivateNeglect_adj,PublicDenig_adj,SocDis_adj,PrivateConflict_adj,Violence_adj,Guns_adj,
                        PrivateNeglect_adj_street,PublicDenig_adj_street,SocDis_adj_street,PrivateConflict_adj_street,Violence_adj_street,Guns_adj_street,
                        PrivateNeglect_adj_tract,PublicDenig_adj_tract,SocDis_adj_tract,PrivateConflict_adj_tract,Violence_adj_tract,Guns_adj_tract,
                        PrivateNeglect_adj_street_adds,PublicDenig_adj_street_adds,SocDis_adj_street_adds,PrivateConflict_adj_street_adds,Violence_adj_street_adds,Guns_adj_street_adds,
                        PrivateNeglect_adj_tract_adds,PublicDenig_adj_tract_adds,SocDis_adj_tract_adds,PrivateConflict_adj_tract_adds,Violence_adj_tract_adds,Guns_adj_tract_adds,!is.na(CT_ID_10),streets,addresses)~BRA_PD,FUN = sum,data=tracts_gini_streets)
names(pds_nc)[38]<-'tracts'

pds_nc[41:46]<-pds_nc[2:7]/pds_nc$tracts
names(pds_nc)[41:46]<-paste(names(pds_nc)[2:7],'pertract',sep='_')

####


####extra####
tracts_nc<-sqldf("select CT_ID_10, 
                  sum(PrivateNeglect) as PrivateNeglect, 
                 sum(PublicDenig) as PublicDenig,
                 sum(SocDis) as SocDis,
                 sum(PrivateConflict) as PrivateConflict,
                 sum(Violence) as Violence,
                 sum(Guns) as Guns,
                 sum(MedAdjust) as MedAdjust,
                 sum(Total) as Total,
                 count(CT_ID_10) as addresses
                 from addresses
                 where CT_ID_10 != 'NA'
                 group by CT_ID_10")
View(tracts_nc)

tracts_nc[11:18]<-tracts_nc[2:9]/tracts_nc$addresses
summary(tracts_nc[11:18])
names(tracts_nc)[11:18]<-paste(names(tracts_nc)[2:9],'peradd',sep='_')

tracts_gini<-aggregate(Violence~CT_ID_10,FUN = gini_prime,data=addresses)
tracts_gini<-tracts_gini[tracts_gini$CT_ID_10 %in% tracts_nc$CT_ID_10[tracts_nc$Violence>0],]
summary(tracts_gini)

gini_plot<-ggplot(streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,], aes(x=PrivateConflict_gini))+geom_histogram()
gini_plot

tracts_gini<-merge(tracts_gini,tracts_nc,by='CT_ID_10',all.x=TRUE)
names(tracts_gini)


####gini comparisons####
mean(streets_gini$PrivateConflict_peradd[streets_gini$PrivateConflict>5 & streets_gini$addresses>5])
pois_test<-gini_prime(rpois(100,.64))
for (i in 2:10000) {
  pois_test[i]<-gini_prime(rpois(100,.64))
}
mean(pois_test)
mean(streets_gini$PrivateConflict_gini[streets_gini$PrivateConflict_gini!='-Inf' & streets_gini$PrivateConflict>5 & streets_gini$addresses>5],na.rm = TRUE)
sd(streets_gini$PrivateConflict_gini[streets_gini$PrivateConflict_gini!='-Inf' & streets_gini$PrivateConflict>5 & streets_gini$addresses>5])/sqrt(length((streets_gini$PrivateConflict_gini[streets_gini$PrivateConflict_gini!='-Inf' & streets_gini$PrivateConflict>5 & streets_gini$addresses>5])))

mean(tracts_gini_streets$PrivateConflict_perstrt[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5])
pois_test_tracts<-gini_prime(rpois(100,2.5))
for (i in 2:10000) {
  pois_test_tracts[i]<-gini_prime(rpois(100,2.5))
}
mean(pois_test_tracts)
mean(tracts_gini_streets$PrivateConflict_gini[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5])
sd(tracts_gini_streets$PrivateConflict_gini[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5])/sqrt(length(tracts_gini_streets$PrivateConflict_gini[tracts_gini_streets$PrivateConflict>5 & tracts_gini_streets$streets>5]))



require(lme4)

?glmer

names(addresses)

PrivConf_hlm<-glmer(PrivateConflict ~ usedparcel + land_usage + (1 | TLID), data=addresses, family=poisson())
summary(PrivConf_hlm)
PrivateConflict_resids<-resid(PrivConf_hlm)

street_PrivConf_extra<-aggregate(PrivateConflict_resid~TLID,data=PrivateConflict_resids, FUN=sum)
streets_gini<-merge(streets_gini, street_PrivConf_extra, by='TLID', all.x=TRUE)
streets_gini[ncol(streets_gini)+1]<-streets_gini$PrivateConflict_resid/streets_gini$addresses
names(streets_gini)[ncol(streets_gini)]<-paste(ncol(streets_gini),'peradd',sep='_')

streets_PrivConf_extra_gini<-aggregate(PrivateConflict_resid~TLID,FUN = gini_prime,data=addresses)
names(streets_PrivConf_extra_gini)[2]<-paste(names(streets_PrivConf_extra_gini)[2],'gini',sep='_')
streets_gini<-merge(streets_gini,streets_PrivConf_extra_gini,by='TLID',all.x=TRUE)

summary(lm(PrivateConflict_resid_gini~PrivateConflict_resid+PrivateConflict_resid_peradd,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))
lm.beta(lm(PrivateConflict_gini~PrivateConflict+PrivateConflict_peradd,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))

streets_gini<-merge(streets_gini[1:26],streets[c(1,16,20,24:28)],by='TLID',all.x=TRUE)
summary(lm(PrivateConflict_gini~PrivateConflict+PrivateConflict_peradd+Length+Main+ZoningExmp+ZoningInd+ZoningNone+ZoningComm,data=streets_gini[streets_gini$PrivateConflict>5 & streets_gini$addresses>5,]))





names(addresses)

streets_gini$PrivateConflict_exp_dev<-2.71828^((streets_gini$Length-93.71)*(-.000253)+.00055*streets_gini$usedparcel+.207676*streets_gini$Main+.416346*streets_gini$ZoningExmp+.235291*streets_gini$ZoningInd+.468739*streets_gini$ZoningNone+.167217*streets_gini$ZoningComm)
addresses_temp<-merge(addresses,streets_gini[c(1,38)],by='TLID',all.x=TRUE)
addresses_temp$PrivateConflict_adj_street<-addresses_temp$PrivateConflict-((addresses_temp$PrivateConflict-addresses_temp$PrivateConflict_adj)*addresses_temp$PrivateConflict_exp_dev)
addresses_temp$PrivateConflict_adj_street<-ifelse(addresses_temp$PrivateConflict_adj_street>0, addresses_temp$PrivateConflict_adj_street,0)
addresses_temp$PrivateConflict_adj_street<-ifelse(addresses_temp$PrivateConflict_adj_street>0 & addresses_temp$PrivateConflict_adj_street<1, 1, addresses_temp$PrivateConflict_adj_street)

View(addresses_temp)



streets_gini<-aggregate(cbind(PrivateNeglect,PublicDenig,SocDis,PrivateConflict,Violence,Guns,
                              PrivateNeglect_adj, PublicDenig_adj, SocDis_adj, PrivateConflict_adj, Violence_adj, Guns_adj,
                              PrivateNeglect_adj_street, PublicDenig_adj_street, SocDis_adj_street, PrivateConflict_adj_street, Violence_adj_street, Guns_adj_street,
                              PrivateNeglect_adj_street_adds_gini_adj_street_adds, PublicDenig_adj_street_adds, SocDis_adj_street_adds, PrivateConflict_adj_street_adds, Violence_adj_street_adds, Guns_adj_street_adds)~TLID,FUN = gini_prime,data=addresses)
