##[SCIENDO]=group
##iteration=number 10

library(ggplot2)
library(foreign)
library(rtf)
library(sp)
library(raster)
library(reshape2)
library(tcltk)
#include_peat=1

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#READ LUMENS LOG FILE
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/", username, sep="")
} else {
  user_path<-paste("C:/Users/", username, sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# SELECTING AVAILABLE QUES-C ANALYSIS

QUESC_list<-as.data.frame(ls(pattern="QUESC_database"))
colnames (QUESC_list) [1]<-"Data"
QUESC_list$Usage<-0

QUESC_list_n<-nrow(QUESC_list)
if(QUESC_list_n<3){
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Minimum three QUES-C Analysis Result required",
                         icon = "info",
                         type = "ok")
  quit()
}

repeat{
  QUESC_list<-edit(QUESC_list)
  if(sum(QUESC_list$Usage)>2){
    break
  }
}


QUESC_list <- QUESC_list[which(QUESC_list$Usage==1),]
QUESC_list$Usage<-NULL
QUESC_list_n<-nrow(QUESC_list)
dbase_all<-NULL

data<-as.character(QUESC_list [QUESC_list_n,1])
t1<-as.integer(substr(data, 16:19, 19))
t2<-as.integer(substr(data, 21:24, 24))
eval(parse(text=(paste("central_data<-QUESC_database_", t1,"_", t2, sep=""))))
central_data$key<- do.call(paste, c(central_data[c("LU_CHG", "Z_NAME")], sep = " in "))


for(i in 1:QUESC_list_n) {
  data<-as.character(QUESC_list [i,1])
  t1<-as.integer(substr(data, 16:19, 19))
  t2<-as.integer(substr(data, 21:24, 24))
  eval(parse(text=(paste("dbase<-QUESC_database_", t1,"_", t2, sep=""))))
  dbase$Start_year<-t1
  dbase$End_year<-t2
  dbase$nYear<-dbase$End_year-dbase$Start_year
  data2<-dbase
  
  data2$ID_LC1<-as.character(data2$ID_LC1)
  data2$ID_LC2<-as.character(data2$ID_LC2)
  
  data2.1<-subset(data2, ID_LC1==ID_LC2, select = ZONE:nYear)
  data2.2<-subset(data2, ID_LC1!=ID_LC2, select = ZONE:nYear)
  
  data2.1$ID_LC1<-as.factor(data2.1$ID_LC1)
  data2.1$ID_LC2<-as.factor(data2.1$ID_LC2) 
  data2.2$ID_LC1<-as.factor(data2.2$ID_LC1)
  data2.2$ID_LC2<-as.factor(data2.2$ID_LC2)  
  
  data2.2$COUNTx<-data2.2$COUNT/data2.2$nYear
  data2.2$COUNTy<-data2.2$COUNT-data2.2$COUNTx
  data2.2$COUNT<-data2.2$COUNTx
  data2.2$COUNTx<-NULL
  data2.2melt <- melt(data = data2.2, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNTy'))
  data2.2cast<- dcast(data = data2.2melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum,fill = 0, drop = FALSE)
  data2.2cast$key<- do.call(paste, c(data2.2cast[c("LC_t1", "Z_NAME")], sep = " in "))
  data2.2cast$LC_t1<-NULL
  data2.2cast$Z_NAME<-NULL
  data2.1$key<- do.call(paste, c(data2.1[c("LC_t1", "Z_NAME")], sep = " in "))
  data2.1<-merge(data2.1, data2.2cast, by="key", all=TRUE)
  data2.1<-replace(data2.1, is.na(data2.1), 0)
  data2.1$COUNT<-data2.1$COUNT+data2.1$.
  data2.1$key<-NULL
  data2.1$.<-NULL
  data2.2$COUNTy<-NULL
  data2ann<-rbind(data2.1,data2.2)
  
  #CALCULATE TRANSITION PROBABILITY MATRIX
  n.zone<-nrow(as.data.frame(unique(data2ann$Z_NAME)))
  data2.melt <- melt(data = data2ann, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('COUNT'))
  lu.count.zone.t2<- dcast(data = data2.melt, formula = LC_t2 + Z_NAME ~ ., fun.aggregate = sum,fill = 0, drop = FALSE)
  data2.melt <- melt(data = data2ann, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNT'))
  lu.count.zone.t1<- dcast(data = data2.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
  colnames(lu.count.zone.t1)[3]<-"COUNT.LU.ZONE.t1"
  colnames(lu.count.zone.t2)[3]<-"COUNT.LU.ZONE.t2"
  data2ann<-merge(data2ann,lu.count.zone.t1, by=c("LC_t1", "Z_NAME"), all=TRUE)
  data2ann<-replace(data2ann, is.na(data2ann), 0)
  data2ann<-merge(data2ann,lu.count.zone.t2, by.x=c("LC_t1", "Z_NAME"), by.y=c("LC_t2", "Z_NAME"), all=TRUE)
  data2ann<-replace(data2ann, is.na(data2ann), 0)
  eval(parse(text=(paste("data2ann$TPM", i, "<-data2ann$COUNT/data2ann$COUNT.LU.ZONE.t1", sep=""))))
  #data2ann<-replace(data2ann, is.na(data2ann), 0)
  
  #HANDLING NEW EMERGING LAND USE TYPE IN TPM
  data2ann <- replace(data2ann, is.na(data2ann), 0)
  data2.cek<- melt(data = data2ann, id.vars=c('ID_LC1','ZONE'), measure.vars=c(paste("TPM", i, sep="")))
  data2.cek<- dcast(data = data2.cek, formula = ID_LC1 + ZONE ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
  colnames(data2.cek)[3]<-"CEK"
  data2.cek1<-subset(data2.cek, CEK==0)
  data2.cek1$ACT<-"Fix"
  data2.cek2<-subset(data2.cek, CEK>0)
  data2.cek2$ACT<-"Ignore"
  data2.cek<-rbind(data2.cek1,data2.cek2)
  data2.cek$CEK<-NULL
  data3<-merge(data2ann,data2.cek, by=c("ID_LC1", "ZONE"), all=TRUE)
  data3<-replace(data3, is.na(data3), 0)
  data3.cek1<-subset(data3, ACT=="Fix")
  data3.cek2<-subset(data3, ACT=="Ignore")
  
  data3.cek1$ID_LC1<-as.character(data3.cek1$ID_LC1)
  data3.cek1$ID_LC2<-as.character(data3.cek1$ID_LC2)
  
  data3.cek1a<-subset(data3.cek1, ID_LC1==ID_LC2)
  data3.cek1b<-subset(data3.cek1, ID_LC1!=ID_LC2)
    
  data3.cek1a$ID_LC1<-as.factor(data3.cek1a$ID_LC1)
  data3.cek1a$ID_LC2<-as.factor(data3.cek1a$ID_LC2) 
  data3.cek1b$ID_LC1<-as.factor(data3.cek1b$ID_LC1)
  data3.cek1b$ID_LC2<-as.factor(data3.cek1b$ID_LC2)  
  
  eval(parse(text=(paste("data3.cek1a$TPM", i, "<-1", sep=""))))
  data4<-rbind(data3.cek1a,data3.cek1b,data3.cek2)
  data4$key<- do.call(paste, c(data4[c("LU_CHG", "Z_NAME")], sep = " in "))
  eval(parse(text=(paste("data4<-subset(data4,select=c(key, TPM", i, "))", sep=""))))
  central_data<-merge(central_data, data4, by="key", all=TRUE)
}
data2ori<-data2
data2<-replace(central_data, is.na(central_data), 0)

command<-NULL
for(j in 1:QUESC_list_n){
  if (j!=QUESC_list_n) {
    command<-paste(command,"data2$TPM", j, "+", sep="")
  } else {
    command<-paste(command,"data2$TPM", j, sep="")
  }
}

eval(parse(text=(paste("data2$TPM1 <-(", command, ")/QUESC_list_n", sep="" ))))

n.zone<-nrow(as.data.frame(unique(data2$Z_NAME)))
data2.melt <- melt(data = data2, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('COUNT'))
lu.count.zone.t2<- dcast(data = data2.melt, formula = LC_t2 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
data2.melt <- melt(data = data2, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('COUNT'))
lu.count.zone.t1<- dcast(data = data2.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
colnames(lu.count.zone.t1)[3]<-"COUNT.LU.ZONE.t1"
colnames(lu.count.zone.t2)[3]<-"COUNT.LU.ZONE.t2"
data2<-merge(data2,lu.count.zone.t1, by=c("LC_t1", "Z_NAME"), all=TRUE)
data2<-merge(data2,lu.count.zone.t2, by.x=c("LC_t1", "Z_NAME"), by.y=c("LC_t2", "Z_NAME"), all=TRUE)
data2<-replace(data2, is.na(data2), 0)


#CALCULATE PREDICTED AREA AT ITERATION 1
data4<-data2
data4$COUNT.it0<-data4$COUNT
data4$COUNT.it1<-data4$TPM1*data4$COUNT.LU.ZONE.t2

#CALCULATE PREDICTED AREA FOR NEXT N ITERATION
for (w in 2:iteration) {
  eval(parse(text=(paste("data4.melt <- melt(data = data4, id.vars=c('LC_t2','ZONE'), measure.vars=c('COUNT.it",w-1,"'))", sep=""))))
  eval(parse(text=(paste("lu.count.zone.t", w+1, "<- dcast(data = data4.melt, formula = LC_t2 + ZONE ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)", sep=""))))
  eval(parse(text=(paste("colnames(lu.count.zone.t", w+1,')[3]<-"COUNT.LU.ZONE.t', w+1, '"', sep=""))))
  eval(parse(text=(paste('data4<-merge(data4,lu.count.zone.t', w+1, ', by.x=c("LC_t1", "ZONE"), by.y=c("LC_t2", "ZONE"), all=TRUE)', sep=""))))
  data4<-replace(data4, is.na(data4), 0)
  eval(parse(text=(paste("data4$COUNT.it", w, "<-data4$TPM1*data4$COUNT.LU.ZONE.t", w+1, sep=""))))
}

write.dbf(data4,"SCIENDO-LUWES_annual_database.dbf")

annual_emission<-NULL
for (y in 0:iteration) {
  eval(parse(text=(paste("data4$em_t", y,"<-data4$COUNT.it", y, "*(data4$CARBON_t1-data4$CARBON_t2)*data4$ck_em*3.67", sep=""))))
  eval(parse(text=(paste("emtot<-sum(data4$em_t", y,")", sep=""))))
  annual_emission<-c(annual_emission, emtot)
}

annual_sequestration<-NULL
for (x in 0:iteration) {
  eval(parse(text=(paste("data4$sq_t", x,"<-data4$COUNT.it", x, "*(data4$CARBON_t2-data4$CARBON_t1)*data4$ck_sq*3.67", sep=""))))
  eval(parse(text=(paste("sqtot<-sum(data4$sq_t", x,")", sep=""))))
  annual_sequestration<-c(annual_sequestration, sqtot)
}

cum_em<-cumsum(annual_emission)
cum_sq<-cumsum(annual_sequestration)
em<-as.data.frame(cbind(annual_emission, cum_em, annual_sequestration, cum_sq))
em$netem<-em$annual_emission-em$annual_sequestration
em$cum_netem<-cumsum(em$netem)

year<-as.character(QUESC_list [QUESC_list_n,1])
t0<-as.integer(substr(data, 21:24, 24))
yearsim<-c(t0:(t0+iteration))

em<-as.data.frame(cbind(yearsim,em))
em$yearsim<-factor(em$yearsim)
plot1<-ggplot(em,aes(yearsim,cum_em,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot2<-ggplot(em,aes(yearsim,annual_emission,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot3<-ggplot(em,aes(yearsim,annual_sequestration,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")
plot4<-ggplot(em,aes(yearsim,cum_netem,group=1))+ geom_line(colour="red")+geom_point(colour="red", size=4, shape=21, fill="white")

dirAnnual<-paste(dirname(proj.file), "/SCIENDO/annual", sep="")
dir.create(dirAnnual, mode="0777")
setwd(dirAnnual)
workingDirectory<-dirAnnual

#====WRITE CAR====
# t1=period1
# t2=period2
# period<-abs(t2-t1)
# data_merge<-read.dbf(carbonData)
# data_merge2<-read.dbf(paste(workingDirectory,"SCIENDO-LUWES_database.dbf", sep="/"))

pu <- melt(data = data4, id.vars=c('ZONE','Z_NAME'), measure.vars=c('COUNT'))
pu <- dcast(data = pu, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )
check_peat<-as.data.frame(as.character(ls(pattern="peat.index")))
if(nrow(check_peat)!=0){
  lut.pu<-lut.pu_peat
}
pu_zname <- lut.pu
colnames(pu_zname)[1] <- "ZONE"
colnames(pu_zname)[2] <- "Z_NAME"
pu<-pu[which(pu$Z_NAME != 0),]
pu <- merge(pu, pu_zname, by="Z_NAME")
pu<-na.omit(pu)
pu$Penunjukkan<-NULL
pu$percentage<-(pu$COUNT/sum(pu$COUNT))
pu$ZONE.y<-NULL
rownames(pu)<-NULL
colnames(pu)[2]="ZONE"
#colnames(pu)[3] <- "ZONE"
test<-as.character(pu$Z_NAME)

zone_merge <- subset(pu,select=c(Z_NAME, COUNT))
colnames(zone_merge)[2] <- "Z_AREA"
data4 <- merge(data4, zone_merge, by="Z_NAME", all=TRUE)
data4$LUTMZone <- data4$COUNT.it0 / data4$Z_AREA
data4<-na.omit(data4)

d1<-melt(data=data4, id.vars=c('ID_LC1','LC_t1'))
d1$variable<-d1$value<-NULL
d1<-unique(d1)
d1<-na.omit(d1)
d2<-melt(data=data4, id.vars=c('ID_LC2','LC_t2'))
d2$variable<-d2$value<-NULL
d2<-unique(d2)
d2<-na.omit(d2)
lu1.lost<-unique(data4$ID_LC2)[is.na(match(unique(data4$ID_LC2),unique(data4$ID_LC1)))]
lu2.lost<-unique(data4$ID_LC1)[is.na(match(unique(data4$ID_LC1),unique(data4$ID_LC2)))]
lu.lost<-c(as.integer(as.matrix(lu1.lost)),as.integer(as.matrix(lu2.lost)))
while(length(lu1.lost)!=0 || length(lu2.lost)!=0){
  if(length(lu1.lost)!=0){
    new.lu<-d2[d2$ID_LC2 %in% lu1.lost, 1:2]
    colnames(new.lu)[1]<-'ID_LC1'
    colnames(new.lu)[2]<-'LC_t1'
    d1<-rbind(d1,new.lu)
    lu1.lost<-unique(d2$ID_LC2)[is.na(match(unique(d2$ID_LC2),unique(d1$ID_LC1)))]
  } else if(length(lu2.lost)!=0){
    new.lu<-d1[d1$ID_LC1 %in% lu2.lost, 1:2]
    colnames(new.lu)[1]<-'ID_LC2'
    colnames(new.lu)[2]<-'LC_t2'
    d2<-rbind(d2,new.lu)
    lu2.lost<-unique(d1$ID_LC1)[is.na(match(unique(d1$ID_LC1),unique(d2$ID_LC2)))]
  }
}
colnames(d2)<-c("ID","CLASS")

name.matrix<-d2
name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$CLASS, minlength=4, method="both"))
name.matrix$order<-name.matrix$ID
name.matrix$order<-as.numeric(levels(name.matrix$order))[name.matrix$order]
name.matrix<- as.data.frame(name.matrix[order(name.matrix$order, decreasing=FALSE),])
name.matrix$order<-NULL

#Creating SCIENDO-Emission Baseline Database
col.select<-as.character(c('em_t0','sq_t0'))
for(i in 1:iteration){
  EM.slc<-paste('em_t',i,sep="")
  col.select<-c(col.select,EM.slc)
  SQ.slc<-paste('sq_t',i,sep="")
  col.select<-c(col.select,SQ.slc)
}
Baseline.db.1<-data4[,1:15]
Baseline.db.2<-data4[c(col.select)]
Baseline.db<-as.data.frame(cbind(Baseline.db.1,Baseline.db.2))
rm(Baseline.db.1)
rm(Baseline.db.2)

options(scipen=999)
Scenario_name<-gsub(" ","","Annual projection")

#CREATING ABACUS PROJECT FILE
Gnrl.info.1<-c("file_version", "title","description", "numberofzones","total_area","time", "include_bg","include_modif", "using_bg_factor","using_modif_factor", "model_iteration")
Gnrl.info.2<-c("1.1.0", "SCIENDO", "Project description",length(unique(pu$ZONE)),sum(data4$COUNT), 1, "false", "false", "true", "true", iteration)
Gnrl.info<-paste(Gnrl.info.1,Gnrl.info.2,sep="=")

#General Information
fileConn<-file(paste(workingDirectory,"/",Scenario_name,".txt",sep=""))
text0<-"#GENERAL"
write(text0, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Gnrl.info, paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")
text<-"\n#ZONE"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

#Zone information
zone<-pu[c('Z_NAME','percentage')]
log.val<-rep('true',length(pu$ZONE))
zone<-cbind(zone, log.val)
write.table(zone, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")

#Landuse Information
text<-"\n#LANDCOVER"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(name.matrix$CLASS, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")

#Eligibility
egb<-matrix('true',ncol=nrow(name.matrix), nrow=nrow(name.matrix))
egb<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),egb))
colnames(egb)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#ELIGIBILITY"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(egb, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#Cost Benefit Unit
text<-"\n#COSTBENEFIT_UNIT"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
text<-"Private\tNet return received by the land-use operator, farmers"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

#Carbon Stock
text<-"\n#CARBONSTOCK"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
#carbon<-matrix(ncol=nrow(name.matrix),nrow=nrow(name.matrix),0)
data_merge.melt <- melt(data = data4, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('CARBON_t1'))
data_merge.melt2 <- melt(data = data4, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('CARBON_t2'))
data_merge.melt <- na.omit(data_merge.melt)
data_merge.melt2 <- na.omit(data_merge.melt2)
carbon1 <- dcast(data = data_merge.melt, formula = LC_t1 ~ Z_NAME, fun.aggregate = mean)
carbon2 <- dcast(data = data_merge.melt2, formula = LC_t2 ~ Z_NAME, fun.aggregate = mean)
c1.lost<-unique(carbon2$LC_t2)[is.na(match(unique(carbon2$LC_t2),unique(carbon1$LC_t1)))]
c2.lost<-unique(carbon1$LC_t1)[is.na(match(unique(carbon1$LC_t1),unique(carbon2$LC_t2)))]
while(length(c1.lost)!=0 || length(c2.lost)!=0){
  if(length(c1.lost)!=0){
    new.lu<-carbon2[carbon2$LC_t2 %in% c1.lost, 1:ncol(carbon2)]
    colnames(new.lu)[1]<-'LC_t1'
    carbon1<-rbind(carbon1,new.lu)
    c1.lost<-unique(carbon2$LC_t2)[is.na(match(unique(carbon2$LC_t2),unique(carbon1$LC_t1)))]
  } else if(length(c2.lost)!=0){
    new.lu<-carbon1[carbon1$LC_t1 %in% c2.lost, 1:ncol(carbon1)]
    colnames(new.lu)[1]<-'LC_t2'
    carbon2<-rbind(carbon2,new.lu)
    c2.lost<-unique(carbon1$LC_t1)[is.na(match(unique(carbon1$LC_t1),unique(carbon2$LC_t2)))]
  }
}
#for(i in 1:nrow(name.matrix)){
#for(j in 1:nrow(name.matrix)){
#carbon[i,j]<-round(unique(data_merge$CARBON_t1[which(data_merge$ID_LC1==i & data_merge$ID_LC2==j)]),digits=2)
#}
#}
#carbon<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),carbon))
#carbon[,2:23][carbon[,2:23]==0]<-format(carbon[,2:23][carbon[,2:23]==0], nsmall=1, digits=2)
#colnames(carbon)<-(c('//LandCover', as.character(name.matrix$CLASS)))
cek_carbon <- sum(carbon1[2]) != 0
k=3
while(cek_carbon){
  if(cek_carbon){
    k <- carbon1[2]
    break
  } else {
    cek_carbon <- sum(carbon1[k]) != 0
    k=k+1
  }
} 
for(i in 2:ncol(carbon1)){
    carbon1[i] <- k
}
write.table(carbon1, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#NPV Private
NPV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
NPV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),NPV))
#NPV[,2:23]<-format(NPV[,2:23],nsmall=1,digits=2)
colnames(NPV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#NPV_Private"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(NPV, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#COST Benefit CONVERSION Private
for (i in 1:nrow(zone)){
  CBCV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  CBCV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),CBCV))
  #CBCV[,2:23]<-format(CBCV[,2:23], nsmall=1,digits=2)
  colnames(CBCV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#COSTBENEFIT_CONVERSION_Private\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(NPV, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#LANDCOVER CHANGE
write("", paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
LC_chg<-melt(data4, id.vars=c('ZONE','Z_NAME','ID_LC1','ID_LC2'), measure.vars=c('LUTMZone'))
LC_chg$order1<-LC_chg$ID_LC1
LC_chg$order1<-as.numeric(levels(LC_chg$order1))[LC_chg$order1]
LC_chg$order2<-LC_chg$ID_LC2
LC_chg$order2<-as.numeric(levels(LC_chg$order2))[LC_chg$order2]
LC_chg<-within(LC_chg, {value<-ifelse(is.na(value),0, value)})
for(i in 1:nrow(zone)){
  LC_chg_Z<-LC_chg[which(LC_chg$Z_NAME==zone$Z_NAME[i]),]
  if(sum(LC_chg_Z$value)==0){
    m<-NPV
  } else {
    LC_chg_Z_M<-dcast(LC_chg_Z, order1~order2, fun.aggregate=mean, value.var='value')
    colnames(LC_chg_Z_M)[1]<-'ID'
    LC_chg_Z_M<-merge(LC_chg_Z_M,name.matrix,by="ID", all=TRUE)
    LC_chg_Z_M$ID<-as.numeric(LC_chg_Z_M$ID)
    LC_chg_Z_M<- as.data.frame(LC_chg_Z_M[order(LC_chg_Z_M$ID, decreasing=FALSE),])
    LC_chg_Z_M$LC_CODE<-NULL
    row.names(LC_chg_Z_M)<-NULL
    LC_chg_Z_M_ID<-LC_chg_Z_M$ID
    LC_chg_Z_M$ID<-NULL
    #LC_chg_Z_M<-LC_chg_Z_M[,c(ncol(LC_chg_Z_M),1:(ncol(LC_chg_Z_M)-1))]
    a<-lu.lost[!(lu.lost %in% names(LC_chg_Z_M))]
    eval(parse(text=(paste("LC_chg_Z_M$'",a,"'<-0", sep=""))))
    LC_chg_Z_M[is.na(LC_chg_Z_M)]<-0
    m<-data.frame(LC_chg_Z_M$CLASS)
    for(j in 1:length(LC_chg_Z_M_ID)){
      eval(parse(text=(paste("m<-cbind(m,LC_chg_Z_M[","'",LC_chg_Z_M_ID[j],"'","])",sep=""))))
    }
  }
  #LC_chg_Z_M<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),LC_chg_Z_M))
  #LC_chg_Z_M[,2:23][LC_chg_Z_M[,2:23]==0]<-format(LC_chg_Z_M[,2:23][LC_chg_Z_M[,2:23]==0], nsmall=1, digits=2)
  colnames(m)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#LANDCOVER_CHANGE\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(m, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#BelowGround Emission
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  BGE<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  BGE<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGE))
  #BGE[,2:23]<-format(BGE[,2:23],nsmall=1,digits=2)
  colnames(BGE)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#BELOWGROUND_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(BGE, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#BelowGround Emission Factor
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  BGEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  BGEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGEF))
  #BGEF[,2:23]<-format(BGEF[,2:23],nsmall=1,digits=2)
  colnames(BGEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#BELOWGROUND_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(BGEF, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Modif Emission
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  ME<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  ME<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),ME))
  #ME[,2:23]<-format(ME[,2:23],nsmall=1,digits=2)
  colnames(ME)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#MODIF_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(ME, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Modif Emission Factor
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  MEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  MEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),MEF))
  #MEF[,2:23]<-format(MEF[,2:23],nsmall=1,digits=2)
  colnames(MEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#MODIF_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
  write.table(MEF, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Transition Probability Matrix 
write("", paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
TPM<-melt(data4, id.vars=c('ZONE','Z_NAME','ID_LC1','ID_LC2'), measure.vars=c('TPM1'))
TPM$order1<-TPM$ID_LC1
TPM$order1<-as.numeric(levels(TPM$order1))[TPM$order1]
TPM$order2<-TPM$ID_LC2
TPM$order2<-as.numeric(levels(TPM$order2))[TPM$order2]
for(j in 0:iteration) {
  for(i in 1:nrow(zone)){
    TPM_Z<-TPM[which(TPM$Z_NAME==zone$Z_NAME[i]),]
    if(sum(TPM_Z$value)==0){
      m<-as.data.frame(diag(1, nrow(name.matrix), nrow(name.matrix)))
      m<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),m))
    } else {
      TPM_Z_M<-dcast(TPM_Z, order1~order2, fun.aggregate=mean, value.var='value')
      colnames(TPM_Z_M)[1]<-'ID'
      TPM_Z_M<-merge(TPM_Z_M,name.matrix,by="ID", all=TRUE)
      TPM_Z_M$ID<-as.numeric(TPM_Z_M$ID)
      TPM_Z_M<- as.data.frame(TPM_Z_M[order(TPM_Z_M$ID, decreasing=FALSE),])
      TPM_Z_M$LC_CODE<-NULL
      row.names(TPM_Z_M)<-NULL
      TPM_Z_M_ID<-TPM_Z_M$ID
      TPM_Z_M$ID<-NULL
      #TPM_Z_M<-TPM_Z_M[,c(ncol(TPM_Z_M),1:(ncol(TPM_Z_M)-1))]
      a<-lu.lost[!(lu.lost %in% names(TPM_Z_M))]
      eval(parse(text=(paste("TPM_Z_M$'",a,"'<-0", sep=""))))
      TPM_Z_M[is.na(TPM_Z_M)]<-0
      m<-data.frame(TPM_Z_M$CLASS)
      for(k in 1:length(TPM_Z_M_ID)){
        eval(parse(text=(paste("m<-cbind(m,TPM_Z_M[","'",TPM_Z_M_ID[k],"'","])",sep=""))))
      }
    }
    
    #TPM_Z_M<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),TPM_Z_M))
    #TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0]<-format(TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0], nsmall=1, digits=2)
    colnames(m)<-(c('//LandCover', as.character(name.matrix$CLASS)))
    text<-paste("\n#TRANSITION_PROBABILITY_MATRIX\tITERATION=", j, "\tZONE=",zone$Z_NAME[i], sep="")
    write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
    write.table(m, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
  }
}
write("\n", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

Abacus_Project_File = paste(workingDirectory, "/",Scenario_name,".car",sep="") #work with car file and also supported text file with abacus project format
#Original_Project_File = paste(workingDirectory, "/","Original_data.car",sep="")
#file.copy(Abacus_Project_File,Original_Project_File)

if (file.exists("C:/Program Files (x86)/LUMENS/AbacusScenario")){
  abacusExecutable = "C:/Progra~2/LUMENS/AbacusScenario/abacus "
} else{
  abacusExecutable = "C:/Progra~1/LUMENS/AbacusScenario/abacus "
}
systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)


#====WRITE REPORT====
title<-"\\b\\fs32 LUMENS-SCIENDO - HISTORICAL BASELINE ANNUAL PROJECTION \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
rtffile <- RTF("LUMENS_SCIENDO-Annual_Projection_report.lpr", font.size=9)
addParagraph(rtffile, "\\b\\fs32 Hasil Analisis\\b0\\fs20")
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addTable(rtffile,em, font.size=8) 
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot1)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot2)
done(rtffile)

command<-paste("start ", "winword ", dirAnnual, "/LUMENS_SCIENDO-Annual_Projection_report.lpr", sep="" )
shell(command)



