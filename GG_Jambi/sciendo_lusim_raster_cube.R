##SCIENDO-PostgreSQL=group
##proj.file=string
##SCIENDO_LUCM_index=string
##factor_folder=string
##statusoutput=output table

# INPUT
SCIENDO_LUCM_index="BAU_LUCM" # to name the SCIENDO sub-directory
projDir <- "D:/GG_Jambi/process/lumens_dir"
factor_folder <- "D:/GG_Jambi/General/data/spatial/raster/lucm_predictors/cube_final2/"#ADinput

library(spatial.tools)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load active project
# load(proj.file) ADomit

# set driver connection
# driver <- dbDriver('PostgreSQL')
# project <- as.character(proj_descr[1,2])
# DB <- dbConnect(
#   driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
#   user=as.character(pgconf$user), password=as.character(pgconf$pass)
# ) # ADomit

#=Set working directory
SCIENDO_folder<-SCIENDO_LUCM_index
# AD modify: Creation of dirs----
result_dir<-paste(projDir,"/SCIENDO", sep="")
if(!dir.exists(result_dir))dir.create(result_dir)
result_dir<-paste(result_dir, "/", SCIENDO_folder, sep="")
if(!dir.exists(result_dir))dir.create(result_dir)
# creat..----
setwd(result_dir)
factor_dir <- (paste(result_dir,"/factor2_final", sep="")) #ADinput
dir.create(factor_dir)
urlAddressRaster <- factor_folder

# preparing factors
listFactors <- list.files(urlAddressRaster, full.names=TRUE, pattern=".tif$")
editorFactors<-data.frame(file=listFactors, select=1)

selectedFactors<-subset(editorFactors, select==1)
factors<-as.character(selectedFactors$file)
nFactors <- length(factors)

aliasFactor<-NULL
for (a in 1:nFactors) {
  paste(eval(parse(text=(paste("temp<-substr(basename(factors[", a, "]), 1, nchar(basename(factors[", a, "])) - 4)", sep="")))))
  aliasFactor<-c(aliasFactor,temp)
}

DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
}

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap1680"))
con$addTag("inputport", attrs=c(name="map", peerid=paste("v", nFactors+1,sep="")))
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/sciendo_factor.ers"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
con$addTag("inputport", attrs=c(name="step"), ".none")
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")
# end.

# begin.
# add functor = LoadMap
for (b in 1:nFactors){
  con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[b]))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', factors[b], '"', sep=""))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), ".none")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id=paste("v",b,sep="")))
  con$closeTag("functor") 
}
# end.

# begin.
# add containerfunctor = CreateCubeMap
con$addTag("containerfunctor", attrs=c(name="CreateCubeMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="createCubeMap1678"))
con$addTag("inputport", attrs=c(name="cellType"), ".float32")
con$addTag("inputport", attrs=c(name="nullValue"), "-9999")
con$addTag("outputport", attrs=c(name="map", id=paste("v", nFactors+1, sep="")))
# add subtag functor for CreateCubeMap
for (c in 1:nFactors) {
  con$addTag("functor", attrs=c(name="NumberAndNameMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[c]))
  con$addTag("inputport", attrs=c(name="map", peerid=paste("v", c, sep="")))
  con$addTag("inputport", attrs=c(name="mapName"), paste('"', aliasFactor[c], '"', sep=""))
  con$addTag("inputport", attrs=c(name="mapNumber"), 0)
  con$closeTag("functor")
}
con$closeTag("containerfunctor")
# end.

# print(con$value())
# write egoml
saveXML(con$value(), file=paste(result_dir, "/2_Create_Raster_Cube.egoml", sep=''))
command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', result_dir, '/2_Create_Raster_Cube.egoml"', sep="")
system(command)
save(list = c("aliasFactor", "SCIENDO_LUCM_index", "SCIENDO_folder", "result_dir"), file = paste0(result_dir, "/passFrom1.RData"))# to be passed on to the next step
       # 1. aliasFactor: vector of the names of the component of the raster cube
       # 2. SCIENDO_LUCM_index
       # 3. SCIENDO_folder
       # 4. result_dir

# dbDisconnect(DB)
# 
# statuscode<-1
# statusmessage<-"SCIENDO has completed successfully"
# statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

