#START#DATA#PREPARATION#################################################################################################

########################################################################################################################
# LIBRARY                                                                                                              #
########################################################################################################################
library(raster)
library(rgdal)
library(spatial.tools)
library(tcltk)
library(r2excel)
library(splitstackshape)
library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(tiff)
library(RColorBrewer)
library(rasterVis)
library(reshape2)
library(foreign)
library(lattice)
library(latticeExtra)
library(jsonlite)

########################################################################################################################
# INITIALIZATION                                                                                                       #
########################################################################################################################
# set working directory, create new if it doesn't exist 
workingDirectory='D:/GG_Papua/process/lumens_dir/IDH/Result'
dir.create(workingDirectory, mode='0777')
setwd(workingDirectory)

# copy selected folder from server to current local working directory, 
# overwrite folder if it already exists
#errorCopying<-file.copy('//172.18.23.45/Project_data/IDH_Green_Growth/Database/GGPData', 
#                        'C:/IDH', 
#                       recursive=TRUE, 
#                        overwrite=TRUE)
#if(errorCopying){
#  msgBox <- tkmessageBox(title='LUMENS',
#                         message='No such file or directory',
#                         icon='warning',
#                         type='ok')
#  quit()
#}

# set target paths
urlAddressRaster='D:/GG_Papua/process/lumens_dir/IDH/raster/' # ./30m or ./100m
urlAddressTabular='D:/GG_Papua/process/lumens_dir/IDH/table/'
urlAddressVector='D:/GG_Papua/process/lumens_dir/IDH/Vector/'

analyzedLocation='Papua'
iteration=5

#Handler to change temporal resolution
iterationAnnual=6


########################################################################################################################
# FUNCTION                                                                                                             #
########################################################################################################################
# load raster as reference
loadRasterAsReference <- function(url, file){
  raster(paste(url, file, sep=''))  
}
getSpatialResolution <- function(reference){
  if (grepl("+units=m", as.character(reference@crs))){
    print("Raster maps have projection in meter unit")
    spatres<-res(reference)[1]*res(reference)[2]/10000
    print(paste("Raster maps have", spatres, "Ha spatial resolution"))
    return(spatres)
  } else if (grepl("+proj=longlat", as.character(reference@crs))){
    print("Raster maps have projection in degree unit")
    spatres<-res(reference)[1]*res(reference)[2]*(111319.9^2)/10000
    print(paste("Raster maps have", spatres, "Ha spatial resolution"))
    return(spatres)
  } else{
    msgBox <- tkmessageBox(title = "LUMENS",
      message = "Raster map projection is unknown",
      icon = "info",
      type = "ok"
    )
    quit()
  }
}
# load raster 
loadRaster <- function(url, file){
  raster(paste(url, file, sep=''))
}
# loadRaster <- function(url, file, reference){
#   temp<-raster(paste(url, file, sep=''))
#   temp<-spatial_sync_raster(temp, reference, method='ngb')*1
#   return(temp)
# }
# load vector 
loadVector <- function(url, file){
  readOGR(dsn=url, layer=file)
}
# read excel file
loadLookupTable <- function(url, file) {
  #read.table(paste(url, file, sep=''), header=T, sep=',')
  xlsx.readFile(paste(url, file, sep=''), 1)  
}
# raster attribute table using freq
generateRasterAttributeTable <- function(rasterLayer, rasterCount=1){
  temp<-na.omit(as.data.frame(freq(rasterLayer)))
  temp$count<-temp$count*rasterCount
  return(temp)
}

########################################################################################################################
# LOOKUP TABLE                                                                                                         #
########################################################################################################################
# General
lookupTableCarbonPeatClassified<-loadLookupTable(urlAddressTabular, 'landuse.xlsx') #'landuse.xlsx'
#lookupTableEcoregion<-loadLookupTable(urlAddressTabular, 'ecoregion.xlsx')
lookupTablePlanningUnit<-loadLookupTable(urlAddressTabular, 'pu_final.xlsx') #'pu.xlsx'
# QUES-B
#lookupContrast<-loadLookupTable(urlAddressTabular, 'contrast.xlsx')
#lookupHabitat<-loadLookupTable(urlAddressTabular, 'habitat.xlsx')
# TA - profitability
lookupProfit<-loadLookupTable(urlAddressTabular, 'profit.xlsx') #'profit.xlsx'
# TA - economy regional
#lookupSector<-loadLookupTable(urlAddresTabular, 'sector.xlsx')
#lookupIntDemand<-loadLookupTable(urlAddresTabular, 'intermediate.xlsx')
#lookupFinDemand<-loadLookupTable(urlAddresTabular, 'finaldemand.xlsx')
#lookupAddValue<-loadLookupTable(urlAddresTabular, 'addvalue.xlsx')
#lookupLabour<-loadLookupTable(urlAddresTabular, 'labour.xlsx')
#lookupLandDistribution<-loadLookupTable(urlAddresTabular, 'landdistribution.xlsx')
#lookupLandRequirement<-loadLookupTable(urlAddresTabular, 'landrequirement.xlsx')
# Scenario
#lookupLandDemand<-loadLookupTable(urlAddressTabular, 'landdemand.xlsx')

columnID_CL<-c(1, 2, 3, 4, 5, 6, 7, 8)
columnClassified<-c("Hutan primer",
                    "Hutan sekunder",
                    "Tanaman pohon monokultur",
                    "Tanaman pohon campuran",
                    "Tanaman pertanian semusim",
                    "Semak, rumput dan lahan terbuka",
                    "Pemukiman",
                    "Lain-lain"
                    )
lookupTableClassified<-data.frame(columnID_CL, columnClassified)

trajectoryChangeCode<-c(11:17, 22:27, 32:37, 42:44, 46:47, 52:57, 62:67, 77, 88)
trajectoryName<-c("Stable natural forest", "Loss to logged-over forest", "Recovery to tree cropping",
                  "Recovery to agroforest", "Loss to cropland", "Loss to bare land and abandoned",
                  "Loss to infrastructure", "Loss to logged-over forest", "Recovery to tree cropping",
                  "Recovery to agroforest", "Loss to cropland", "Loss to bare land and abandoned",
                  "Loss to infrastructure", "Recovery to forest", "Recovery to tree cropping", 
                  "Recovery to agroforest", "Loss to cropland", "Loss to bare land and abandoned",
                  "Loss to infrastructure", "Recovery to forest", "Recovery to tree cropping",
                  "Recovery to agroforest", "Loss to bare land and abandoned", "Loss to infrastructure",
                  "Recovery to forest", "Recovery to tree cropping", "Recovery to agroforest",
                  "Loss to cropland", "Loss to bare land and abandoned", "Loss to infrastructure",
                  "Recovery to forest", "Recovery to tree cropping", "Recovery to agroforest",
                  "Loss to cropland", "Loss to bare land and abandoned", "Loss to infrastructure",
                  "Loss to infrastructure","Other"
                  )
trajectoryStatus<-c("Stable forest", "Forest degradation", "Deforestation", "Deforestation",
                     "Deforestation", "Deforestation", "Deforestation", "Stable forest", 
                     "Deforestation", "Deforestation", "Deforestation", "Deforestation",
                     "Deforestation", "Reforestation", "Other", "Other", "Other", "Other",
                     "Other", "Other", "Other", "Other","Other","Other","Other","Other",
                     "Other","Other","Other","Other", "Other","Other","Other","Other",
                     "Other","Other","Other", "Others"
                     )
lookupTableTrajectoriesChanges<-data.frame(trajectoryChangeCode, trajectoryName, trajectoryStatus)

trajectoryID<-c(1:9)
trajectoryName<-c("Loss to cropland", "Loss to infrastructure", "Loss to logged-over forest",
                  "Loss to bare land and abandoned", "Stable natural forest", "Recovery to agroforest",
                  "Recovery to tree cropping", "Recovery to forest", "Other"
                  )
lookupTableTrajectories<-data.frame(trajectoryID, trajectoryName)
lookupTableTrajectoriesChanges<-merge(lookupTableTrajectoriesChanges, lookupTableTrajectories, by='trajectoryName')

nLandCoverId<-nrow(lookupTableCarbonPeatClassified)

########################################################################################################################
# REFERENCE MAP                                                                                                        #
########################################################################################################################
# zAdministrativeUnit<-loadRasterAsReference(urlAddressRaster, 'pu.tif')
# spatialResolution<-getSpatialResolution(zAdministrativeUnit)
 
# freqZAdministrativeUnit<-generateRasterAttributeTable(zAdministrativeUnit, spatialResolution)


########################################################################################################################
# PLANNING UNIT / ZONES                                                                                                #
########################################################################################################################
# zEcoregion<-loadRaster(urlAddressRaster, 'ecoregion.tif', zAdministrativeUnit)
# freqZEcoregion<-generateRasterAttributeTable(zEcoregion, spatialResolution)

zPlanningUnit<-loadRaster(urlAddressRaster, 'pu_fix.tif')
spatialResolution<-getSpatialResolution(zPlanningUnit)

freqZPlanningUnit<-generateRasterAttributeTable(zPlanningUnit, spatialResolution)
nPlanningUnitId<-nrow(lookupTablePlanningUnit)


########################################################################################################################
# FACTOR / DISTANCE / EXPLANATORY VARIABLES                                                                            #
########################################################################################################################
factors <- list.files(urlAddressRaster, full.names=TRUE, pattern="factor")
nFactors <- length(factors)


########################################################################################################################
# LAND USE/COVER                                                                                                       #
########################################################################################################################
landCoverT1<-loadRaster(urlAddressRaster, 'lc2010_fix.tif')
landCoverT2<-loadRaster(urlAddressRaster, 'lc2018_fix.tif')
#landCoverT3<-loadRaster(urlAddressRaster, 'lc2005_sumsel_v2.tif')
#landCoverT4<-loadRaster(urlAddressRaster, 'lc2010_sumsel_v2.tif')
#landCoverT5<-loadRaster(urlAddressRaster, 'lc2014_sumsel_v2.tif')

T1<-2010
T2<-2018
# #T3<-2005
# #T4<-2010
# T5<-2014
# T6<-2020
# T7<-2025
# T8<-2030
# T9<-2034
allPeriod<-c(T1, T2)
nPeriod<-length(allPeriod)
transitionPeriod<-c(paste(T1, '-', T2, sep='')
                    )

freqLandCoverT1<-generateRasterAttributeTable(landCoverT1, spatialResolution)
freqLandCoverT2<-generateRasterAttributeTable(landCoverT2, spatialResolution)
# freqLandCoverT3<-generateRasterAttributeTable(landCoverT3, spatialResolution)
# freqLandCoverT4<-generateRasterAttributeTable(landCoverT4, spatialResolution)
# freqLandCoverT5<-generateRasterAttributeTable(landCoverT5, spatialResolution)

# number of land use/cover
nLandCover<-nrow(as.data.frame(as.character(ls(pattern='landCover'))))


########################################################################################################################
# MAIN ATTRIBUTES                                                                                                      #
# ID_LC_T1 = Land use/cover ID in T1                                                                                   #
# ID_LC_T2 = Land use/cover ID in T2                                                                                   #
# ID_Z     = Planning unit/zone ID                                                                                     #
# PERIOD   = Period                                                                                                    #
# COUNT    = Area of changes                                                                                           #
# LC_T1    = Land use/cover description in T1                                                                          #
# LC_T2    = Land use/cover description in T2                                                                          #
# ADMIN    = Administrative unit ID                                                                                    #
# PEAT     = Peat                                                                                                      #
# PLAN     = Planning Unit ID                                                                                          #
# C_T1     = Carbon stock in T1                                                                                        #
# C_T2     = Carbon stock in T2                                                                                        #
# P_T1     = Factor emission of peat in T1                                                                             #
# P_T2     = Factor emission of peat in T2                                                                             #
# PROF_T1  = Land use profitability in T1                                                                              #
# PROF_T2  = Land use profitability in T2                                                                              #
#                                                                                                                      #
# SUPPORTING ATTRIBUTES                                                                                                #
# ID_ECO  = Ecoregion ID                                                                                               #
# ID_CL                                                                                                                #
#                                                                                                                      #
#                                                                                                                      #
########################################################################################################################
# rename lookup table columns
colnames(lookupTableCarbonPeatClassified)=c('ID_LC', 'LC', 'C', 'P', 'ID_CL')
lookupTableLandUseCover<-subset(lookupTableCarbonPeatClassified, select=c(ID_LC, LC))
colnames(lookupTablePlanningUnit)=c('ID_Z', 'ADMIN', 'PLAN', 'PEAT')
colnames(lookupTableClassified)=c('ID_CL', 'CLASSIFIED')
colnames(lookupProfit)=c('ID_LC', 'LC', 'PROFIT')
# colnames(lookupTableEcoregion)=c('ID_ECO', 'ECO')

# rename planning unit attribute table
colnames(freqZPlanningUnit)=c('ID_Z', 'COUNT_Z')
freqZPlanningUnit<-merge(freqZPlanningUnit, lookupTablePlanningUnit)

# (1) rename columns of land use/cover attribute table
colnames(freqLandCoverT1)=c('ID_LC', 'COUNT')
colnames(freqLandCoverT2)=c('ID_LC', 'COUNT')
# colnames(freqLandCoverT3)=c('ID_LC', 'COUNT')
# colnames(freqLandCoverT4)=c('ID_LC', 'COUNT')
# colnames(freqLandCoverT5)=c('ID_LC', 'COUNT')
# (2) check existing land use/cover with lookup table
freqLandCoverT1<-merge(freqLandCoverT1, lookupTableLandUseCover, by='ID_LC', all=TRUE)
freqLandCoverT1<-within(freqLandCoverT1, { COUNT<-ifelse(is.na(COUNT), 0, COUNT) })
freqLandCoverT2<-merge(freqLandCoverT2, lookupTableLandUseCover, by='ID_LC', all=TRUE)
freqLandCoverT2<-within(freqLandCoverT2, { COUNT<-ifelse(is.na(COUNT), 0, COUNT) })
# freqLandCoverT3<-merge(freqLandCoverT3, lookupTableLandUseCover, by='ID_LC', all=TRUE)
# freqLandCoverT3<-within(freqLandCoverT3, { COUNT<-ifelse(is.na(COUNT), 0, COUNT) })
# freqLandCoverT4<-merge(freqLandCoverT4, lookupTableLandUseCover, by='ID_LC', all=TRUE)
# freqLandCoverT4<-within(freqLandCoverT4, { COUNT<-ifelse(is.na(COUNT), 0, COUNT) })
# freqLandCoverT5<-merge(freqLandCoverT5, lookupTableLandUseCover, by='ID_LC', all=TRUE)
# freqLandCoverT5<-within(freqLandCoverT5, { COUNT<-ifelse(is.na(COUNT), 0, COUNT) })
# (3) summary
summaryOfLandUseChange<-subset(freqLandCoverT1, select=c(ID_LC, LC, COUNT))
colnames(summaryOfLandUseChange)[2]='Land_use_type'
colnames(summaryOfLandUseChange)[3]=paste(T1, '_ha', sep='')
for(i in 2:nLandCover){
  eval(parse(text=(paste("summaryOfLandUseChange<-merge(summaryOfLandUseChange, freqLandCoverT", i, ", by='ID_LC')", sep=""))))
  eval(parse(text=(paste("colnames(summaryOfLandUseChange)[", i+2, "]<-paste(T", i, ", '_ha', sep='')", sep=""))))
  summaryOfLandUseChange$LC<-NULL
}
# (4) rename
colnames(freqLandCoverT1)=c('ID_LC_T1', 'COUNT_T1', 'LC_T1')
colnames(freqLandCoverT2)=c('ID_LC_T2', 'COUNT_T2', 'LC_T2')
# colnames(freqLandCoverT3)=c('ID_LC_T3', 'COUNT_T3', 'LC_T3')
# colnames(freqLandCoverT4)=c('ID_LC_T4', 'COUNT_T4', 'LC_T4')
# colnames(freqLandCoverT5)=c('ID_LC_T5', 'COUNT_T5', 'LC_T5')

# create land use change map with planning unit and last three years' maps
landUseChangeMap1 <- zPlanningUnit*1 + landCoverT1*10^3 + landCoverT2*10^6 
# landUseChangeMap2 <- zPlanningUnit*1 + landCoverT2*10^3 + landCoverT3*10^6 
# landUseChangeMap3 <- zPlanningUnit*1 + landCoverT3*10^3 + landCoverT4*10^6 
# landUseChangeMap4 <- zPlanningUnit*1 + landCoverT4*10^3 + landCoverT5*10^6 

freqLandUseChangeMap1 <- generateRasterAttributeTable(landUseChangeMap1, spatialResolution)
# freqLandUseChangeMap2 <- generateRasterAttributeTable(landUseChangeMap2, spatialResolution)
# freqLandUseChangeMap3 <- generateRasterAttributeTable(landUseChangeMap3, spatialResolution)
# freqLandUseChangeMap4 <- generateRasterAttributeTable(landUseChangeMap4, spatialResolution)

# first error checking COUNT == 8,689,903 
#                         96,506,769 * 900/10000 = 8,685,609.21
i=1
#for(i in 1:1){
  eval(parse(text=(paste('totalArea<-sum(freqLandUseChangeMap', i, '$count)', sep=''))))
  print(totalArea)
  # if(spatialResolution==1){
  #   if(totalArea != 8684908){
  #     msgBox <- tkmessageBox(title='LUMENS',
  #                            message='Inequivalent area' ,
  #                            icon='warning',
  #                            type='ok')
  #     quit()
  #   }
  # } else {
  #   if(totalArea != 8684908){
  #     msgBox <- tkmessageBox(title='LUMENS',
  #                            message='Inequivalent area',
  #                            icon='warning',
  #                            type='ok')
  #     quit()
  #   }
  # }
#}

# LOCK THE COMBINATION OF PLANNING UNIT (187) * LC_T1 (31) * LC_T2 (31)
# TOTAL ROW = 179707
# (1) succeed but took a long time and really memory consuming
# dummy <- data.frame(freqZPlanningUnit[1,1], 0, 
#                     freqLandCover1990[1,1], 
#                     freqLandCover2000[1,1]
#                     )
# colnames(dummy)=c('ID_Z', 'COUNT', 'ID_LC_T1', 'ID_LC_T2')
# foreach(i=2:nrow(freqZPlanningUnit)) %do%  {
#   foreach(j=2:nrow(freqLandCover1990)) %do% {
#     foreach(k=2:nrow(freqLandCover2000)) %do% {
#         dummyData <- c(freqZPlanningUnit[i,1], 0,
#                        freqLandCover1990[j,1], 
#                        freqLandCover2000[k,1])
#         dummy<-rbind(dummy, dummyData)
#     }
#   }
# }
# (2) hard code part 1
dummy1<-data.frame(nPU=lookupTablePlanningUnit$ID_Z, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookupTableLandUseCover$ID_LC, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))

dummy3<-data.frame(nT2=rep(rep(lookupTableLandUseCover$ID_LC, nLandCoverId), nPlanningUnitId))

landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeMapDummy)<-c('ID_Z', 'ID_LC_T1', 'ID_LC_T2')
# (3) hard code part 2
# landUseChangeMapDummy <- loadLookupTable(urlAddressTabular, 'luc_dummy.xlsx')
i=1
# for(i in 1:1){
  nOfMaps<-3
  k<-0
  eval(parse(text=(paste('freqLandUseChangeMap', i, '$value_temp<-freqLandUseChangeMap', i, '$value', sep=''))))
   while(k < nOfMaps) {
    l<-nOfMaps-k
    eval(parse(text=(paste("freqLandUseChangeMap", i, "$Var", l, "<-freqLandUseChangeMap", i, "$value_temp %% 1000", sep=""))))  
    eval(parse(text=(paste('freqLandUseChangeMap', i, '$value_temp<-floor(freqLandUseChangeMap', i, '$value_temp/1000)', sep=''))))
     k<-k+1
   }
  eval(parse(text=(paste('freqLandUseChangeMap', i, '$value_temp<-NULL', sep=''))))
  eval(parse(text=(paste('freqLandUseChangeMap', i, '$value<-NULL', sep=''))))
  eval(parse(text=(paste("colnames(freqLandUseChangeMap", i, ")=c('COUNT', 'ID_Z', 'ID_LC_T1', 'ID_LC_T2')", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(landUseChangeMapDummy, freqLandUseChangeMap", i, ", by=c('ID_Z', 'ID_LC_T1', 'ID_LC_T2'), all=TRUE)", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-replace(freqLandUseChangeMap", i, ", is.na(freqLandUseChangeMap", i, "), 0)", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "$PERIOD<-'", allPeriod[i], "-", allPeriod[i+1], "'", sep=''))))
# }

# do MERGE
# lumensMainDatabase <- ID_LC_T1, ID_LC_T2, ID_Z, PERIOD, COUNT, LC_T1, LC_T2, ADMIN, PEAT, PLAN, C_T1, C_T2, P_T1, P_T2, PROF_T1, PROF_T2
lookupTableCarbonPeatClassified_T1<-subset(lookupTableCarbonPeatClassified, select=-ID_CL)
lookupTableCarbonPeatClassified_T2<-subset(lookupTableCarbonPeatClassified, select=-ID_CL)

colnames(lookupTableCarbonPeatClassified_T1)=c('ID_LC_T1', 'LC_T1', 'C_T1', 'P_T1')
colnames(lookupTableCarbonPeatClassified_T2)=c('ID_LC_T2', 'LC_T2', 'C_T2', 'P_T2')

lookupTableProfit_T1<-subset(lookupProfit, select=c(ID_LC, PROFIT))
lookupTableProfit_T2<-subset(lookupProfit, select=c(ID_LC, PROFIT))

colnames(lookupTableProfit_T1)=c('ID_LC_T1', 'PROF_T1')
colnames(lookupTableProfit_T2)=c('ID_LC_T2', 'PROF_T2')

for(i in 1:1){
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(freqLandUseChangeMap", i, ", lookupTablePlanningUnit, by='ID_Z')", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(freqLandUseChangeMap", i, ", lookupTableCarbonPeatClassified_T1, by='ID_LC_T1')", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(freqLandUseChangeMap", i, ", lookupTableCarbonPeatClassified_T2, by='ID_LC_T2')", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(freqLandUseChangeMap", i, ", lookupTableProfit_T1, by='ID_LC_T1')", sep=''))))
  eval(parse(text=(paste("freqLandUseChangeMap", i, "<-merge(freqLandUseChangeMap", i, ", lookupTableProfit_T2, by='ID_LC_T2')", sep=''))))
}

# second error checking COUNT == 8,683,752 
#                         96,506,769 * 900/10000 = 8,685,609.21
#for(i in 1:1){
  eval(parse(text=(paste('totalArea<-sum(freqLandUseChangeMap', i, '$COUNT)', sep=''))))
  print(totalArea)
  # if(spatialResolution==1){
  #   if(totalArea != 8684908){
  #     msgBox <- tkmessageBox(title='LUMENS',
  #                            message='Inequivalent area' ,
  #                            icon='warning',
  #                            type='ok')
  #     quit()
  #   }
  # } else {
  #   if(totalArea != 8684908){
  #     msgBox <- tkmessageBox(title='LUMENS',
  #                            message='Inequivalent area',
  #                            icon='warning',
  #                            type='ok')
  #     quit()
  #   }
  # }
#}

# bind all
lumensMainDatabase<-rbind(freqLandUseChangeMap1)

lumensMainDatabase$ID_LC_T1<-as.numeric(as.character(lumensMainDatabase$ID_LC_T1))
lumensMainDatabase$ID_LC_T2<-as.numeric(as.character(lumensMainDatabase$ID_LC_T2))
lumensMainDatabase$ID_Z<-as.numeric(as.character(lumensMainDatabase$ID_Z))
lumensMainDatabase$COUNT<-as.numeric(as.character(lumensMainDatabase$COUNT))
lumensMainDatabase$PROF_T1<-as.numeric(as.character(lumensMainDatabase$PROF_T1))
lumensMainDatabase$PROF_T2<-as.numeric(as.character(lumensMainDatabase$PROF_T2))
lumensMainDatabase$C_T1<-as.numeric(as.character(lumensMainDatabase$C_T1))
lumensMainDatabase$C_T2<-as.numeric(as.character(lumensMainDatabase$C_T2))
lumensMainDatabase$P_T1<-as.numeric(as.character(lumensMainDatabase$P_T1))
lumensMainDatabase$P_T2<-as.numeric(as.character(lumensMainDatabase$P_T2))


# save .xlsx .RData and .RHistory to workingDirectory
write.table(lumensMainDatabase, 'lumensMainDatabase.csv', row.names=F, col.names=T, sep=',')
# out of memory
# xlsx.writeFile(lumensMainDatabase, file="lumensMainDatabase.xlsx", sheetName="lumensMainDatabase") 
save(lumensMainDatabase, file='lumensMainDatabase')
lastActiveUser<-paste(Sys.time(), Sys.getenv('USERNAME'), sep=' ')
save(list=ls(all.names=TRUE), file="dataCompilation", envir = .GlobalEnv)
savehistory(file='lumensHistory')

###############################################################################################END#DATA#PREPARATION#####


########################################################################################################################
# HISTORICAL BASELINE ANNUAL PROJECTION                                                                                #
########################################################################################################################

# Create annual database
# Select period 2005 - 2010
lumensMainAnnualDatabase0510<-lumensMainDatabase[which(lumensMainDatabase$PERIOD==transitionPeriod[1]),]

nYear=5/(T2-T1) # nYear=8/(T2-T1)
lumensUnchangedArea<-subset(lumensMainAnnualDatabase0510, ID_LC_T1 == ID_LC_T2)
lumensChangedArea<-subset(lumensMainAnnualDatabase0510, ID_LC_T1 != ID_LC_T2)

# changed area per year
lumensChangedArea$COUNTx<-lumensChangedArea$COUNT/nYear # count per year
lumensChangedArea$COUNTy<-lumensChangedArea$COUNT-lumensChangedArea$COUNTx # count for n-period minus count per year
lumensChangedArea$COUNT<-lumensChangedArea$COUNTx # original count on changed area per year
lumensChangedArea$COUNTx<-NULL

lumensChangedAreaMelt<-melt(data = lumensChangedArea, id.vars=c('LC_T1', 'ID_Z'), measure.vars=c('COUNTy'))
# for DINAMICA
lumensChangedAreaCast<-dcast(data = lumensChangedAreaMelt, formula = LC_T1 + ID_Z ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
lumensChangedArea$COUNTy<-NULL

# unchanged area per year
lumensUnchangedArea<-merge(lumensUnchangedArea, lumensChangedAreaCast, by=c('LC_T1', 'ID_Z'))
lumensUnchangedArea$COUNT<-lumensUnchangedArea$COUNT+lumensUnchangedArea$. # original count on unchanged area per year 
lumensUnchangedArea$.<-NULL
# main annual database with each area in base year, 2005
lumensMainAnnualDatabase0506<-rbind(lumensUnchangedArea, lumensChangedArea)

# Calculate Transition Probability Matrix
# calculate land use/cover area t1 for each planning unit
lumensMainDatabaseMeltT1 <- melt(data = lumensMainAnnualDatabase0506, id.vars=c('ID_LC_T1','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT1 <- dcast(data = lumensMainDatabaseMeltT1, formula = ID_LC_T1 + ID_Z ~ ., fun.aggregate = sum) 
colnames(countLandCoverByZoneT1)[3]<-'COUNT_LU_ZONE_T1'
# calculate land use/cover area t2 for each planning unit
lumensMainDatabaseMeltT2 <- melt(data = lumensMainAnnualDatabase0506, id.vars=c('ID_LC_T2','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT2 <- dcast(data = lumensMainDatabaseMeltT2, formula = ID_LC_T2 + ID_Z ~ ., fun.aggregate = sum)
colnames(countLandCoverByZoneT2)[3]<-'COUNT_LU_ZONE_T2'

lumensMainAnnualDatabase0506<-merge(lumensMainAnnualDatabase0506, countLandCoverByZoneT1, by=c('ID_LC_T1','ID_Z'))
lumensMainAnnualDatabase0506<-merge(lumensMainAnnualDatabase0506, countLandCoverByZoneT2, by.x=c('ID_LC_T1','ID_Z'), by.y=c('ID_LC_T2','ID_Z'))
# TPM 2005-2010
lumensMainAnnualDatabase0506$TPM1<-lumensMainAnnualDatabase0506$COUNT/lumensMainAnnualDatabase0506$COUNT_LU_ZONE_T1 
lumensMainAnnualDatabase0506<-replace(lumensMainAnnualDatabase0506, is.na(lumensMainAnnualDatabase0506), 0)

# Handling TPM
# Check if TPM has zero value
lumensMainDatabaseMeltTPM_CheckIfZero<-melt(data = lumensMainAnnualDatabase0506, id.vars=c('ID_LC_T1','ID_Z'), measure.vars=c('TPM1'))
lumensMainDatabaseMeltTPM_CheckIfZero<-dcast(data = lumensMainDatabaseMeltTPM_CheckIfZero, formula = ID_LC_T1 + ID_Z ~ ., fun.aggregate = sum)
# Add column check as a status
#   CEK=0 -> Fix; ELSE -> Ignore 
colnames(lumensMainDatabaseMeltTPM_CheckIfZero)[3]<-'CEK'
lumensMainDatabaseMeltTPM_CheckIfZero<-within(lumensMainDatabaseMeltTPM_CheckIfZero, { ACT<-ifelse(CEK==0, 'Fix', 'Ignore') })
lumensMainDatabaseMeltTPM_CheckIfZero$CEK<-NULL

lumensMainDatabaseMeltTPMFixed<-merge(lumensMainAnnualDatabase0506, lumensMainDatabaseMeltTPM_CheckIfZero, by=c('ID_LC_T1', 'ID_Z'))
# fixed TPM1
lumensMainDatabaseMeltTPMFixed_Fix<-subset(lumensMainDatabaseMeltTPMFixed, ACT=='Fix')
lumensMainDatabaseMeltTPMFixed_Ignore<-subset(lumensMainDatabaseMeltTPMFixed, ACT=='Ignore')
if(nrow(lumensMainDatabaseMeltTPMFixed_Fix) > 0){
  lumensMainDatabaseMeltTPMFixed_Fix<-within(lumensMainDatabaseMeltTPMFixed_Fix, { TPM1<-ifelse(ID_LC_T1==ID_LC_T2, 1, TPM1) })
}
lumensMainDatabaseMeltTPMFixed<-rbind(lumensMainDatabaseMeltTPMFixed_Fix,lumensMainDatabaseMeltTPMFixed_Ignore)
lumensMainDatabaseMeltTPMFixed$ACT<-NULL

# Create unique names for each change
lumensMainDatabaseMeltTPMFixed$LU_CHG <- do.call(paste, c(lumensMainDatabaseMeltTPMFixed[c("LC_T1", "LC_T2")], sep = " to "))
lumensMainDatabaseMeltTPMFixed$key<- do.call(paste, c(lumensMainDatabaseMeltTPMFixed[c("LU_CHG", "ID_Z")], sep = " in "))
lumensMainDatabaseMeltTPMFixed<-subset(lumensMainDatabaseMeltTPMFixed, select=c(key, TPM1))

lumensMainAnnualDatabase0510$LU_CHG <- do.call(paste, c(lumensMainAnnualDatabase0510[c("LC_T1", "LC_T2")], sep = " to "))
lumensMainAnnualDatabase0510$key<- do.call(paste, c(lumensMainAnnualDatabase0510[c("LU_CHG", "ID_Z")], sep = " in "))
lumensMainAnnualDatabase0510$LU_CHG<-NULL
# main annual database with each area in final year, 2010
lumensMainAnnualDatabase<-merge(lumensMainAnnualDatabase0510, lumensMainDatabaseMeltTPMFixed, by='key')

########################################################################################################################
# Select period 2010 - 2014
lumensMainAnnualDatabase1015<-lumensMainDatabase[which(lumensMainDatabase$PERIOD==transitionPeriod[1]),]

nYear=5/(T2-T1)
lumensUnchangedArea<-subset(lumensMainAnnualDatabase1015, ID_LC_T1 == ID_LC_T2)
lumensChangedArea<-subset(lumensMainAnnualDatabase1015, ID_LC_T1 != ID_LC_T2)

# changed area per year
lumensChangedArea$COUNTx<-lumensChangedArea$COUNT*nYear # count per year
 lumensChangedArea$COUNTy<- lumensChangedArea$COUNTx # count for n-period minus count per year
# lumensChangedArea$COUNT<-lumensChangedArea$COUNTx # original count on changed area per year
lumensChangedArea$COUNTx<-NULL

lumensChangedAreaMelt<-melt(data = lumensChangedArea, id.vars=c('LC_T1', 'ID_Z'), measure.vars=c('COUNTy'))
lumensChangedAreaCast<-dcast(data = lumensChangedAreaMelt, formula = LC_T1 + ID_Z ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)
lumensChangedArea$COUNTy<-NULL

# unchanged area per year
lumensUnchangedArea<-merge(lumensUnchangedArea, lumensChangedAreaCast, by=c('LC_T1', 'ID_Z'))
# lumensUnchangedArea$COUNT<-lumensUnchangedArea$COUNT+lumensUnchangedArea$. # original count on unchanged area per year
lumensUnchangedArea$COUNT <- lumensUnchangedArea$. # AD repAbv
lumensUnchangedArea$.<-NULL
# main annual database with each area in base year, 2010
lumensMainAnnualDatabase1011<-rbind(lumensUnchangedArea, lumensChangedArea)

# Calculate Transition Probability Matrix
# calculate land use/cover area t1 for each planning unit
lumensMainDatabaseMeltT1 <- melt(data = lumensMainAnnualDatabase1011, id.vars=c('ID_LC_T1','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT1 <- dcast(data = lumensMainDatabaseMeltT1, formula = ID_LC_T1 + ID_Z ~ ., fun.aggregate = sum) 
colnames(countLandCoverByZoneT1)[3]<-'COUNT_LU_ZONE_T1'
# calculate land use/cover area t2 for each planning unit
lumensMainDatabaseMeltT2 <- melt(data = lumensMainAnnualDatabase1011, id.vars=c('ID_LC_T2','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT2 <- dcast(data = lumensMainDatabaseMeltT2, formula = ID_LC_T2 + ID_Z ~ ., fun.aggregate = sum)
colnames(countLandCoverByZoneT2)[3]<-'COUNT_LU_ZONE_T2'

lumensMainAnnualDatabase1011<-merge(lumensMainAnnualDatabase1011, countLandCoverByZoneT1, by=c('ID_LC_T1','ID_Z'))
lumensMainAnnualDatabase1011<-merge(lumensMainAnnualDatabase1011, countLandCoverByZoneT2, by.x=c('ID_LC_T1','ID_Z'), by.y=c('ID_LC_T2','ID_Z'))
# TPM 2010 - 2014
lumensMainAnnualDatabase1011$TPM2<-lumensMainAnnualDatabase1011$COUNT/lumensMainAnnualDatabase1011$COUNT_LU_ZONE_T1 
lumensMainAnnualDatabase1011<-replace(lumensMainAnnualDatabase1011, is.na(lumensMainAnnualDatabase1011), 0)

# Handling TPM
# Check if TPM has zero value
lumensMainDatabaseMeltTPM_CheckIfZero<-melt(data = lumensMainAnnualDatabase1011, id.vars=c('ID_LC_T1','ID_Z'), measure.vars=c('TPM2'))
lumensMainDatabaseMeltTPM_CheckIfZero<-dcast(data = lumensMainDatabaseMeltTPM_CheckIfZero, formula = ID_LC_T1 + ID_Z ~ ., fun.aggregate = sum)
# Add column check as a status
#   CEK=0 -> Fix; ELSE -> Ignore 
colnames(lumensMainDatabaseMeltTPM_CheckIfZero)[3]<-'CEK'
lumensMainDatabaseMeltTPM_CheckIfZero<-within(lumensMainDatabaseMeltTPM_CheckIfZero, { ACT<-ifelse(CEK==0, 'Fix', 'Ignore') })
lumensMainDatabaseMeltTPM_CheckIfZero$CEK<-NULL

lumensMainDatabaseMeltTPMFixed<-merge(lumensMainAnnualDatabase1011, lumensMainDatabaseMeltTPM_CheckIfZero, by=c('ID_LC_T1', 'ID_Z'))
# Fixed TPM2
lumensMainDatabaseMeltTPMFixed_Fix<-subset(lumensMainDatabaseMeltTPMFixed, ACT=='Fix')
lumensMainDatabaseMeltTPMFixed_Ignore<-subset(lumensMainDatabaseMeltTPMFixed, ACT=='Ignore')
if(nrow(lumensMainDatabaseMeltTPMFixed_Fix) > 0){
  lumensMainDatabaseMeltTPMFixed_Fix<-within(lumensMainDatabaseMeltTPMFixed_Fix, { TPM2<-ifelse(ID_LC_T1==ID_LC_T2, 1, TPM2) })
}
lumensMainDatabaseMeltTPMFixed<-rbind(lumensMainDatabaseMeltTPMFixed_Fix,lumensMainDatabaseMeltTPMFixed_Ignore)
lumensMainDatabaseMeltTPMFixed$ACT<-NULL

# Create unique names for each change
lumensMainDatabaseMeltTPMFixed$LU_CHG <- do.call(paste, c(lumensMainDatabaseMeltTPMFixed[c("LC_T1", "LC_T2")], sep = " to "))
lumensMainDatabaseMeltTPMFixed$key<- do.call(paste, c(lumensMainDatabaseMeltTPMFixed[c("LU_CHG", "ID_Z")], sep = " in "))
lumensMainDatabaseMeltTPMFixed<-subset(lumensMainDatabaseMeltTPMFixed, select=c(key, TPM2))

lumensMainAnnualDatabase1015$LU_CHG <- do.call(paste, c(lumensMainAnnualDatabase1015[c("LC_T1", "LC_T2")], sep = " to "))
lumensMainAnnualDatabase1015$key<- do.call(paste, c(lumensMainAnnualDatabase1015[c("LU_CHG", "ID_Z")], sep = " in "))
lumensMainAnnualDatabase1015$LU_CHG<-NULL
# main annual database with each area in final year, 2015
lumensMainAnnualDatabase1015<-merge(lumensMainAnnualDatabase1015, lumensMainDatabaseMeltTPMFixed, by='key')
lumensMainAnnualDatabase1015Selected<-subset(lumensMainAnnualDatabase1015, select=c(key, COUNT, TPM2))

# FINAL MERGE
lumensMainAnnualDatabase$COUNT<-NULL
lumensMainAnnualDatabase<-merge(lumensMainAnnualDatabase, lumensMainAnnualDatabase1015Selected, by='key')
########################################################################################################################
# Averate of TPM per year from 2005 - 2010 - 2014
lumensMainAnnualDatabase$TPM<-(lumensMainAnnualDatabase$TPM1+lumensMainAnnualDatabase$TPM2)/2
lumensMainAnnualDatabase$PERIOD<-lumensMainAnnualDatabase$key<-NULL
lumensMainAnnualDatabase$TPM1<-lumensMainAnnualDatabase$TPM2<-NULL

# get annual count in first  
lumensMainAnnualDatabaseMeltT1 <- melt(data = lumensMainAnnualDatabase, id.vars=c('ID_LC_T1','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT1 <- dcast(data = lumensMainAnnualDatabaseMeltT1, formula = ID_LC_T1 + ID_Z ~ ., fun.aggregate = sum) 
colnames(countLandCoverByZoneT1)[3]<-'COUNT_LU_ZONE_T1'
# second period
lumensMainAnnualDatabaseMeltT2 <- melt(data = lumensMainAnnualDatabase, id.vars=c('ID_LC_T2','ID_Z'), measure.vars=c('COUNT'))
countLandCoverByZoneT2 <- dcast(data = lumensMainAnnualDatabaseMeltT2, formula = ID_LC_T2 + ID_Z ~ ., fun.aggregate = sum)
colnames(countLandCoverByZoneT2)[3]<-'COUNT_LU_ZONE_T2'

lumensMainAnnualDatabase<-merge(lumensMainAnnualDatabase, countLandCoverByZoneT1, by=c('ID_LC_T1','ID_Z'))
lumensMainAnnualDatabase<-merge(lumensMainAnnualDatabase, countLandCoverByZoneT2, by.x=c('ID_LC_T1','ID_Z'), by.y=c('ID_LC_T2','ID_Z'))
# calculate predicted area at iteration 1
lumensMainAnnualDatabase$COUNT_IT0<-lumensMainAnnualDatabase$COUNT
lumensMainAnnualDatabase$COUNT_IT1<-lumensMainAnnualDatabase$TPM*lumensMainAnnualDatabase$COUNT_LU_ZONE_T2
# calculate predicted area at iteration-N
for (w in 2:iterationAnnual) {
  eval(parse(text=(paste("lumensMainAnnualDatabaseMeltT1 <- melt(data = lumensMainAnnualDatabase, id.vars=c('ID_LC_T2','ID_Z'), measure.vars=c('COUNT_IT",w-1,"'))", sep=""))))
  eval(parse(text=(paste("countLandCoverByZoneT", w+1, " <- dcast(data = lumensMainAnnualDatabaseMeltT1, formula = ID_LC_T2 + ID_Z ~ ., fun.aggregate = sum, fill = 0, drop = FALSE)", sep=""))))
  eval(parse(text=(paste("colnames(countLandCoverByZoneT", w+1,')[3]<-"COUNT_LU_ZONE_T', w+1, '"', sep=""))))
  
  eval(parse(text=(paste('lumensMainAnnualDatabase<-merge(lumensMainAnnualDatabase, countLandCoverByZoneT', w+1, ', by.x=c("ID_LC_T1", "ID_Z"), by.y=c("ID_LC_T2", "ID_Z"), all=TRUE)', sep=""))))
  eval(parse(text=(paste("lumensMainAnnualDatabase$COUNT_IT", w, "<-lumensMainAnnualDatabase$TPM*lumensMainAnnualDatabase$COUNT_LU_ZONE_T", w+1, sep=""))))
}

write.table(lumensMainAnnualDatabase, file="LUMENSHist_database.csv", row.names = F, col.names = T, quote=F, sep = ",")

# ######################################################################################################################
# # QUES-C                                                                                                               #
# # -------------------------------------------------------------------------------------------------------------------- #
# # 1) Emission                                                                                                          #
# #    - Summary, Infographic, Map                                                                                       #
# #                                                                                                                      #
# # 2) Sequestration                                                                                                     #
# #    - Summary, Infographic, Map                                                                                       #
# #                                                                                                                      #
# # 3) Peat emission                                                                                                     #
# #    - Summary, Infographic, Map                                                                                       #
# #                                                                                                                      #
# ########################################################################################################################
# 
# lumensMainAnnualDatabaseTemp<-lumensMainAnnualDatabase
# lumensMainAnnualDatabaseTemp$CHK_EM<-lumensMainAnnualDatabaseTemp$C_T1 > lumensMainAnnualDatabaseTemp$C_T2
# lumensMainAnnualDatabaseTemp$CHK_SQ<-lumensMainAnnualDatabaseTemp$C_T1 < lumensMainAnnualDatabaseTemp$C_T2
# # calculate annual emission
# annualEmission<-NULL
# for (y in 1:iterationAnnual) {
#   eval(parse(text=(paste("lumensMainAnnualDatabaseTemp$EM_T", y,"<-lumensMainAnnualDatabaseTemp$COUNT_IT", y, "*(lumensMainAnnualDatabaseTemp$C_T1-lumensMainAnnualDatabaseTemp$C_T2)*lumensMainAnnualDatabaseTemp$CHK_EM*3.67", sep=""))))
#   eval(parse(text=(paste("totalEmission<-sum(lumensMainAnnualDatabaseTemp$EM_T", y,")", sep=""))))
#   annualEmission<-c(annualEmission, totalEmission)
# }
# # calculate annual sequestration
# annualSequestration<-NULL
# for (x in 1:iterationAnnual) {
#   eval(parse(text=(paste("lumensMainAnnualDatabaseTemp$SQ_T", x,"<-lumensMainAnnualDatabaseTemp$COUNT_IT", x, "*(lumensMainAnnualDatabaseTemp$C_T2-lumensMainAnnualDatabaseTemp$C_T1)*lumensMainAnnualDatabaseTemp$CHK_SQ*3.67", sep=""))))
#   eval(parse(text=(paste("totalSequestration<-sum(lumensMainAnnualDatabaseTemp$SQ_T", x,")", sep=""))))
#   annualSequestration<-c(annualSequestration, totalSequestration)
# }
# # calculate annual emission from peat
# annualPeatEmission<-NULL
# for (x in 1:iterationAnnual) {
#   # period = 1 year
#   eval(parse(text=(paste("lumensMainAnnualDatabaseTemp$P_EM_T", x,"<-((lumensMainAnnualDatabaseTemp$P_T1+lumensMainAnnualDatabaseTemp$P_T2)/2)*lumensMainAnnualDatabaseTemp$COUNT_IT", x, sep=""))))
#   eval(parse(text=(paste("totalPeatEmission<-sum(lumensMainAnnualDatabaseTemp$P_EM_T", x,")", sep=""))))
#   annualPeatEmission<-c(annualPeatEmission, totalPeatEmission)
# }
# # table summary
# yearOfAnnualIteration<-c(paste(T5:(T5+iterationAnnual-1), (T5+1):(T5+iterationAnnual), sep="-"))
# 
# modelSummary<-data.frame(cbind(annualEmission, annualPeatEmission))
# modelSummary$totalEmission<-modelSummary$annualEmission+modelSummary$annualPeatEmission
# modelSummary<-cbind(modelSummary, annualSequestration)
# modelSummary$netEmission<-modelSummary$totalEmission-modelSummary$annualSequestration
# modelSummary$cummulativeEmission<-cumsum(modelSummary$totalEmission)
# modelSummary$cummulativeSequestration<-cumsum(modelSummary$annualSequestration)
# modelSummary$cummulativeNetEmission<-cumsum(modelSummary$netEmission)
# 
# modelSummary<-cbind(yearOfAnnualIteration, modelSummary)
# 
# colnames(modelSummary)=c('Tahun', 'Emisi_Karbon', 'Emisi_Gambut', 'Total_Emisi', 'Total_Sequestrasi', 'Emisi_Bersih', 'Emisi_Kumulatif', 'Sequestrasi_Kumulatif', 'Emisi_Bersih_Kumulatif')
# 
# 
# # QUES-C per period
# lumensMainDatabaseQUESC<-lumensMainDatabase
# lumensMainDatabaseQUESC$CHK_EM<-lumensMainDatabaseQUESC$C_T1 > lumensMainDatabaseQUESC$C_T2
# lumensMainDatabaseQUESC$CHK_SQ<-lumensMainDatabaseQUESC$C_T1 < lumensMainDatabaseQUESC$C_T2
# lumensMainDatabaseQUESC$EM<-lumensMainDatabaseQUESC$COUNT*(lumensMainDatabaseQUESC$C_T1-lumensMainDatabaseQUESC$C_T2)*lumensMainDatabaseQUESC$CHK_EM*3.67
# lumensMainDatabaseQUESC$SQ<-lumensMainDatabaseQUESC$COUNT*(lumensMainDatabaseQUESC$C_T2-lumensMainDatabaseQUESC$C_T1)*lumensMainDatabaseQUESC$CHK_SQ*3.67
# # calculate emission, sequestration, and peat emission
# lumensMainDatabaseQUESC<-cbind(lumensMainDatabaseQUESC, str_split_fixed(lumensMainDatabaseQUESC$PERIOD, '-', 2))
# lumensMainDatabaseQUESC$'1'<-as.numeric(as.character(lumensMainDatabaseQUESC$'1'))
# lumensMainDatabaseQUESC$'2'<-as.numeric(as.character(lumensMainDatabaseQUESC$'2'))
# lumensMainDatabaseQUESC$P_EM<-lumensMainDatabaseQUESC$COUNT*((lumensMainDatabaseQUESC$P_T1+lumensMainDatabaseQUESC$P_T2)/2)*(lumensMainDatabaseQUESC$'2'-lumensMainDatabaseQUESC$'1')
# lumensMainDatabaseQUESC$'1'<-lumensMainDatabaseQUESC$'2'<-NULL
# 
# # grouping all per period 
# lumensMainDatabaseQUESCMelt <- melt(data = lumensMainDatabaseQUESC, id.vars=c('PERIOD'), measure.vars=c('EM'))
# emissionPerPeriod <- dcast(data = lumensMainDatabaseQUESCMelt, formula =  PERIOD ~ ., fun.aggregate = sum)
# colnames(emissionPerPeriod)[2]='Emisi_Karbon'
# 
# lumensMainDatabaseQUESCMelt <- melt(data = lumensMainDatabaseQUESC, id.vars=c('PERIOD'), measure.vars=c('P_EM'))
# peatEmissionPerPeriod <- dcast(data = lumensMainDatabaseQUESCMelt, formula =  PERIOD ~ ., fun.aggregate = sum)
# colnames(peatEmissionPerPeriod)[2]='Emisi_Gambut'
# 
# lumensMainDatabaseQUESCMelt <- melt(data = lumensMainDatabaseQUESC, id.vars=c('PERIOD'), measure.vars=c('SQ'))
# sequestrationPerPeriod <- dcast(data = lumensMainDatabaseQUESCMelt, formula =  PERIOD ~ ., fun.aggregate = sum)
# colnames(sequestrationPerPeriod)[2]='Total_Sequestrasi'
# 
# modelSummaryPerPeriod<-merge(emissionPerPeriod, peatEmissionPerPeriod, by='PERIOD')
# modelSummaryPerPeriod$Total_Emisi<-modelSummaryPerPeriod$Emisi_Karbon+modelSummaryPerPeriod$Emisi_Gambut
# modelSummaryPerPeriod<-merge(modelSummaryPerPeriod, sequestrationPerPeriod, by='PERIOD')
# modelSummaryPerPeriod$Emisi_Bersih<-modelSummaryPerPeriod$Total_Emisi-modelSummaryPerPeriod$Total_Sequestrasi
# modelSummaryPerPeriod$Emisi_Kumulatif<-cumsum(modelSummaryPerPeriod$Total_Emisi)
# modelSummaryPerPeriod$Sequestrasi_Kumulatif<-cumsum(modelSummaryPerPeriod$Total_Sequestrasi)
# modelSummaryPerPeriod$Emisi_Bersih_Kumulatif<-cumsum(modelSummaryPerPeriod$Emisi_Bersih)
# 
# 
# ########################################################################################################################
# # Pre-QUES                                                                                                             #
# # -------------------------------------------------------------------------------------------------------------------- #
# # 1) Land Use/Cover Change Analysis                                                                                    #
# #    - General Land Use/Cover Change Table                                                                             #
# #    - Infographic                                                                                                     #
# #                                                                                                                      #
# # 2) Trajectory Analysis                                                                                               #
# #    - Deforestation, Degradation, Expansion of Plantation                                                             #
# #    - Infographic                                                                                                     #
# #                                                                                                                      #
# ########################################################################################################################
# ##
# ## 1) Land Use/Cover Change Analysis
# ##
# # A. Overall summary of land use change in hectare and its rate
# summaryOfLandUseChange$LU_CODE<-as.factor(toupper(abbreviate(summaryOfLandUseChange$Land_use_type, minlength=4, strict=FALSE, method="both")))
# summaryOfLandUseChangeRate<-summaryOfLandUseChangeHectare<-summaryOfLandUseChange[1:2]
# pColumnNameHectare<-NULL
# pColumnNameRate<-NULL
# for(o in 1:(nPeriod-5)){
#   timePeriod<-allPeriod[o+1] - allPeriod[o]
#   col1<-paste(allPeriod[o], '-', allPeriod[o+1], '_ha', sep='')
#   col2<-paste(allPeriod[o], '-', allPeriod[o+1], '_%/yrs', sep='')
#   eval(parse(text=(paste("pChange", "<-data.frame(summaryOfLandUseChange[,(", o, "+2+1)]-summaryOfLandUseChange[,(", o, "+2)])", sep=""))))
#   pRate<-round(((pChange / (summaryOfLandUseChange[3] * timePeriod)) * 100), 2)
#   colnames(pChange)[1]<-col1
#   colnames(pRate)[1]<-col2
#   summaryOfLandUseChangeHectare<-cbind(summaryOfLandUseChangeHectare,pChange)
#   summaryOfLandUseChangeRate<-cbind(summaryOfLandUseChangeRate,pRate)
#   pColumnNameHectare<-c(pColumnNameHectare, col1)
#   pColumnNameRate<-c(pColumnNameRate, col2)
# }
# # produce table and graph for overall change
# summaryOfLandUseChangeMelt <- melt(data = summaryOfLandUseChange, id.vars=c('Land_use_type','LU_CODE'))
# summaryOfLandUseChangeMelt <- summaryOfLandUseChangeMelt[which(summaryOfLandUseChangeMelt$variable!="ID_LC"),]
# colnames(summaryOfLandUseChangeMelt)<-c("Land_use_type", "LU_CODE", "Year", "Area")
# # overall changes plot 
# summaryOfLandUseChangePlotArea<-ggplot(summaryOfLandUseChangeMelt, aes(x=reorder(LU_CODE, -Area), y=Area, fill=Year)) +
#   geom_bar(stat="identity", position="dodge") + 
#   theme(axis.text.x= element_text(angle=45,hjust=1)) + 
#   ylab("Area (Ha)") +
#   xlab("Land use type") +
#   theme(legend.title = element_text(size=10), legend.text = element_text(size = 10), axis.text.x = element_text(size = 10), axis.title.x=element_blank())
# # produce table and graph for overall change in rate
# eval(parse(text=(paste("combineSummaryOfLandUseChange<-cbind(summaryOfLandUseChangeHectare,
#                        (summaryOfLandUseChangeRate[3:", 2+nPeriod-5,"]),
#                        (summaryOfLandUseChange[", 2+nLandCover+1, "]))", sep=""))))
# # overall changes plot for area changes
# combineSummaryOfLandUseChangeHectareMelt <- melt(data = combineSummaryOfLandUseChange, id.vars=c('Land_use_type','LU_CODE'), measure.vars=pColumnNameHectare)
# colnames(combineSummaryOfLandUseChangeHectareMelt)<-c("Land_use_type","LU_CODE", "Year", "Area")
# summaryOfLandUseChangePlotHectare<-ggplot(combineSummaryOfLandUseChangeHectareMelt, aes(x=reorder(LU_CODE, -Area), y=Area, fill=Year)) +
#   geom_bar(stat="identity",position="dodge") +
#   theme(axis.text.x= element_text(angle=45,hjust=1))+
#   ylab("Area (Ha)") +
#   xlab("Land use type") +
#   theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10), axis.title.x=element_blank()) +
#   coord_flip()
# # overall changes plot for rate changes
# combineSummaryOfLandUseChangeRateMelt <- melt(data = combineSummaryOfLandUseChange, id.vars=c('Land_use_type','LU_CODE'), measure.vars=pColumnNameRate)
# colnames(combineSummaryOfLandUseChangeRateMelt)<-c("Land_use_type","LU_CODE", "Year", "Area")
# summaryOfLandUseChangePlotRate<-ggplot(combineSummaryOfLandUseChangeRateMelt, aes(x=reorder(LU_CODE, -Area), y=Area, fill=Year)) +
#   geom_bar(stat="identity",position="dodge") +
#   theme(axis.text.x = element_text(angle=45,hjust=1)) +
#   ylab("Area (Ha)") +
#   xlab("Land use type") +
#   theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10), axis.title.x=element_blank()) +
#   coord_flip() +
#   ylim (c(-100, 100))
# 
# 
# # B. Calculate top ten largest land use changes 
# lumensMainDatabaseEmitZero <- lumensMainDatabase[which(lumensMainDatabase$COUNT > 0),]
# for(j in 2:(nPeriod-5)){
#   eval(parse(text=(paste("largestChanges <- subset(lumensMainDatabaseEmitZero, PERIOD=='", allPeriod[j], "-", allPeriod[j+1], "')", sep=""))))
#   largestChanges <- subset(largestChanges, select=c(ID_LC_T1, ID_LC_T2, ID_Z, LC_T1, LC_T2, PLAN, COUNT))
#   largestChanges$LU_CHG <- do.call(paste, c(largestChanges[c('LC_T1', 'LC_T2')], sep = ' to '))
#   
#   # create IDChange, the difference between two ID_L, which is used to see the changes in land cover
#   # then remove unchanged land cover, and finally save the top ten largest changes
#   largestChanges$ID1<-as.numeric(as.character((largestChanges$ID_LC_T1)))
#   largestChanges$ID2<-as.numeric(as.character((largestChanges$ID_LC_T2)))
#   largestChanges$IDChange<-largestChanges$ID1-largestChanges$ID2
#   largestChanges<-largestChanges[which(largestChanges$IDChange!=0),]
#   largestChanges<-as.data.frame(largestChanges[order(-largestChanges$COUNT),])
#   largestChanges$CHG_CODE<-as.factor(toupper(abbreviate(largestChanges$LU_CHG, minlength=5, strict=FALSE, method="both")))
#   largestChanges$ID1<-largestChanges$ID2<-largestChanges$IDChange<-NULL
#   
#   # top ten changes
#   largestChangeTopTen<-head(largestChanges, n=10)
#   largestChangeTopTen$LC_T1<-largestChangeTopTen$LC_T2<-NULL
#   
#   # summary of landuse dominant change
#   changeOnly<-aggregate(COUNT~LU_CHG, data=largestChanges, FUN=sum)
#   changeOnly$CHG_CODE<-as.factor(toupper(abbreviate(changeOnly$LU_CHG, minlength=5, strict=FALSE, method="both")))
#   changeOnly<-changeOnly[order(-changeOnly$COUNT),]
#   changeOnly<-changeOnly[c(1,3,2)]
#   
#   # top ten dominant changes based on landuse/cover change
#   changeOnlyTopTen<-head(changeOnly, n=10)
#   
#   # summary of zonal dominant change
#   largestDominantChangesByZonal<-as.data.frame(NULL)
#   for (l in 1:length(freqZPlanningUnit$ID_Z)){
#     tryCatch({
#       largestChangesZone<-as.data.frame(largestChanges[which(largestChanges$ID_Z == freqZPlanningUnit$ID_Z[l]),])
#       largestChangesZone<-aggregate(COUNT~ID_Z+LU_CHG, data=largestChangesZone, FUN=sum)
#       largestChangesZone$CHG_CODE<-as.factor(toupper(abbreviate(largestChangesZone$LU_CHG, minlength=5, strict=FALSE, method="both")))
#       largestChangesZone<-largestChangesZone[order(-largestChangesZone$COUNT),]
#       largestChangesZone<-largestChangesZone[c(1,2,4,3)]
#       #top ten dominant changes based on planning unit
#       largestChangesZoneTopTen<-head(largestChangesZone, n=10)
#       largestDominantChangesByZonal<-rbind(largestDominantChangesByZonal, largestChangesZoneTopTen)
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#   }
#   
#   # produce chart of largest source of changes in landuse
#   colnames(changeOnlyTopTen)[3]<-"COUNT"
#   largestChangesPlot <- ggplot(data=changeOnlyTopTen, aes(x=reorder(CHG_CODE, -COUNT), y=COUNT, fill=CHG_CODE)) +
#     geom_bar(stat='identity', position='dodge') +
#     geom_text(data=changeOnlyTopTen, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)), size=3, vjust=0.1) +
#     ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", analyzedLocation, allPeriod[j], "-", allPeriod[j+1] )) +
#     labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') +
#     guides(fill=FALSE) +
#     theme(plot.title = element_text(lineheight= 5, face="bold")) + 
#     scale_y_continuous() +
#     theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8), panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#   
#   # save variable
#   eval(parse(text=(paste('changeOnlyTopTen', allPeriod[j], '_', allPeriod[j+1], '<-changeOnlyTopTen', sep=''))))  
#   eval(parse(text=(paste('largestDominantChangesByZonal', allPeriod[j], '_', allPeriod[j+1], '<-largestDominantChangesByZonal', sep=''))))  
#   eval(parse(text=(paste('largestChangesPlot', allPeriod[j], '_', allPeriod[j+1], '<-largestChangesPlot', sep=''))))  
# }
# 
# # C. Infographic
# 
# 
# ##
# ##2) Trajectory Analysis 
# ##
# for(m in 1:(nPeriod-5)){
#   lumensMainDatabaseTrajectories<-lumensMainDatabaseClassified
#   eval(parse(text=(paste('lumensMainDatabaseTrajectories$ID_CL_T1_temp<-lumensMainDatabaseTrajectories$ID_CL_T', m, sep=''))))
#   eval(parse(text=(paste('lumensMainDatabaseTrajectories$ID_CL_T2_temp<-lumensMainDatabaseTrajectories$ID_CL_T', m+1, sep=''))))
#   lumensMainDatabaseTrajectories$CL1<-lumensMainDatabaseTrajectories$ID_CL_T1_temp*10
#   lumensMainDatabaseTrajectories$CL2<-lumensMainDatabaseTrajectories$ID_CL_T2_temp
#   lumensMainDatabaseTrajectories$trajectoryChangeCode<-lumensMainDatabaseTrajectories$CL1+lumensMainDatabaseTrajectories$CL2
#   lumensMainDatabaseTrajectories<-as.data.frame(merge(lumensMainDatabaseTrajectories, lookupTableTrajectoriesChanges, by='trajectoryChangeCode'))
#   lumensMainDatabaseTrajectories$TR_CODE<-toupper(abbreviate(lumensMainDatabaseTrajectories$trajectoryName))
#   
#   # 1. Deforestation
#   tryCatch({
#     indexDeforestation1<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp==1)
#     indexDeforestation2<-subset(lumensMainDatabaseTrajectories, ID_CL_T2_temp==2)
#     indexDeforestation<-rbind(indexDeforestation1, indexDeforestation2)
#     indexDeforestation<-subset(indexDeforestation, ID_CL_T2_temp!=1)
#     indexDeforestation<-subset(indexDeforestation, ID_CL_T2_temp!=2)
#     indexDeforestation<-subset(indexDeforestation, COUNT!=0)
#     indexDeforestation <- aggregate(COUNT ~  ZONE, data=indexDeforestation, FUN=sum)
#     colnames(indexDeforestation)<-c('ZONE', 'Deforestasi')
#     totalDeforestation<-data.frame(ZONE="TOTAL", Deforestasi=sum(indexDeforestation[,2]))
#     indexDeforestation<-rbind(indexDeforestation, totalDeforestation)
#     eval(parse(text=(paste('indexDeforestation', allPeriod[m], '_', allPeriod[m+1], '<-indexDeforestation', sep=''))))
#   }, error=function(e){cat("No deforestation found", "\n")})
#   
#   # 2. Forest degradation
#   tryCatch({
#     indexForestDegradation<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp==1)
#     indexForestDegradation<-subset(indexForestDegradation, ID_CL_T2_temp==2)
#     indexForestDegradation<-subset(indexForestDegradation, COUNT!=0)
#     indexForestDegradation<-aggregate(COUNT ~  ZONE, data=indexForestDegradation, FUN=sum)
#     colnames(indexForestDegradation)<-c('ZONE', 'Degradasi_Hutan')
#     totalForestDegradation<-data.frame(ZONE="TOTAL", Degradasi_Hutan=sum(indexForestDegradation[,2]))
#     indexForestDegradation<-rbind(indexForestDegradation, totalForestDegradation)
#     eval(parse(text=(paste('indexForestDegradation', allPeriod[m], '_', allPeriod[m+1], '<-indexForestDegradation', sep=''))))
#   }, error=function(e){cat("No degradation found", "\n")})
#   
#   # 3. Reforestation
#   tryCatch({
#     indexReforestation<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp!=1)
#     indexReforestation<-subset(indexReforestation, ID_CL_T1_temp!=2)
#     indexReforestation1<-subset(indexReforestation, ID_CL_T2_temp==1)
#     indexReforestation2<-subset(indexReforestation, ID_CL_T2_temp==2)
#     indexReforestation<-rbind(indexReforestation1, indexReforestation2)
#     indexReforestation<-subset(indexReforestation, COUNT!=0)
#     indexReforestation<-aggregate(COUNT ~  ZONE, data=indexReforestation, FUN=sum)
#     colnames(indexReforestation)<-c('ZONE', 'Reforestasi')
#     totalReforestation<-data.frame(ZONE="TOTAL", Reforestasi=sum(indexReforestation[,2]))
#     indexReforestation<-rbind(indexReforestation, totalReforestation)
#     eval(parse(text=(paste('indexReforestation', allPeriod[m], '_', allPeriod[m+1], '<-indexReforestation', sep=''))))
#   }, error=function(e){cat("No reforestation found", "\n")})
#   
#   # 4. Stable forest
#   # colnames(indexStableForest)<-c('ZONE', 'Tetap_Hutan')
#   # totalStableForest<-data.frame(ZONE="TOTAL", Tetap_Hutan=sum(indexStableForest[,2]))
#   # indexStableForest<-rbind(indexStableForest, totalStableForest)
#   
#   # 5. Initial forest cover
#   tryCatch({
#     indexInitialForestCover<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp==1)
#     indexInitialForestCover<-subset(indexInitialForestCover, COUNT!=0)
#     indexInitialForestCover<-aggregate(COUNT ~  ZONE, data=indexInitialForestCover, FUN=sum)
#     colnames(indexInitialForestCover)<-c('ZONE', 'Forest_T1')
#     totalInitialForestCover<-data.frame(ZONE="TOTAL", Forest_T1=sum(indexInitialForestCover[,2]))
#     indexInitialForestCover<-rbind(indexInitialForestCover, totalInitialForestCover)
#     eval(parse(text=(paste('indexInitialForestCover', allPeriod[m], '_', allPeriod[m+1], '<-indexInitialForestCover', sep=''))))
#   }, error=function(e){cat("No initial forest cover found", "\n")})
#   
#   # 6. Total forest cover
#   tryCatch({
#     indexTotalForestCover1<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp==1)
#     indexTotalForestCover2<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp==2)
#     indexTotalForestCover<-rbind(indexTotalForestCover1, indexTotalForestCover2)
#     indexTotalForestCover<-subset(indexTotalForestCover, COUNT!=0)
#     indexTotalForestCover<-aggregate(COUNT ~  ZONE, data=indexTotalForestCover, FUN=sum)
#     colnames(indexTotalForestCover)<-c('ZONE', 'Forest_T1')
#     totalTotalForestCover<-data.frame(ZONE="TOTAL", Forest_T1=sum(indexTotalForestCover[,2]))
#     indexTotalForestCover<-rbind(indexTotalForestCover, totalTotalForestCover)
#     eval(parse(text=(paste('indexTotalForestCover', allPeriod[m], '_', allPeriod[m+1], '<-indexTotalForestCover', sep=''))))
#   }, error=function(e){cat("No total forest cover found", "\n")})
#   
#   # 7. Initial non-forest
#   tryCatch({
#     indexInitialNonForest<-subset(lumensMainDatabaseTrajectories, ID_CL_T1_temp!=1)
#     indexInitialNonForest<-subset(indexInitialNonForest, ID_CL_T1_temp!=2)
#     indexInitialNonForest<-subset(indexInitialNonForest, COUNT!=0)
#     indexInitialNonForest<-aggregate(COUNT ~  ZONE, data=indexInitialNonForest, FUN=sum)
#     colnames(index.init.nonforest)<-c('ZONE', 'Forest_T1')
#     totalInitialNonForest<-data.frame(ZONE="TOTAL",Forest_T1=sum(indexInitialNonForest[,2]))
#     indexInitialNonForest<-rbind(indexInitialNonForest, totalInitialNonForest)
#     eval(parse(text=(paste('indexInitialNonForest', allPeriod[m], '_', allPeriod[m+1], '<-indexInitialNonForest', sep=''))))
#   }, error=function(e){cat("No initial forest cover found", "\n")})
#   
#   # Degradation rate 
#   tryCatch({
#     degradationRate<-merge(indexForestDegradation, indexInitialForestCover, by='ZONE', all=T)
#     degradationRate$Degradation_Rate<-(degradationRate[2]/degradationRate[3]*100)[,1]
#     degradationRate<-cbind(degradationRate[1], round(degradationRate[4],2))
#     eval(parse(text=(paste('degradationRate', allPeriod[m], '_', allPeriod[m+1], '<-degradationRate', sep=''))))
#   }, error=function(e){cat("No degradation found", conditionMessage(e), "\n")})
#   
#   # Deforestation rate 
#   tryCatch({
#     deforestationRate<-merge(indexDeforestation, indexTotalForestCover, by='ZONE', all=T)
#     deforestationRate$Deforestation_Rate<-(deforestationRate[2]/deforestationRate[3]*100)[,1]
#     deforestationRate<-cbind(deforestationRate[1], round(deforestationRate[4],2))
#     eval(parse(text=(paste('deforestationRate', allPeriod[m], '_', allPeriod[m+1], '<-deforestationRate', sep=''))))
#   }, error=function(e){cat("No deforestation found", conditionMessage(e), "\n")})
#   
#   # Reforestation rate
#   tryCatch({
#     reforestationRate<-merge(indexReforestation, indexInitialNonForest, by='ZONE', all=T)
#     reforestationRate$Reforestation_Rate<-(reforestationRate[2]/reforestationRate[3]*100)[,1]
#     reforestationRate<-cbind(reforestationRate[1], round(reforestationRate[4],2))
#     eval(parse(text=(paste('reforestationRate', allPeriod[m], '_', allPeriod[m+1], '<-reforestationRate', sep=''))))
#   }, error=function(e){cat("No reforestation found", conditionMessage(e), "\n")})
#   
#   # Summary forest change in percentage
#   forestChangeRate<-merge(deforestationRate, degradationRate, by='ZONE', all=T)
#   if(ncol(reforestationRate)==2){
#     forestChangeRate<-merge(forestChangeRate, reforestationRate, by='ZONE',all=T)
#     eval(parse(text=(paste('forestChangeRate', allPeriod[m], '_', allPeriod[m+1], '<-forestChangeRate', sep=''))))
#   } else {
#     print('no reforestation found')
#   }
#   
#   # select column ?
#   # save variables
#   eval(parse(text=(paste('lumensMainDatabaseTrajectories', allPeriod[m], '_', allPeriod[m+1], '<-lumensMainDatabaseTrajectories', sep=''))))
#   
#   # Trajectories map
#   eval(parse(text=(paste('landCoverTrajectory1<-landCover', allPeriod[m], sep=''))))
#   eval(parse(text=(paste('landCoverTrajectory2<-landCover', allPeriod[m+1], sep=''))))
#   
#   landCoverTrajectory1<-reclassify(landCoverTrajectory1, cbind(128, NA))
#   landCoverTrajectory2<-reclassify(landCoverTrajectory2, cbind(128, NA))
#   
#   landCoverTrajectory1<-ratify(landCoverTrajectory1,count=TRUE,overwrite=TRUE)
#   landCoverTrajectory2<-ratify(landCoverTrajectory2,count=TRUE,overwrite=TRUE)
#   
#   colnames(lookupTableClassifiedOnly)[2]="ID"
#   levels(landCoverTrajectory1)<-merge((levels(landCoverTrajectory1)), lookupTableClassifiedOnly, by="ID")
#   levels(landCoverTrajectory2)<-merge((levels(landCoverTrajectory2)), lookupTableClassifiedOnly, by="ID")
#   colnames(lookupTableClassifiedOnly)[2]="ID_LC"
#   
#   # Reached total allocation of 8133Mb
#   landCoverTrajectory1<-deratify(landCoverTrajectory1, 'ID_CL')
#   landCoverTrajectory2<-deratify(landCoverTrajectory2, 'ID_CL')
#   
#   landUseTrajectoryMap<-overlay(landCoverTrajectory1, landCoverTrajectory2, fun=function(x,y){return((x*10)+y)})
#   landUseTrajectoryMap<-ratify(landUseTrajectoryMap, count=TRUE, overwrite=TRUE)
#   
#   colnames(lookupTableTrajectoriesChanges)[2]="ID"
#   levels(landUseTrajectoryMap)<-merge((levels(landUseTrajectoryMap)), lookupTableTrajectoriesChanges, by='ID')
#   landUseTrajectoryMapFinal<-deratify(landUseTrajectoryMap, 'trajectoryID')
#   landUseTrajectoryMapFinal<-ratify(landUseTrajectoryMapFinal, count=TRUE, overwrite=TRUE)
#   colnames(lookupTableTrajectoriesChanges)[2]="trajectoryChangeCode"
#   
#   colnames(lookupTableTrajectoriesChanges)[4]="ID"
#   levels(landUseTrajectoryMapFinal)<-merge((levels(landUseTrajectoryMapFinal)), lookupTableTrajectoriesChanges, by='ID')
#   colnames(lookupTableTrajectoriesChanges)[4]="trajectoryID"
#   
#   eval(parse(text=(paste('landUseTrajectoryMapFinal', allPeriod[m], '_', allPeriod[m+1], '<-landUseTrajectoryMapFinal', sep=''))))
#   
#   # Calculate summary statistics by zone and overall
#   lumensMainDatabaseTrajectoriesMelt <- melt(data = lumensMainDatabaseTrajectories, id.vars=c('ZONE','trajectoryName', 'TR_CODE'), measure.vars=c('COUNT'))
#   lumensMainDatabaseTrajectoriesZone <- dcast(data = lumensMainDatabaseTrajectoriesMelt, formula = ZONE ~ TR_CODE, fun.aggregate = sum)
#   lumensMainDatabaseTrajectoriesOverall <- dcast(data = lumensMainDatabaseTrajectoriesMelt, formula = trajectoryName ~ ., fun.aggregate = sum)
#   
#   lumensMainDatabaseTrajectoriesForestMelt <- melt(data = lumensMainDatabaseTrajectories, id.vars=c('ZONE','trajectoryStatus'), measure.vars=c('COUNT'))
#   lumensMainDatabaseTrajectoriesForestZone <- dcast(data = lumensMainDatabaseTrajectoriesForestMelt, formula = ZONE ~ trajectoryStatus, fun.aggregate = sum)
#   lumensMainDatabaseTrajectoriesForestZone$Other<-lumensMainDatabaseTrajectoriesForestZone$Others<-NULL
#   
#   lumensMainDatabaseTrajectorieForestOverall <- dcast(data = lumensMainDatabaseTrajectoriesForestMelt, formula = trajectoryStatus ~ ., fun.aggregate = sum)
#   
#   lumensMainDatabaseTrajectoriesDriveMelt <- melt(data = lumensMainDatabaseTrajectories, id.vars=c('trajectoryName','trajectoryStatus'), measure.vars=c('COUNT'))
#   lumensMainDatabaseTrajectoriesDriveZone <- dcast(data = lumensMainDatabaseTrajectoriesDriveMelt, formula = trajectoryName ~ trajectoryStatus, fun.aggregate = sum)
#   lumensMainDatabaseTrajectoriesDriveZone$Other<-lumensMainDatabaseTrajectoriesDriveZone$Others<-NULL
#   
#   colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","Abbrev", "variable", "Area"); #rename column names
#   #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
#   plot_traj<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#     theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#     theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+
#     theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#   
#   plot_traj_group<-ggplot(data=PreQUES_traj_database.overal,aes(Traj,.,fill=Traj))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#     theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#     theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
#     theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#   
#   colnames(PreQUES_traj_forest.melt)<-c("Zone", "Forest_Change","variable", "Area"); #rename column names
#   #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
#   plot_def<-ggplot(data=PreQUES_traj_forest.melt,aes(factor(Zone),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#     theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#     theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
#     theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#   
#   colnames(PreQUES_traj_drive.melt)<-c("Trajectories", "Forest_Change","variable", "Area"); #rename column names
#   #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
#   plot_drive<-ggplot(data=PreQUES_traj_drive.melt,aes(factor(Trajectories),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#     theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#     theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
#     theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#   
#   colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
#   colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")
#   colnames(PreQUES_traj_forest.overal)<-c("Forest cover changes", "Area (Ha)")
#   
#   #plot: total changes per trajectory
#   for(s in 2:(ncol(PreQUES_traj_database.zone))){
#     print(s)
#     c<-s-1
#     PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_database.zone, id.vars=c('Trajectories'), measure.vars=c(colnames(PreQUES_traj_database.zone)[s]))
#     plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(Trajectories),value,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#       theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#       theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
#       theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#     #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
#     addNewLine(rtffile)
#     addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
#     addNewLine(rtffile)
#     addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Lahan di Berbagai Zona Perencanaan untuk jenis ",colnames(PreQUES_traj_database.zone)[s], "\\b0 \\fs20 di ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
#     addNewLine(rtffile)
#   }
#   
#   #plot: total changes per trajectory
#   for(s in 2:(ncol(PreQUES_traj_forest.zone))){
#     print(s)
#     c<-s-1
#     PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_forest.zone, id.vars=c('Z_NAME'), measure.vars=c(colnames(PreQUES_traj_forest.zone)[s]))
#     colnames(PreQUES_traj_database.zone.melt_pertrajek)[1]<-'ZONE'
#     plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(ZONE),value,fill=factor(ZONE)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#       theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#       theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
#       theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#     #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
#     addNewLine(rtffile)
#     addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
#     addNewLine(rtffile)
#     addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Hutan di Berbagai Zona Perencanaan untuk ",colnames(PreQUES_traj_forest.zone)[s], "\b  di \\b0 \\fs20", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
#     addNewLine(rtffile)
#   }
# }
