# QUES-C remake
# AD
# Inputs: Results of previous run "PQ_QB.R"

# library-----
# libraries with no duplicates====
library(rtf)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(rgeos)
library(grid)
library(jsonlite)
library(RPostgreSQL)
library(DBI)
library(rpostgis)
library(spatial.tools)
library(rgdal)
library(plyr)
library(tiff)
library(reshape2)
library(foreign)
library(splitstackshape)
library(magick)
library(vegan)
library(RSQLite)
library(SDMTools)
library(dplyr)
library(gridExtra)
library(pracma)
library(zoo)


# 1. Setting up basic database parameters====
proj.file <- choose.files(default = "D:/GG_Jambi/process/lumens_dir/trial/bauP_Jambi/bauP_Jambi.lpj")
# select the .lpj based on which the project shall be executed
load(proj.file)

driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname="bauP_Jambi", host=as.character(pgconf$host), port=as.character(pgconf$port), #ADCHECK
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

# calling up the lists of data
# Setting up the working directory
setwd(paste0(dirname(proj.file), "/QUES"))
# derive the list of available data----
list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))

# To retrieve ref data


# retrieve the cstock and peat_em also other tables====
c_luName <- "cstock"
peat_luName <- "em_peat"
pu_name <- "pu_ver1"
if (pu_name=="ref") {
  lookup_z <- dbReadTable(DB, c("public", "lut_ref"))
  names(lookup_z) <- c("ID_Z", "Z_NAME")
} else {
  lookup_z<-dbReadTable(DB, c("public", list_of_data_pu[list_of_data_pu$RST_NAME==pu_name, "LUT_NAME"]))
  names(lookup_z) <- c("ID_Z", "COUNT", "Z_NAME")
  lookup_z <- lookup_z[ , c(1,3)]
}
# standardized column names

# extract peat planning unit IDs
peat_keyword <- "ambut"
peat_puID <- lookup_z[grep(peat_keyword, lookup_z[,2]) , 1]
cstock <- dbReadTable(DB, c("public", list_of_data_lut[list_of_data_lut$TBL_NAME==c_luName, "TBL_DATA"]))
em_peat <- dbReadTable(DB, c("public", list_of_data_lut[list_of_data_lut$TBL_NAME==peat_luName, "TBL_DATA"]))

# lut_ref has been available in the proj.file

# setting the combined raster layer
ref <- ref*10^6

yir <- unique(list_of_data_luc$PERIOD)
yir <- sort(yir)

# LOOPING FRAMEWORK FOR EACH TIMESTEP====
for(ts in 1: (length(yir)-1)){
  # retrieve the changemap
  ti1 <- yir[ts]
  ti2 <- yir[ts+1]
  d_ti <- ti2-ti1 # delta year
  chMap_name <- paste0("chgmap_", pu_name, ti1, ti2)
  chMap_name <- list_of_data_f[list_of_data_f$RST_NAME == chMap_name, "RST_DATA"]
  ch_map <- getRasterFromPG(pgconf, project, chMap_name, paste0(chMap_name, ".tif"))#getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
  # combine with admin data
  ch_map <- ref+ch_map
  # retrieve freq
  fr_chMap <- data.frame(freq(ch_map, useNA = "no"), stringsAsFactors= FALSE)
  # disaggregate using substr # note that the result will be presented as character. Do convert into numeric
  fr_chMap$IDADM <- floor(fr_chMap$value / 10^6)
  fr_chMap$ID_Z <- fr_chMap$value%%10^2
  fr_chMap$ID_T1 <- floor(fr_chMap$value%%10^4/10^2)
  fr_chMap$ID_T2 <- floor(fr_chMap$value%%10^6/10^4)
  
  # carbon merge t1-t2
  for(w in 1:2){
    if(w ==1) orNames <- names(cstock)
    names(cstock)[1] <-paste0("ID_T", w)
    names(cstock)[2] <-paste0("LC_", w)
    names(cstock)[3] <- paste0("c_", w)
    fr_chMap <- merge(fr_chMap, cstock, by= paste0("ID_T", w), all.x = TRUE)
    if(w ==2) names(cstock) <- orNames
  }
  
  # calculate emission
  fr_chMap$Em_co2Eq <- (fr_chMap$c_1 - fr_chMap$c_2)*fr_chMap$count*res(ref)[1]^2/10000 * 3.67
  fr_chMap$Seq <- 0
  fr_chMap[fr_chMap$Em_co2Eq < 0, "Seq"] <- -1* fr_chMap[fr_chMap$Em_co2Eq < 0, "Em_co2Eq"]
  fr_chMap[fr_chMap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission
  # peat_em merge t1-t2
  rec_table <- fr_chMap[, c("value", "Em_co2Eq", "Seq", "count")]
  rec_table$Em_px <- rec_table$Em_co2Eq/rec_table$count
  rec_table$Seq_px <- rec_table$Seq/rec_table$count
  # generate emission and sequestration map
  em_map <- reclassify(ch_map, rec_table[, c("value", "Em_px")])
  writeRaster(em_map, paste0("QUES-C/Em_", ti1, "_", ti2, ".tif"))
  seq_map <- reclassify(ch_map, rec_table[, c("value", "Seq_px")])
  writeRaster(seq_map, paste0("QUES-C/Seq_", ti1, "_", ti2, ".tif"))
  # peat_puID contains numbers which stand for id of pu containing peat
  for(w in 1:2){
    em_peatMod <- em_peat[, c(1, 3)]
    em_peatMod[, 2] <- em_peatMod[, 2]*d_ti/2
    names(em_peatMod)[1] <-paste0("ID_T", w)
    names(em_peatMod)[2] <- paste0("EmPeat_", w)
    fr_chMap <- merge(fr_chMap, em_peatMod, by= paste0("ID_T", w), all.x = TRUE)
  }
  # correction for peat_em which falls in non-peat planning unit (*0)
  fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")] <- fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")]*0
  # calculate total peat_em
  fr_chMap[, c("EmPeat_1", "EmPeat_2")] <- fr_chMap[, c("EmPeat_1", "EmPeat_2")]*fr_chMap[, "count"]* res(ref)[1]^2/10000 # conversion factor to hectare
  fr_chMap$EmPeatTot <- fr_chMap$EmPeat_1 + fr_chMap$EmPeat_2
  # legend merge using lookup tables: pu, admin
  fr_chMap <- merge(fr_chMap, lut_ref, by = "IDADM", all.x = TRUE)
  fr_chMap <- merge(fr_chMap, lookup_z, by = "ID_Z", all.x =TRUE)
  # add period annotation
  fr_chMap$PERIOD <- paste0(ti1, "-", ti2)
  # store at master table, in .csv, rbind with previous runs
  if(ts == 1) carb_compile <- fr_chMap else carb_compile <- data.frame(rbind(carb_compile, fr_chMap), stringsAsFactors = FALSE)
  # Summarize calculation result following the template: Period; Gross Em; Seq; Nett abg em; peat em; Total em. Store as .csv
  if(ts == 1) {
    summary_table <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
  } else {
    summary_add <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
    summary_table <- data.frame(rbind(summary_table, summary_add), stringsAsFactors = FALSE)
  }
  if(ts == (length(yir)-1)){
    summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
    write.csv(summary_table, "QUES-C/summary_emission.csv", row.names = FALSE)
    write.csv(carb_compile, "QUES-C/emission_LCdynamics.csv", row.names = FALSE)
  }
}
