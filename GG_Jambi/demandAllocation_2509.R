# From: demand_all_nov16.R
#DISCLAIMER:
#1. Make sure the simulated land cover has been renamed into the standard format and saved in the urlAddressRaster directory

#The input raster should be named lc2222_sumsel_v2.tif just like the other rasters
library(raster)
library('reshape2')
library(reshapeGUI)
library(r2excel)
library(readxl)

#FUNCTION DEFINITION====
loadDemandTable <- function(period= integer(),url=urlAddressTabular, file= "land_demand_correct.xlsx"){
  read_excel(paste0(url, file), (period+1))#ADjust
}

loadRaster <- function(url, file){
  raster(paste(url, file, sep=''))
}

z_avail <- function(z_num= l){
  av_tab <- big_freqtab[big_freqtab$value > z_num*100 & big_freqtab$value < (z_num+1)*100, ]
  av_tab$value <- av_tab$value - z_num*100
  nul_av_lcid <- src_id[! src_id %in% av_tab$value]
  if(length(nul_av_lcid)!=0){
    for(r in 1: length(nul_av_lcid)){
      av_tab <- rbind(av_tab, c(nul_av_lcid[r],0))
    }
  }
  # add rows with 0 area for undefined landcover classes
  # unClass <- setdiff(lc_indo_lu$ID, av_tab[,1])
  # add_avTab <- data.frame(value = unClass, area = 0, stringsAsFactors = FALSE)
  # names(add_avTab) <- names(av_tab)
  # av_tab <- data.frame(rbind(av_tab, add_avTab))
  return(av_tab)
}

# a <- freq(supp_rst[Which(zone_rst==l, cells=TRUE)])


urlAddressTabular <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/"#ADjusted
urlAddressRaster <- "D:/GG_Jambi/General/data/table/land_requirements/raster_dir/"
tpm_dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready35-40/complete_trans/"#CHGABLE
tpm_dir_prev <- ""#CHGABLE
lc_indo_lu <- read.csv(paste0(urlAddressTabular, "lut_lu.csv"), stringsAsFactors = FALSE)
comm_lc_id <- c(10, 13, 20, 22) #ADjusted
project_time <- seq(2015, 2045,by = 5) #ADjusted
in_rst_time <- 2040 #CHGABLE
supp_rst <- loadRaster(urlAddressRaster, paste0("lc",in_rst_time,"_jambi.tif"))
# if(in_rst_time != project_time[1]) real_rst <- loadRaster(urlAddressRaster, paste0("lc", in_rst_time,"_sumsel_v2.tif"))
zone_rst <- loadRaster(urlAddressRaster, "pu_final.tif")

#combine the pu raster file with lc map====
lcxzone_rst <- zone_rst*100 + supp_rst
big_freqtab <- data.frame(freq(lcxzone_rst), stringsAsFactors = FALSE)
big_freqtab <- big_freqtab[!is.na(big_freqtab$value),]



setwd(tpm_dir)
tpm_flt <- list.files(pattern = "^Single_step0",full.names = FALSE,include.dirs = FALSE)
tpm_pers <- list.files(pattern = "^Unchanged_step", full.names = FALSE, include.dirs = FALSE)

n_zones <- length(tpm_flt)
t = 6 #ADINPUT====
# for(t in 1: (length(project_time)-1)){ #ADmanual
  eval(parse(text=paste0("demand_p", t, "<- loadDemandTable(", t,")")))
  eval(parse(text=paste0("demand_p", t, "<- demand_p", t,"[demand_p", t,"$ID %in% comm_lc_id,1:(n_zones+2)]"))) #why +2?
# }

demand_file <- eval(parse(text=paste0("demand_p", which(project_time==in_rst_time))))
names(demand_file)[3: ncol(demand_file)] <- gsub(pattern = "Z", replacement = "z", x = names(demand_file)[3:ncol(demand_file)])
check_tab <- data.frame()

#read the expected lc area directories
# exp_files <- list.files(path = tpm_dir, pattern = paste0("'^exp_",in_rst_time,"z'"), full.names = TRUE, include.dirs = FALSE, recursive = FALSE)


#LOOP START with variable 'l' as index based on the zones available====
# l=1
for(l in 1: n_zones){
  #script to filter records in BAU TPM which violate the predefined rules=====
  # 0. Availability calculation====
  
  
  
  # 1. input ADOMIT temporarily====
  # read the expected lc table====
  # if(in_rst_time != project_time[1]){
  #   exp_tab <- read.csv(file = paste0(tpm_dir_prev,"/exp_",in_rst_time,"z",l,".csv"), stringsAsFactors = FALSE)
  # } else exp_tab <- data.frame()
  exp_tab <- data.frame()
  # read the tpm table====
  tpm_file <- read.csv(file = grep(pattern = paste0("0",l,".csv\\z"), tpm_flt, perl=TRUE, value = TRUE), stringsAsFactors = FALSE)
  tpm_file <- tpm_file[,1:3]
  # read the unchanged proportion table====
  unc_file <- read.csv(file = grep(pattern = paste0("_",l,".csv\\z"), tpm_pers, perl=TRUE, value=TRUE), stringsAsFactors = FALSE)
  unc_file <- unc_file[, 1:3]
  # 2. initial checking====
  # 2.1. ensure the total of the rates <= 1====
  tmp_tpm <- data.frame(rbind(tpm_file, unc_file), stringsAsFactors = FALSE)
  src_id <- unique(tmp_tpm$From.)
  fls_id <- numeric()
  for(t in 1: length(src_id)){
    if(t==1){
      rt_sum <- sum(tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"])
    }else rt_sum[t] <- sum(tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"])
    if(rt_sum[t] < 0 | rt_sum[t] > 1 & !isTRUE(all.equal(rt_sum[t],1))) fls_id <- c(fls_id, src_id[t])
  }
  ctrl_tab <- data.frame(cbind(src_id, rt_sum), stringsAsFactors=FALSE)
  # 2.2 recalculate the values when the total rate exceeds 1====
  if(length(fls_id)!=0){
    for(t in 1: length(fls_id)){
      tmp_tpm[tmp_tpm$From.==fls_id[t],"Rate"] <- tmp_tpm[tmp_tpm$From.==fls_id[t],"Rate"]/ctrl_tab[ctrl_tab$src_id==fls_id[t], "rt_sum"]
    }
  }
  
  # 3. Transformation of trajectories violating the rule into 0====
  # 3.1 No conversion into undisturbed forests====
  # except for undisturbed forest itself
  tmp_tpm[tmp_tpm$From.!= 1 & tmp_tpm$To.==1,"Rate" ] <- 0
  # 3.2 No cross conversion between forest types====
  id <- 1:7
  group <- c(1,1,1,2,2,3,3)
  forest_id <- data.frame(cbind(id, group),stringsAsFactors = FALSE)
  
  # for(t in 1: 3){
  #   tmp_tpm[tmp_tpm$From. %in% forest_id[forest_id$group==t,"id"] & tmp_tpm$To. %in% forest_id[forest_id$group!=t,"id"], "Rate"] <- 0
  # }
  
  init_sum <- sum(tmp_tpm$Rate)
  print(init_sum)
  # 3.3 No conversion from & to no data, waterbody, and fish ponds(0, 27, 26)====
  no_conv_id <- c(0, 29, 30, 31) #ADjusted
  # tmp_tpm[tmp_tpm$From. %in% no_conv_id & tmp_tpm$To. %in% no_conv_id & tmp_tpm$From. != tmp_tpm$To., "Rate"] <- 0
  
  # 3.4 No conversion from settlements (25)====
  # tmp_tpm[tmp_tpm$From. == 27 & tmp_tpm$From. != tmp_tpm$To., "Rate"] <- 0
  # tmp_tpm[tmp_tpm$From. == 27 & tmp_tpm$From. == tmp_tp m$To., "Rate"] <- 1
  # 3.5 No non-forest into forest restoration====
  nf_id <- src_id[! src_id %in% forest_id$id]
  # tmp_tpm[tmp_tpm$From. %in% nf_id & tmp_tpm$To. %in% forest_id$id, "Rate"] <- 0
  
  # 3.6 No conversion from coffee, oilpalm, rice, and other crops + Rubber (12)====
  exc_comm_id <- c(10, 12, 13, 20, 22) #ADCHECK  c(10, 13, 20, 22) #ADjusted
  tmp_tpm[tmp_tpm$From. %in% exc_comm_id & tmp_tpm$From.!=tmp_tpm$To., "Rate"] <- 0
  tmp_tpm[tmp_tpm$From. %in% exc_comm_id & tmp_tpm$From.==tmp_tpm$To., "Rate"] <- 1
  # 3.6.1 No further conversion into rubber====
  tmp_tpm[tmp_tpm$To. ==12 & tmp_tpm$From.!=tmp_tpm$To., "Rate"] <- 0
  # 3.7 No Conversion from mine into any other lc but shrub (23, 21:24)# ADjusted====
  tmp_tpm[tmp_tpm$From. ==25 & !tmp_tpm$To. %in% 23:26, "Rate"] <- 0
  
  
  # 4. Redistribution of the rates so that the total equals 1====
  no_value <- 0
  for(t in 1: length(src_id)){
    dv <- sum(tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"])
    if(dv != 0 ) {
      tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"] <- tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"]/dv
    }else {
      tmp_tpm[tmp_tpm$From.==src_id[t],"Rate"] <- 0
      no_value <- no_value +1
    }
  }
  
  #checkpoint1====
  if(length(src_id)-no_value != sum(tmp_tpm$Rate)){
    
    quit()
  }else print("CP1 reached")
  
  #     PHASE2====
  # 1. Zone availability calculation====
  zone_avail <- z_avail()
  names(zone_avail) <- c("ID_LC_T1", "area")
  
  # ADD calculate the diff between zone_avail with the exp_tab====
  
  zone_supp <- zone_avail#red
  names(zone_supp) <- c('ID', 'area') #red
  
  
  zone_matrix <- tmp_tpm[(tmp_tpm$From.!=tmp_tpm$To.)|(tmp_tpm$From.==tmp_tpm$To. & !tmp_tpm$From. %in% comm_lc_id), c("From.", "To.", "Rate")]
  
  # 1.1 Calculate the persistent commodities and imp_sources allocations====
  per_com_matrix <- tmp_tpm[tmp_tpm$From.==tmp_tpm$To. & tmp_tpm$From. %in% comm_lc_id, c("From.", "To.", "Rate")]
  per_com_matrix$rel_contribution <- 0
  per_com_matrix$raw_allocation <- 0
  per_com_matrix$real_alloc <- 0
  for(t in 1: nrow(per_com_matrix)){
    per_com_matrix[t, "real_alloc"] <- per_com_matrix[t, "Rate"] * zone_avail[zone_avail$ID_LC_T1==per_com_matrix[t,"From."],"area"]
    #substracting the allocated persistent areas from the available area====
    zone_supp[zone_supp$ID==per_com_matrix[t,"From."],"area"] <-zone_supp[zone_supp$ID==per_com_matrix[t,"From."],"area"]-per_com_matrix[t, "real_alloc"]
  }
  
  # 2. Relative contribution of trajectories in regards to the targeted requirements====
  
  for(h in 1: length(src_id)){
    zone_matrix[zone_matrix$From.==src_id[h], "rel_contribution"] <- zone_matrix[zone_matrix$From.==src_id[h], "Rate"] * zone_avail[zone_avail$ID_LC_T1==src_id[h], 'area']
  }
  
  for(h in 1: length(unique(zone_matrix$To.))){
    divder <- sum(zone_matrix[zone_matrix$To.==unique(zone_matrix$To.)[h], "rel_contribution"])
    if(divder!=0){
      zone_matrix[zone_matrix$To.==unique(zone_matrix$To.)[h], "rel_contribution"] <- zone_matrix[zone_matrix$To.==unique(zone_matrix$To.)[h], "rel_contribution"]/divder
    } else zone_matrix[zone_matrix$To.==unique(zone_matrix$To.)[h], "rel_contribution"] <- 0
  }
  
  #Raw allocation, neccessary?====
  # 3. separation of the zone matrix=====
  #in group in order to prioritize the commodity related trajectories: zone matrix is the one containing the relevant trajectories
  #group 1 zone_matrix
  less_matrix <- zone_matrix[! zone_matrix$To. %in% comm_lc_id,]
  
  zone_matrix <- zone_matrix[zone_matrix$To. %in% comm_lc_id,]
  
  # 4. zone demand definition====
  zone_dem <- eval(parse(text=paste0("data.frame(demand_file[, c('ID','z",l,"')])")))
  for(t in 1: nrow(zone_dem)){
    if(nrow(exp_tab)!=0){
      unf_prev_req <- exp_tab[exp_tab$ID_LC_T1==zone_dem[t, "ID"], "area"]- zone_avail[zone_avail$ID_LC_T1==zone_dem[t, "ID"], "area"]
    } else unf_prev_req <- 0
    # print(unf_prev_req)
    if(unf_prev_req < 0){
      print("unf_prev_req value is less than 0 (negative)")
    }
    zone_dem[t,paste0("z",l)] <- zone_dem[t,paste0("z",l)] + zone_supp[zone_supp$ID== zone_dem[t, "ID"], "area"] + unf_prev_req #because the persistent comm lc shoulnot be included
  }
  names(zone_dem)[2] <- "area"
  zone_dem_int <- zone_dem
  zone_matrix$raw_allocation <- 0
  
  
  # 5. Raw allocation based on the rel_contribution instead of rate to adjust the trajctories proportionally with the requirements====
  dem_in_matrix <- comm_lc_id #Red
  for(u in 1: length(dem_in_matrix)){
    zone_matrix$raw_allocation [which(zone_matrix$To.==dem_in_matrix[u])]<- zone_matrix[zone_matrix$To.==dem_in_matrix[u],'rel_contribution']*zone_dem[zone_dem$ID==dem_in_matrix[u],
                                                                                                                                                       'area']
  }
  
  # #checkpoint2----
  # if(sum(zone_matrix$rel_contribution) != length(comm_lc_id) | !isTRUE(all.equal(sum(zone_matrix$raw_allocation), sum(zone_dem[zone_dem$ID %in% comm_lc_id, "area"])))){
  #   quit()
  # }
  
  # 5.1. Sorting the zone matrix with the persistent comm_lc_id first====
  chg_z_matrix <- zone_matrix[zone_matrix$From.!=zone_matrix$To.,]
  chg_z_matrix <- chg_z_matrix[order(chg_z_matrix$Rate, decreasing = TRUE),]
  
  
  zone_matrix <- chg_z_matrix
  # zone_matrix <- data.frame(rbind(zone_matrix[zone_matrix$From.==zone_matrix$To.,], chg_z_matrix), stringsAsFactors = TRUE)
  
  
  
  # 6. Real allocation calculation====
  
  for(t in 1: nrow(zone_matrix)){
    if(zone_supp[zone_supp$ID==zone_matrix$From.[t],"area"] > 0) {
      zone_matrix$balance[t] <- zone_supp[zone_supp$ID==zone_matrix$From.[t],
                                          "area"]-zone_matrix$raw_allocation[t]
    }else zone_matrix$balance[t] <- 0-zone_matrix$raw_allocation[t]
    if(zone_matrix$balance[t] >= 0 & zone_matrix$To.[t] %in% dem_in_matrix){
      if(zone_matrix$raw_allocation[t] < zone_dem_int[zone_dem_int$ID==zone_matrix$To.[t],'area']){
        zone_matrix$real_alloc[t] <- zone_matrix$raw_allocation[t]
      } else if (zone_matrix$raw_allocation[t] >= zone_dem_int[zone_dem_int$ID==zone_matrix$To.[t],'area']){
        zone_matrix$real_alloc[t] <- zone_dem_int[zone_dem_int$ID==zone_matrix$To.[t],'area']
      }
    }else if(zone_matrix$balance[t]< 0 & zone_matrix$To.[t] %in% dem_in_matrix){
      zone_matrix$real_alloc[t] <- zone_matrix$raw_allocation[t]+zone_matrix$balance[t]
    } else zone_matrix$real_alloc[t] <- 0
    zone_supp[zone_supp$ID==zone_matrix$From.[t],"area"] <- zone_supp[zone_supp$ID==zone_matrix$From.[t],"area"]-zone_matrix$real_alloc[t]
    if(zone_matrix$To.[t] %in% dem_in_matrix) {
      zone_matrix$rem_demand [t]<- zone_dem_int[zone_dem_int$ID==zone_matrix$To.[t],'area']- zone_matrix$real_alloc[t]
      zone_dem_int[zone_dem_int$ID==zone_matrix$To.[t],'area'] <- zone_matrix$rem_demand[t]
    } else zone_matrix$rem_demand [t]<- 0
  }
  
  un_dem_ID <- zone_dem_int[zone_dem_int$area >0, 'ID']
  
  #checkpoint2====
  #principle: allocated+remaining demand = init_area+demand
  if(!isTRUE(all.equal(sum(c(zone_matrix$real_alloc, zone_dem_int$area)),sum(zone_dem$area))) & !isTRUE(all.equal(sum(c(sum(zone_matrix$real_alloc),
                                                                                                                        sum(zone_supp$area),
                                                                                                                        sum(per_com_matrix$real_alloc))),
                                                                                                                  sum(zone_avail$area)))){
    print("Checkpoint 2 has not successfully passed...")
    quit()
  }
  
  
  imp_lc_id <- c(comm_lc_id, 0,8, 19, 25, 27:31) #ADCHECKc(0, 29, 30, 31) #ADjusted #includes implausible sources: nodata, acacia plantation, tea, mining, settlement, fish pond, waterbody, cloud, and shadow
  # for(t in 1:length(comm_lc_id)){
  #   zone_supp[zone_supp$ID== comm_lc_id[t], "area"] <- zone_supp[zone_supp$ID== comm_lc_id[t], "area"] - zone_matrix[zone_matrix$From.==comm_lc_id[t] & zone_matrix$From.==zone_matrix$To., "real_alloc"]
  # }
  
  # 7. Alternative 1; maximize the available trajectories to facilitate the remaining requirements====
  if(length(un_dem_ID)!=0){
    poss_source_trj <- zone_matrix[zone_matrix$From. %in% zone_supp[zone_supp$area>0 & !zone_supp$ID %in% imp_lc_id,1] & zone_matrix$To. %in% un_dem_ID,c('From.', 'To.', 'Rate')]
    poss_source_trj <- poss_source_trj[order(poss_source_trj$Rate),]  
    if(nrow(poss_source_trj) != 0){
      for(t in 1: nrow(poss_source_trj)){
        poss_source_trj$rem_area [t]<- zone_supp[zone_supp$ID==poss_source_trj$From.[t],'area']
        if(poss_source_trj$rem_area[t]> zone_dem_int[zone_dem_int$ID==poss_source_trj$To.[t], 'area']){
          poss_source_trj$add_contrib[t] <- zone_dem_int[zone_dem_int$ID==poss_source_trj$To.[t], 'area']
        } else poss_source_trj$add_contrib[t] <- poss_source_trj$rem_area[t]
        zone_dem_int[zone_dem_int$ID==poss_source_trj$To.[t],'area' ] <- zone_dem_int[zone_dem_int$ID==poss_source_trj$To.[t],'area']-poss_source_trj$add_contrib[t]
        zone_supp[zone_supp$ID==poss_source_trj$From.[t],'area']<- zone_supp[zone_supp$ID==poss_source_trj$From.[t],'area']-poss_source_trj$add_contrib[t]
        zone_matrix[zone_matrix$From.== poss_source_trj$From.[t] & zone_matrix$To.== poss_source_trj$To.[t], 'real_alloc'] <- zone_matrix[zone_matrix$From.== poss_source_trj$From.[t] & zone_matrix$To.== poss_source_trj$To.[t],
                                                                                                                                          'real_alloc'] + poss_source_trj$add_contrib[t]
      }
      print(paste0("The analysis of zone",l," part 1 has been finished"))
    } else print(paste0("The analysis of zone",l," part 1 has been skipped"))
  }
  # print(paste0("The analysis of zone",l," part 1 has been finished"))
  un_dem_ID <- zone_dem_int[zone_dem_int$area >0, 'ID']
  
  if(!isTRUE(all.equal(sum(c(zone_matrix$real_alloc, zone_dem_int$area)),sum(zone_dem$area))) & !isTRUE(all.equal(sum(c(sum(zone_matrix$real_alloc),
                                                                                                                        sum(zone_supp$area),
                                                                                                                        sum(per_com_matrix$real_alloc))),
                                                                                                                  sum(zone_avail$area)))){
    print("Checkpoint 3 has not successfully passed...")
    quit()
  }
  # 8. Alternative 2:define new trajectories based on remaining POSSIBLE sources====
  #by possible it means: not the commodities itself and as described at the general rules earlier: 0, 23, 25, 26, 27 & comm_lc_id====
  
  
  if(length(zone_dem_int[zone_dem_int$area!=0,'area'])!=0) up_lim <- 1/2 * min(zone_dem_int[zone_dem_int$area!=0,'area'])
  loopno <- 1
  precomp_z_matrix <- zone_matrix[,c("From.","To.","real_alloc")]
  if(sum(zone_supp$area)!=0){
    repeat{
      if(length(un_dem_ID)!=0 & !identical(as.numeric(un_dem_ID), as.numeric(zone_supp[zone_supp$area > 0 & zone_supp$ID !=0, 'ID'][1]))){
        # 2nd alt. Seek the chance to define new trajectory based on the available land & remaining demand====
        repeat{
          #a. choose randomly the ID of unfacilitated demand
          if(length(un_dem_ID)!=1) c_demID <- sample(un_dem_ID, 1) else c_demID <- un_dem_ID
          if(zone_dem_int[zone_dem_int$ID == c_demID, 'area'] > 0 & c_demID !=0) break
        }
        repeat{#the area of demand to be addressed====
          if(zone_dem_int[zone_dem_int$ID == c_demID, 'area'] >=5) c_demArea <- sample(5: up_lim, 1) else c_demArea <- zone_dem_int[zone_dem_int$ID == c_demID, 'area']
          if(c_demArea <= zone_dem_int[zone_dem_int$ID == c_demID, 'area']) break
        }
        #the remaining supply to be chosen
        source_ID <- sample(zone_supp[zone_supp$area>0 & ! zone_supp$ID %in% imp_lc_id, 'ID'],1 )
        repeat{
          if(sum(zone_supp[zone_supp$ID %in% source_ID, 'area']) < c_demArea){
            source_ID <- c(source_ID, sample(zone_supp[zone_supp$area>0 & zone_supp$ID !=0 & !zone_supp$ID %in% source_ID,'ID'],1 ))
          } else break
        }
        if(length(source_ID) >1){
          area_comp <- numeric()
          rem_lastID_area <- sum(zone_supp[zone_supp$ID %in% source_ID, 'area']) - c_demArea
          for(s in 1: length(source_ID)){
            if(s != length(source_ID)){
              area_comp <- c(area_comp, zone_supp[zone_supp$ID==source_ID[s], 'area'])
              zone_supp[zone_supp$ID==source_ID[s], 'area'] <- 0
            } else{
              area_comp <- c(area_comp, (zone_supp[zone_supp$ID==source_ID[s],'area'] - rem_lastID_area))
              zone_supp[zone_supp$ID==source_ID[s],'area'] <- rem_lastID_area
            }
          }
          add_trj <- data.frame(cbind(source_ID, c_demID, area_comp))
          names(add_trj) <- names(precomp_z_matrix)
        } else {
          add_trj <- c(source_ID, c_demID, c_demArea)
          zone_supp[zone_supp$ID==source_ID, 'area'] <-zone_supp[zone_supp$ID==source_ID, 'area'] - c_demArea
        }
        
        precomp_z_matrix <- rbind(precomp_z_matrix, add_trj)
        zone_dem_int[zone_dem_int$ID == c_demID,'area']<- zone_dem_int[zone_dem_int$ID == c_demID,'area']-c_demArea
        loopno <- loopno+1
        if(sum(precomp_z_matrix$real_alloc, zone_dem_int$area) != 1143.429){
          print(loopno)
          print(sum(precomp_z_matrix$real_alloc, zone_dem_int$area))
          print(add_trj)
        }
        un_dem_ID <- zone_dem_int[zone_dem_int$area >0, 'ID']
      }else if(length(un_dem_ID) == 1 & length(zone_supp[zone_supp$area > 0 & zone_supp$ID !=0, 'ID']) == 1){
        if(identical(as.numeric(un_dem_ID), as.numeric(zone_supp[zone_supp$area > 0 & zone_supp$ID !=0, 'ID'][1]))) break
      }else break
    }
  }
  if(exists("source_ID")){
    #ADHERE
    precomp_melt <- melt(data = precomp_z_matrix, id.vars=c('From.','To.'), measure.vars=c('real_alloc'))
    precomp_melt <- dcast(data=precomp_melt, formula= From. + To. ~ variable, fun.aggregate = sum)
    names(precomp_melt) <- names(precomp_z_matrix)
    precomp_z_matrix <- precomp_melt
  }
  print(paste0("The analysis of zone",l," part 2 has been finished"))
  
  #precomp_z_matrix has substituted zone_matrix
  #checkpoint====
  
  
  #zone_supply
  #PHASE 3. OTHER LAND ALLOCATIONS====
  less_matrix$raw_allocation <- 0
  less_matrix$real_alloc <- 0
  ot_id <- unique(less_matrix$From.)
  un_ot_traj <- less_matrix[less_matrix$From. %in% zone_supp[zone_supp$area >0 , 'ID'],1:5]
  un_ot_traj <- un_ot_traj[order(un_ot_traj$Rate),]
  
  #should we define a new rate according to the remaining area YES! actual rate/total rate of the source: the total rate should sum up to 1
  # the trajectory without area left should NOT be OMITTED but given 0 as the real_alloc instead
  
  #the principle is as following: the remaining area divided proportionally based on the original rates
  red_id <- numeric()
  red_area <- numeric()
  for(t in 1: length(ot_id)){
    if(ot_id[t] %in% zone_supp[zone_supp$area >0 , 'ID']){
      dvdr <- sum(less_matrix[less_matrix$From.==ot_id[t],"Rate"])
      if(dvdr !=0){
        less_matrix[less_matrix$From.==ot_id[t], "real_alloc"] <- less_matrix[less_matrix$From.==ot_id[t],"Rate"]/dvdr * zone_supp[zone_supp$ID==ot_id[t],"area"]
        if(!isTRUE(all.equal(sum(less_matrix[less_matrix$From.==ot_id[t], "real_alloc"]), zone_supp[zone_supp$ID==ot_id[t],"area"]))){
          print("Checkpoint 5 has not been passed...")
          quit()
        }
      } else if(dvdr==0 & zone_supp[zone_supp$ID==ot_id[t],"area"]>0){
        red_id <- c(red_id, ot_id[t])
        red_area <- c(red_area, zone_supp[zone_supp$ID==ot_id[t],"area"])
      } else less_matrix[less_matrix$From.==ot_id[t], "real_alloc"] <- 0 #check if the sum equals 1
    } else less_matrix[less_matrix$From.==ot_id[t], "real_alloc"] <- 0
  }
  
  #redundant id handling====
  if(length(red_id)!=0){
    red_mat <- data.frame(cbind(red_id, red_area), stringsAsFactors = FALSE)
    sink_id <- unique(less_matrix[less_matrix$real_alloc !=0 & less_matrix$From. != less_matrix$To.,"To."])
    if(length(sink_id)!=0){
    for(r in 1: nrow(red_mat)){
      #ADHERE
      add_area <- red_mat[r,"red_area"]/length(sink_id)
      for(s in 1:length(sink_id)){
        if(length(less_matrix[less_matrix$From.==red_mat[r,"red_id"] & less_matrix$To. == sink_id[s],"real_alloc"])!=0){
          less_matrix[less_matrix$From.==red_mat[r,"red_id"] & less_matrix$To. == sink_id[s],"real_alloc"] <- add_area
        } else{
          less_matrix <- rbind(less_matrix, c(red_mat[r, "red_id"], sink_id[s], 0, 0, 0, add_area))
        }
      }
    }
    } else{
      for( r in 1: nrow(red_mat)){
        less_matrix <- rbind(less_matrix, c(red_mat[r, "red_id"], red_mat[r, "red_id"], 0, 0, 0, red_mat[r, "red_area"]))
      }
    }
  }
  
  
  
  # PHASE 4. check the area consistency====
  if(!isTRUE(all.equal(sum(c(sum(less_matrix$real_alloc), sum(precomp_z_matrix$real_alloc), sum(per_com_matrix$real_alloc))), sum(zone_avail$area)))){
    print("Area inconsistence has been detected...")
    quit()
  }
  print(paste0("The analysis of zone",l," part 3 has been finished. Final analysis phase is running..."))
  # PHASE 5. Check the requirement fulfillments====
  kept <- c("From.","To.", "real_alloc")
  fin_land_area <- data.frame(rbind(less_matrix[,kept], precomp_z_matrix[, kept], per_com_matrix[,kept]), stringsAsFactors=FALSE)
  eval(parse(text=paste0("final_lc_z",l," <- melt(data = fin_land_area, id.vars=c('To.'), measure.vars=c('real_alloc'))")))
  eval(parse(text=paste0("final_lc_z",l," <- dcast(data = final_lc_z",l,", formula = To. ~ variable, fun.aggregate = sum)")))
  eval(parse(text = paste0("final_lc_z",l,"$ID_Z <-", l)))
  eval(parse(text=paste0("final_lc_z",l,"<- final_lc_z",l,"[,c(3,1,2)]")))
  eval(parse(text=paste0("names(final_lc_z",l,")  <- c('ID_Z', names(zone_avail))")))
  eval(parse(text=paste0("write.csv(final_lc_z",l,",file='",tpm_dir,"/exp_", project_time[which(project_time==in_rst_time)+1],
                         "z",l,".csv',row.names=FALSE)")))
  #   2 Normalization of the rates by dividing the real alloc with the available at initial=====
  fin_land_area$Rate <- 0
  for(t in 1: length(src_id)){
    div <- zone_avail[zone_avail$ID_LC_T1==src_id[t], "area"]
    if(div!=0){
      fin_land_area[fin_land_area$From.==src_id[t], "Rate"] <- fin_land_area[fin_land_area$From.==src_id[t], "real_alloc"]/div
    }else fin_land_area[fin_land_area$From.==src_id[t], "Rate"] <- 0
  }
  
  #checkpoint 6====
  # if the Rate !=0, the real_alloc/Rate should return the area of zone_avail
  l_av_id <- zone_avail[zone_avail$area !=0, "ID_LC_T1"]
  for(t in 1: length(l_av_id)){
    if(!isTRUE(all.equal(unique(round(fin_land_area[fin_land_area$From.==l_av_id[t] & fin_land_area$Rate !=0, "real_alloc"]/fin_land_area[fin_land_area$From.==l_av_id[t] & fin_land_area$Rate !=0, "Rate"],
                                      digits = 0)), zone_avail[zone_avail$ID_LC_T1==l_av_id[t], "area"]))){
      print(paste0("Unmatched record at id: ",l_av_id[t],"..."))
      quit()
    }
  }
  
  
  # X.3. Removal of unchanged trajectories =====
  fin_tpm <- fin_land_area[fin_land_area$From.!= fin_land_area$To.,c("From.", "To.", "Rate")]
  names(fin_tpm) <- c("From*", "To*", "Rate")
  
  #
  
  
  # X.4. Save the final resulting table====
  if(l < 10){
    write.csv(fin_tpm, file = paste0(tpm_dir,"/Single_step_Plan_00000",l,".csv"), row.names = FALSE, quote = FALSE)
  } else write.csv(fin_tpm, file = paste0(tpm_dir,"/Single_step_Plan_0000",l,".csv"), row.names = FALSE, quote = FALSE)
  
  #PHASE FINAL====
  zone_de <- eval(parse(text=paste0("data.frame(demand_file[, c('ID','z",l,"')], stringsAsFactors= FALSE)")))
  names(zone_de)[2] <- "area"
  
  eval(parse(text=paste0("act_real <- final_lc_z",l,"[final_lc_z",l,"$ID_LC_T1 %in% zone_de$ID,]")))
  
  for(t in 1: nrow(zone_de)){
    zone_de[t,"area"]<- zone_de[t, "area"] + zone_avail[zone_avail$ID_LC_T1==zone_de[t,1], "area"] + unf_prev_req
    check_tab[l,t] <- all.equal(zone_de[t, "area"], act_real[act_real$ID_LC_T1==zone_de$ID[t],"area"])
  }#positive values mean theat some demand has not yet been facilitated
  
  #summarize the total areas in the whole zones to obtain the expected total area====
  if(l==1){
    total_lc <- eval(parse(text=paste0("final_lc_z",l,"[,c('ID_LC_T1', 'area')]")))
  } else{
    total_lc$area <- total_lc$area + eval(parse(text=paste0("final_lc_z",l,"$area")))
  }
  
}



# 
#   
#   
#   
#   #LATERON-====
# 
# 
# if(nrow(un_ot_traj)!=0){
#   for(t in 1:nrow(un_ot_traj)){
#     
#     un_ot_traj$raw_allocation[t] <- un_ot_traj$Rate[t] * zone_avail[zone_avail$ID_LC_T1==un_ot_traj$From.[t], 'area']
#   }
#   zone_dem_int2 <- data.frame(unique(un_ot_traj$To.))
#   for(t in 1: length(unique(un_ot_traj$To.))){
#     zone_dem_int2$area [t]<- sum(un_ot_traj[un_ot_traj$To.== unique(un_ot_traj$To.)[t], 'raw_allocation'])
#   }
#   names(zone_dem_int2)[1] <- 'ID'
#   for(t in 1:nrow(un_ot_traj)){
#     if(zone_supp[zone_supp$ID==un_ot_traj$From.[t],"area"] > 0) {
#       un_ot_traj$balance[t] <- zone_supp[zone_supp$ID==un_ot_traj$From.[t],
#                                          "area"]-un_ot_traj$raw_allocation[t]
#     }else un_ot_traj$balance[t] <- 0-un_ot_traj$raw_allocation[t]
#     if(un_ot_traj$balance[t] >= 0){
#       un_ot_traj$real_alloc[t] <- un_ot_traj$raw_allocation[t]
#     } else {
#       un_ot_traj$real_alloc[t] <- un_ot_traj$raw_allocation[t]+un_ot_traj$balance[t]
#     } ##zone_matrix$real_alloc[t] <- 0
#     if(un_ot_traj$real_alloc[t] > zone_dem_int2[zone_dem_int2$ID==un_ot_traj$To.[t],'area'])un_ot_traj$real_alloc[t] <- zone_dem_int2[zone_dem_int2$ID==un_ot_traj$To.[t],'area']
#     zone_supp[zone_supp$ID==un_ot_traj$From.[t],"area"] <- zone_supp[zone_supp$ID==un_ot_traj$From.[t],"area"]-un_ot_traj$real_alloc[t]
#     un_ot_traj$rem_demand [t]<- zone_dem_int2[zone_dem_int2$ID==un_ot_traj$To.[t],'area']- un_ot_traj$real_alloc[t]
#     zone_dem_int2[zone_dem_int2$ID==un_ot_traj$To.[t],'area'] <- un_ot_traj$rem_demand[t]
#     #if theres inadequate supply to accomodate such demand, the highest rate should be prioritized
#   }
#   unTraj_precomp <- un_ot_traj[un_ot_traj$real_alloc >0,c('From.', 'To.', 'real_alloc')]
#   names(unTraj_precomp) <- names(precomp_z_matrix)
#   precomp_z_matrix <- rbind(precomp_z_matrix, unTraj_precomp)
# }
# print(paste0("The analysis of zone",l," part 3 has been finished. Final analysis phase is running..."))
# 
# 
# # X.2 Normalization of the rates by dividing the real alloc with the available at initial=====
# 
# 
# # X.3. Removal of unchanged trajectories =====
# f_tpm_tab <- tmp_tpm

# X.4. Save the final resulting table====

# 3. 

#NOTES:
# should the rules be applied at this phase?====YES!!
# for commodities which may undergo changes, only the difference between the initial supply and the real allocation as defined at zone_matrix
# may be allocated for other uses