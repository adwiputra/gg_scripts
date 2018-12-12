# 25/09/2018
# AD
# based on allocate_expansion.R from IR_Muba
# allocate the expansions
library(raster)
# INPUTS====
#functions
loadRaster <- function(url, file){
  raster(paste(url, file, sep=''))
}
tabl.dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/"
urlAddressRaster <- "D:/GG_Jambi/General/data/table/land_requirements/raster_dir/"
# load the raster layer
pu_layer <- raster(paste0(urlAddressRaster, "pu_final.tif"))
lc_15 <- raster(paste0(urlAddressRaster, "lc2015_jambi.tif"))

# load the lookup table
lc_indo_lu <- read.csv(paste0(tabl.dir, "lut_lu.csv"), stringsAsFactors = FALSE)
pu_lu <- read.csv(paste0(tabl.dir, "planning_unit.csv"), stringsAsFactors = FALSE)


# zonal statistic====
zone_calc <- function(r, lc_interest = c("Padi", "Sawah irigasi"), class_lu = lc_indo_lu, z = pu_layer, z_lu = pu_lu){
  # blanko
  z_lu <- z_lu[, c(1,2)]
  names(z_lu) <- c("z_vl", "z_chr")
  # subset lookup table
  class_lu <- class_lu[, c(1,2)]
  names(class_lu) <- c("vl", "cl_chr")
  total_cmd <- character()
  for(l in 1: length(lc_interest)){
    # eval(parse(text=paste0("z_lu$lc_i", l, " <- 0"))) # container column
    # reclassify "r" so that the value of interest has value 1 while others have value 0
    # generate reclassification scheme
    rc_table <- data.frame(pre_vl = class_lu$vl, new_val = 0, stringsAsFactors = FALSE)
    rc_table[rc_table$pre_vl == class_lu[class_lu$cl_chr == lc_interest[l], "vl"], "new_val"] <- 1
    # anticipate the nodata value
    no_val <- setdiff(unique(values(r)), rc_table$pre_vl)
    if(length(no_val) !=0){
      # add single additional rc_table row for each no_val
      for(n in 1: length(no_val)){
        add_rc <- data.frame(pre_vl = no_val[n], new_val = NA, stringsAsFactors = FALSE)
        rc_table <- rbind(rc_table, add_rc)
      }
    }    
    rc_table <- as.matrix.data.frame(rc_table)
    # reclass
    r_rec <- reclassify(r, rc_table, right = NA)
    # calculate zonal statistic on "r_rec"
    z_count <- zonal(r_rec, z= z, fun="sum")
    z_count <- as.data.frame(z_count) # automatically assign the names of "zone" and "sum" to the resulting data.frame
    z_lu <- merge(z_lu, z_count, by.x = "z_vl", by.y = "zone", all.x = TRUE)
    names(z_lu)[2*(1+l)-1] <- paste0("Area_", gsub(" ", "_", lc_interest[l]))
    # add calculation on proportion across planning units
    eval(parse(text = paste0("z_lu$Prop_", gsub(" ", "_", lc_interest[l]), " <- z_lu$Area_", gsub(" ", "_", lc_interest[l]), "/sum(z_lu$Area_", gsub(" ", "_", lc_interest[l]), ")")))
    total_cmd <- c(total_cmd, paste0("z_lu$", paste0("Area_", gsub(" ", "_", lc_interest[l]))))
  }
  # return a data frame containing the area of lc_interest (cell count) for each zone
  total_cmd <- paste0(total_cmd, collapse = " + ")
  z_lu$Total <- eval(parse(text=paste0(total_cmd)))
  return(z_lu)
}
# calculate the proportion AD250918====
prp.kopi <- zone_calc(r = lc_15, lc_interest = grep("kopi", lc_indo_lu$LEGEND, value = TRUE))
prp.sawit <- zone_calc(r = lc_15, lc_interest = grep("sawit", lc_indo_lu$LEGEND, value = TRUE))
prp.Sawah <- zone_calc(r = lc_15, lc_interest = grep("Sawah", lc_indo_lu$LEGEND, value = TRUE))
prp.lain <- zone_calc(r = lc_15, lc_interest = grep("lain", lc_indo_lu$LEGEND, value = TRUE))
# save tables
kWord <- c("kopi", "sawit", "Sawah", "lain")
for(k in 1: length(kWord)){
  eval(parse(text= paste0("write.csv(prp.", kWord[k], ", '", tabl.dir, "prop_", kWord[k], ".csv', row.names = FALSE)")))
}


# rice
padi_zone_all <- zone_calc(r = lc_15)
# padi_zone_all$pctg <- 100*padi_zone_all$Total/sum(padi_zone_all$Total)
# rubber
rubber_zone_all <- zone_calc(r = lc_15, lc_interest = grep("karet", lc_indo_lu$Ver3, value = TRUE))
# oil palm
op_zone_all <- zone_calc(r = lc_15, lc_interest = grep("sawit", lc_indo_lu$Ver3, value = TRUE))


# allocate the increase


# misc====
# A. project linearly the area of the plantations area based on the data of expected 2017-2022 area
# data frame
plant_area <- data.frame(year = c(2017:2022), area_ha = c(498995, 499161, 499211, 581646, 499851, 499955), stringsAsFactors = FALSE)
nyear <- data.frame(year= 2030)
# simple linear reg model
plant_area.lm <- lm(area_ha ~ year, data = plant_area)
# solve the formula
# plant_30_area <- predict(plant_area.lm, nyear) 
coeff <- coefficients(plant_area.lm)
nyear = 2030
area_plantation_30 <- coeff[1] + coeff[2]*nyear
area_plantation_14 <- coeff[1] + coeff[2]*2014
# adjustment according to the actual spatial data
area_plantation_30 <- (sum(rubber_zone_all$Total)+sum(op_zone_all$Total)) * (1+ ((area_plantation_30-area_plantation_14)/area_plantation_14))
# B. rice
rice_prod_plan <- data.frame(yr = 2016:2022, prod_ton = c(271284, 474069, 502939, 533551, 566085, 600543, 637126), prc_tonha = c(3.999, 5.8, 5.974, 6.153, 6.338, 6.528, 6.724))
rice_prod_plan$est_area <- rice_prod_plan$prod_ton/rice_prod_plan$prc_tonha
rice_area_lm <- lm(est_area ~ yr, data= rice_prod_plan)
coeff <- coefficients(rice_area_lm)
# nyear = 2030
area_rice_30 <- coeff[1] + coeff[2]*nyear
area_rice_14 <- coeff[1] + coeff[2]*2014
# adjustment according to the actual spatial data
area_rice_30 <- sum(padi_zone_all$Total)* (1+((area_rice_30-area_rice_14)/area_rice_14))
# <- predict.lm(rice_area_lm, nyear)


# C. Calculating the adjusted total area expected in 2030====
# each commodity has the expected area explicitly mentioned (directly or indirectly based on RPJMD) to be substracted with the 2014 existing area,
# then projected into rate of increase.

# functions definition
# 1 rate area expansion or shrink based on statistical annotations
rate_area <- function(area_t1, area_t2, t1, t2){
  # cnst = constant
  cnst <- area_t2/area_t1
  cnst <- (cnst)^(1/(t2-t1))
  # return rate in percentage
  pct_chg <- (cnst - 1) * 100
  return(pct_chg)
}
# 2 expected area at t2 given actual area_t1
act_area_exp <- function(t1, t2, pc_change = 0, act_t1){
  # return the area at t2 based on the act_t1 value
  # pc_change is the value returned by the rate_area function
  act_t2 <- (1+pc_change/100)^(t2-t1) * act_t1
  return(act_t2)
}

# 3 distribute across the relevant lc class for each time step
# to be run per category: rice field expansion and plantations expansion
dist_cover <- function(t1_rea = raster(), rea_lookup = lc_indo_lu, ar_t2, l_compo = character(), step = numeric(), t1, t2){
  # l_compo: the classes which constitue the area
  # ar_t2: area to be spread across the classes listed in l_compo; the area at t2; calculated a priori from the linear regression
  # [NOT USED] rt_x = rate of expansion
  # step: multi-steps of the input period
  # pu_rel: planning unit class(es) that is appropriate to hold the expansion
  # Steps====
  # generate the data.frame
  # define the number of column based on the 'step' variable
  colname_df <- character()
  for(s in 1: step){
    colname_df <- c(colname_df, paste0("step", s, " = 0"))
  }
  eval(parse(text=paste0("cov_area_dist <- data.frame(lc_cs = l_compo,", paste0(colname_df, collapse = ","), ", stringsAsFactors = FALSE)")))
  # Generate the column which contains the cell value of the l_compo based on information in 'lc_indo_lu'
  names(rea_lookup)[1] = "ID"
  names(rea_lookup)[2] = "lc_cs"
  cov_area_dist <- merge(cov_area_dist, rea_lookup[, c(1,2)], by = "lc_cs", all.x = TRUE)
  cov_area_dist <- cov_area_dist[, c(ncol(cov_area_dist), 1: (ncol(cov_area_dist)-1))]
  # write the area of the l_compo at t1 as the basis to determine the ratio of allocation of ar_t2
  # freq of t1_rea
  area_t1map <- data.frame(freq(t1_rea), stringsAsFactors = FALSE)
  names(area_t1map) <- c("vl", "step0")
  cov_area_dist <- merge(cov_area_dist, area_t1map, by.x = "ID", by.y = "vl", all.x = TRUE)
  cov_area_dist <- cov_area_dist[, c(1, 2, ncol(cov_area_dist), 3: (ncol(cov_area_dist)-1))]
  # calculate the rate of expansi0n
  rt_x <- rate_area(area_t1 = sum(cov_area_dist$step0), area_t2 = ar_t2, t1 = t1, t2 = t2)
  # apply the expansion rate to the 'cov_area_dist' table
  d_y <- (t2-t1)/step
  for(r in 1:nrow(cov_area_dist)){
    for(s in 1:step){
      cov_area_dist[r, paste0("step", s)] <- act_area_exp(t1 = t1, t2 = t1+s*d_y, pc_change = rt_x, act_t1 = cov_area_dist[r, "step0"])
      
    }
  }
  # readjust the names of the column
  colname_df <- character()
  for(s in 1:step){
    colname_df <- c(colname_df, paste0(as.character(t1 + (s-1)*d_y), "-", as.character(t1+ s*d_y)))
  }
  names(cov_area_dist) <- c("ID", "Cov_cl", "init_area", colname_df)
  
  # final result
  return(cov_area_dist)
}


# calculate the proportion of the area across the planning units for each lc_classes in analysis
# in association with the zone_calc function
# z.lclass_prop <- function(lclass = character(), )
dis.on_prop <- function(area_dis = numeric(), lc_val= numeric(), prp_table = data.frame(), step= numeric(), t1=t1, t2=t2, look_lc = lc_indo_lu){
  # area_dis is a vector with the same length as step indicating the area increase per step defined;
  # routines of the function: read the inputs, table of proportion suitability should be read (logical); otherwise, read the proportion of lc across lu (input),
  # loop per step;
  # a priori defines the columns of the data frames to be returned
  z_columns <- prp_table[, 1] # the pu_id
  z_columns <- z_columns[order(z_columns)]
  z_columns <- paste0("Z_", z_columns)
  z_columns <- paste0(z_columns, "= 0")
  # blanko data.frame
  df_st <- eval(parse(text=paste0("data.frame( ID_lc = look_lc[,1], lc =  look_lc[,2], ", paste0(z_columns, collapse = ","), ", stringsAsFactors = FALSE)")))
  # match the pu_id with the right column
  prp_table <- prp_table[, c(1,3)]
  names(prp_table) <- c("id_z", "prop")
  non_zero_pu <- prp_table[prp_table$prop > 0, "id_z"]
  # assign the proportion value to the column
  for(n in 1:length(non_zero_pu)){
    eval(parse(text=paste0("df_st[df_st$ID_lc == lc_val, 'Z_", non_zero_pu[n], "'] <- prp_table[prp_table$id_z ==", non_zero_pu[n], ", 'prop']")))
  }
  df_list <- list()
  for(s in 1: step){
    blanko <- df_st
    blanko[blanko$ID_lc == lc_val, 3: ncol(blanko)]<- blanko[blanko$ID_lc == lc_val,3: ncol(blanko)]*area_dis[s]
    #    read the value of a certain land cover increase in the corresponding step
    #    generate one data frame for each step; land cover as rows and planning units as columns (as alias "Z_'punumber'")
    df_list[[s]] <- blanko
  }
  return(df_list)
}

# 
# 
#    multiply the value with relevant proportion
# store all of the generated data frame as a list
# return the list

# generate the input for function dis.on_prop====
prp.pu_rice <- read.csv("D:/IR_Muba/spatial/Raster_AD/rice_special/suitable_rice_pu.csv", stringsAsFactors = FALSE)
names(prp.pu_rice)<- c("ID", "Ct", "prop")
prp.pu_rice <- merge(pu_lu[, c(1:2)], prp.pu_rice[, c("ID", "prop")], by.x = "Value", by.y="ID", all.x =TRUE) # input as whole
prp.pu_rice[is.na(prp.pu_rice$prop), "prop"] <- 0# to be used only for the sawah lebak
prp.pu_largeoilpalm <- zone_calc(r = lc_15, lc_interest = grep("besar", lc_indo_lu$Ver3, value = TRUE))# input column 1,2,4
prp.pu_smalloilpalm <- zone_calc(r = lc_15, lc_interest = grep("kecil", lc_indo_lu$Ver3, value = TRUE))# input column 1,2,4
prp.pu_rubber <- zone_calc(r = lc_15, lc_interest = grep("Monokultur", lc_indo_lu$Ver3, value = TRUE)) # Mono only # input column 1,2,4
# 4 combine the data frames of planned expansion as single data frame per period of time
# args pu_rel = character()
# based on the planning unit area; existence of the l_comm in the area
# Rice: overlay the area of tanaman pangan with the new planning unit: overlapping planning unit is to be targeted: tanaman pangan, non gambut lindung
# Rubber: Non HGU, Non forest, non gambut
# Oilpalm: HGU (large scale), Non HGU (small scale), gambut budidaya dan non gambut
# INPUT====
# 1. function will receive the input of the combined tables returned from previous function
# 2. another input would be a character list with length that match the number of rows of the table
#   - each part of the list contains the character vector which defines the planning unit to where the allocation will take place
# OUTPUT====
# list of table
# each represents the additional area for each period step across planning units (column "Z_xx")

# execution of the functions
# run line 70-93 >> get the area_rice_30 and area_plantation_30
rice.est <- dist_cover(t1_rea = lc_15, rea_lookup = lc_indo_lu, ar_t2 = area_rice_30, l_compo = c("Padi", "Sawah irigasi"), step = 4, t1 = 2014, t2=2030)
plantation.est <- dist_cover(t1_rea = lc_15, rea_lookup = lc_indo_lu, ar_t2 = area_plantation_30, l_compo = c("Agroforestri karet", "Monokultur karet", "Kelapa sawit skala besar", "Kelapa sawit skala kecil"), step = 4, t1 = 2014, t2=2030)
allcomm.est <- rbind(rice.est, plantation.est)

# execute the function dis.on_prop in a loop
comp_list <- list()
compile.prptab <- list(prp.pu_rice, prp.pu_rice, prp.pu_rubber[, c(1,2,4)], prp.pu_rubber[, c(1,2,4)], prp.pu_largeoilpalm[, c(1,2,4)], prp.pu_smalloilpalm[, c(1,2,4)])
step=4
for(r in 1: nrow(allcomm.est)){
  comp_list[[r]] <- dis.on_prop(area_dis = as.numeric(allcomm.est[r, 4:(4+step-1)]-allcomm.est[r, "init_area"]), lc_val = allcomm.est[r, "ID"], prp_table = compile.prptab[[r]], step = 4, t1 = 2014, t2= 2030)
}
# merge the result into four tables
# for lc_class
step=4
d_y=4
for(s in 1: step){
  for(l in 1: length(comp_list)){
    if(l == 1) res_ts <- comp_list[[l]][[s]] else{
      add_rec <- comp_list[[l]][[s]]
      res_ts[, 3:ncol(res_ts)] <- res_ts[, 3:ncol(res_ts)] +  add_rec[, 3:ncol(add_rec)]
    }
  }
  write.csv(res_ts, paste0(tabl.dir, (2014+(d_y*(s-1))), "_", (2014+(d_y*s)), "_demand.csv"), row.names = FALSE)
}
