# Original from: H:\Sentinel_Landscape\Process\script\sent_lscp.R

#Species Distribution Modeling with MaxEnt
#to be included within LUMENS' QUES-B Sub-Module

#Outlines:
#memory allocation####
options(java.parameters = "-Xmx8g")
# AE comment1 : This is quite dangerous if available memory is notsufficient. My suggestion is to find a way to
# detect available memory and automatically allocate max 75% of that into JVM

#0. Setting the suitable working directory and generating output directory####
library(dismo)
library(spatial.tools) # to adjust overlayability of the predictor/covariate layers
library(rtf)
library(raster)
library(maptools)
library(ENMeval)
library(utils)

#time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#READ LUMENS LOG FILE
#user_temp_folder<-Sys.getenv("TEMP")
#if(user_temp_folder=="") {
#  user_temp_folder<-Sys.getenv("TMP")
#}
#LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
#log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
#proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
#load(proj.file)

#defining a function to create a table containing the list of raster files within the .Rdata####
ls_raster <- function(){
  all_file <- as.data.frame(ls(envir = globalenv()))
  all_file$type <- NA
  colnames(all_file)[1] <- "vars"
  for(i in 1: nrow(all_file)){
    eval(parse(text=(paste("all_file[",i,", 'type'] <- class(",all_file$vars[i],")", sep = ""))))
  }
  rasterfiles <- as.data.frame(all_file[grep(pattern = "^Raster",x = all_file$type, ignore.case = F),1])
  rasterfiles$Notes <- NA
  colnames(rasterfiles)[1]<- "var_name"
  for(i in 1: nrow(rasterfiles)){
    rasterfiles[i,2] <- eval(parse(text=paste("names(",rasterfiles[i,1],")", sep = "")))
  }
  return(rasterfiles)
}

pt_correct <- function (xy_table, ekstent){
  #Removing records outside the layer extent
  xy_table <- xy_table[xy_table[,1]>ekstent@xmin & xy_table[,1] < ekstent@xmax & xy_table[,2] < ekstent@ymax & xy_table[,2] > ekstent@ymin,]
  return (xy_table)
}
#wd####
#preques_folder<-paste("PreQUES_analysis_", T1,"_",T2,"_",PreQUES.index,sep="")
#quesb_dir<-paste(dirname(proj.file),"/QUES/QUES-B/", sep="")
#quesb_dir <- "D:/Sentinel_Landscape/Process/"
quesb_dir <- choose.dir()
sdm_dir <- "SDMod"#setwd(result_dir)
#dir.create(preques_folder)
#output directory
#result_dir<-paste(result_dir,preques_folder, sep='')
#setwd(result_dir)



# AE comment1 : dismo dan ENMeval belum termasuk dalam installer LUMENS yang sekarang. Perlu diinventarisasi
# apa saja dependency dari masing-masing package ini. I.e :   rJava (??? 0.5-0), rgdal, rgeos, XML, ROCR, 
# deldir, gstat, randomForest, kernlab, maptools, jsonlite, gbm , supaya bisa di masukan ke installer

# AE comment2 : perlu dicari cara bagaimana instalasi otomatis maxent.jar ke folder "dismo/java"


#user defined variables####
#WD is the directory containing the presence data (.csv) and directory of the covariate layers.####
setwd(quesb_dir)
if(!dir.exists(sdm_dir))dir.create(sdm_dir)
setwd(sdm_dir)

#Checking QUES-B SDM logfile####
log <- "QB_SDM_log.txt"
if(file.exists(log)){
  qb_log <- read.table(log, header = T, stringsAsFactors = F)
  sdm_rep <- qb_log[nrow(qb_log),1]
  sdm_id <- sdm_rep+1
  addlog <- as.data.frame(cbind(sdm_id, as.character(Sys.time()), "NA", "NA"))
  colnames(addlog) <- colnames(qb_log)
  qb_log <- as.data.frame(rbind(qb_log, addlog))}else{sdm_id =1
  qb_log <- as.data.frame(cbind(sdm_id, as.character(Sys.time())))
  qb_log$End <- "NA"
  qb_log$odir <- "NA"
  colnames(qb_log)<- c("Repetition", "Start", "End", "out_dir")
  }
out <- paste("qb_sdm_ot_",sdm_id, sep = "")###INPUT; index showing how many runs have been made should be defined
qb_log[sdm_id,"out_dir"] <- out
write.table(qb_log, file = log, row.names = F)


#INPUT####
pres_dat <- readShapePoints(file.choose()) #nama csv koordinat nipah

#[REV] pres_dat <- readShapePoints(file.choose())


#cov_dir <- "Validation" #nama folder validasi

#cov_dir <- "J:/FTA/africa"
cov_dir <- "D:/GG_Jambi/Ecosystem/suitability/from_SP/compiled"
cov_dir <- gsub("\\","/", cov_dir, fixed="TRUE")

ex_dir = ""
o_pred_filename<-"predict_map.tiff"
generate_map <- FALSE #binary T/F
# bg_cts <- 13000
fold <- 10
fin_eval_prop <- 30 #proportion (in %) of presence data and background points to be set aside for the final model evaluation
###INPUT
#mask <- raster("lc_icraf_utm.tif") #basemap to be input by the user ###INPUT: RASTER


auc_thres <- 0.6
#extra_dir <- "indo" #for extrapolation purpose, the directory is not the same as the cov_dir


#setwd(wd)
# AE comment 5: sama dengan comment3, wd saat ini ditentukan secara otomatis did alam RData

if(!dir.exists(out)) dir.create(out)

#A. Data Input####
#Import 1. Presence Data (points) & 2. Covariate Layers (raster) from source file(s)
#A1
#t_pres <- read.csv(pres_dat, header = T) #[REV] remove the command
# AE comment 6: perlu beberapa line script tambahan untuk mengecek apakah format data sudah sesuai dengan 
# yang diinginkan oleh script. Jika tidak maka ada dua opsi: (1) eksekusi script berhenti dan error
# message ditampilkan atau (2) script berhenti sementara, error message ditampilkan,
# user diberi kesempatan mengedit format data, dan kemudian pengecekan ulang dilakukan
#latlon_pick <- as.data.frame(colnames(t_pres))
#latlon_pick$lon <- 0
#latlon_pick$lat <- 0
##repeat{
#  latlon_pick<-edit(latlon_pick)
#  if(sum(latlon_pick$lon)==1 & sum(latlon_pick$lat)==1){ 
#    break
#  } else break #with warning message telling user to edit the input
#}


#THE MOST IMPORTANT THING IS TO ENSURE THAT THE CSV FILE CONTAINS COORDINATES OF SPECIES'PRESENCE LOCALITIES: indirect approach by letting the user define the columns
#containing the information regarding x and y coordinates of the species localities.
#ALSO NEED TO CHECK WHETHER THE COORDINATES ARE WITHIN THE STUDIED AREA WITH THE SAME PROJECTION SYSTEM.(later after the covariate layers have been loaded)



# assigning the total presence record into a variable called 'tpres'
pres <- as.data.frame(cbind(pres_dat$coords.x1, pres_dat$coords.x2))

colnames(pres) <- c("longitude","latitude") #standardized the column names
#excluding the first column containing the species (object) being modeled
# AE comment 7: field yang dipilih sebagai field koordinata x dan y harus berasal input user

#[REV] Replace command 119-143 w/ :####
# pres <- as.data.frame(cbind(pres_dat$coords.x1, pres_dat$coords.x2))

#divide the available presence localities into k number of groups randomly
#set.seed(0)
group <- kfold(pres, k = fold)
#Define some of the presence data as training dataset, while the 20% as test dataset

#Covariate Data Input
#av_lyr <- list.files(cov_dir,pattern = ".asc", full.names = T) # only take ascii raster files into account
if(cov_dir!=""){
  av_lyr <- list.files(cov_dir,pattern = ".tif", full.names = T) # only take .tif raster files into account
  av_lyr <- grep(".tif\\z", av_lyr, perl = T, value = T) #specifically extract only the file names that end with pattern ".tif"
  # AE comment 8: sama seperti comment3, list of files ditentukan melalui RData
  
  #====Selecting which layer to be excluded and which contains categorical variables
  av_data <- as.data.frame(av_lyr)
  av_data$Notes <- "Souce: External dir."
  
  #adding the existing raster files inside the .Rdata####
  #in_dat <- ls_raster()
  #colnames(in_dat) <- colnames(av_data)
  #av_data <- rbind(av_data, in_dat) #the displayed list is the raster names instead of the variable names
}else {
  av_data <- ls_raster()
  colnames(av_data) <- c("av_lyr", "Notes")
}


av_data$include <- 1
av_data$cat <- 0
n <- nrow(av_data)

#(290216) AD: av_data is the variable to be edited when user wants to change model parameterization after assessing the generated model performance####
#repeat{
  repeat{
    av_data<-edit(av_data)
    if(sum(av_data$include)!=0){
      break
    }
  }
  
  # AE comment 9: from user perspective, inclusion is always easier than exclusion, instead of choosing which layer to exclude
  # I think it is much easier for user to choose which layer to be included
  
  
  av_lyr <- as.data.frame(av_data[av_data$include==1,])
  av_lyr$id <- 1:nrow(av_lyr)
  cat_id <- av_lyr[av_lyr$cat==1,'id']
  av_lyr$call<- NA
  row_ext <- grep (".tif\\z", av_lyr$av_lyr, perl = T)
  for(i in 1: nrow(av_lyr)){
    if(i %in% row_ext) av_lyr$call[i] <- paste("raster('",av_lyr[i,1],"')", sep = "") else av_lyr$call[i]<- paste(av_lyr[i, 1])
  }
  av_lyr <- as.character(av_lyr[,'call'])
  #cat_lyr <- as.character(av_data[av_data$cat==1,1])
  #for(i in 1:length(cat_lyr)){
  #  if (i==1){
  #  cat_nm <- as.character(cat_lyr[i])
  #  }else cat_nm <- as.character(paste(cat_nm, cat_lyr[i], sep = ","))
  #}
  
  #Assessing the sameness of raster extent by matrix####
  identic.matrx <- data.frame()
  #lyr_id <- 1:length(av_lyr)
  #identic.matrx <- as.data.frame(lyr_id)
  #identic.matrx$a <- 1:9
  g <- data.frame()
  for(a in 1: length(av_lyr)){
    for(b in 1:length(av_lyr)){
      eval(parse(text=paste("if(identical(extent(",av_lyr[a],"),extent(",av_lyr[b],")))identic.matrx[a,b] <- 'TRUE' else {identic.matrx[a,b] <- 'FALSE'
                            g <- rbind(g, c(a,b))
    }", sep = "")))
    }
  }
  colnames(identic.matrx)<- 1:length(av_lyr)
  
  if(identical(dim(g), as.integer(c(0,0)))) {print("The input layers have the same spatial extent and resolution. Continuing process..")} else {
    print("At least one of the input layers has unmatched spatial extent and/or resolution. Adjusting extent...")
    print(c("Unmatched record found:\t"))
    #display the unmatched pair while also displaying the table to be edit>>after that, the edit table as input for the spatial adjustment.
    edit(identic.matrx)
    id <- 1:length(av_lyr)
    l_edit <- as.data.frame(cbind(id,av_lyr))
    l_edit$basemap <- 0
    l_edit$adj <- 0
    repeat{
      l_edit<-edit(l_edit)
      if(sum(l_edit$basemap)==1 & sum(l_edit$adj)!=0){
        break
      }
    }
    bmp <- as.integer(as.vector(l_edit[l_edit$basemap==1,1]))
    adjst <- as.integer(as.vector(l_edit[l_edit$adj==1,1]))
    for(i in 1:length(adjst)){
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-", av_lyr[adjst[i]], sep = "")))
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-spatial_sync_raster(unsynced = adj_lyr_",adjst[i],", reference =",av_lyr[bmp],")", sep = "")))
      #replacing the unsynced layer in the av_lyr
      av_lyr[adjst[i]] <- as.character(paste("adj_lyr_",adjst[i], sep = ""))
    }
  }
  
  
  #Create a rasterstack from the cov. layers. Include looping to import all of the rasters in a single command
  
  for (i in 1: length(av_lyr)){
    if(i==1)lyr_call <- av_lyr[1] else lyr_call <- paste(lyr_call,av_lyr[i], sep =",")
    #  lyr_call <- paste(lyr_call,",raster(av_lyr[",i,"])", sep = "")
  }
  
  eval(parse(text = paste("comp_lyr <- stack(",lyr_call,")", sep = "")))
  # AE comment 10: mungkin perlu spatial_sync_raster command dulu untuk memastikan seluruh layer yang digunakan memiliki 
  # resolusi, koordinat system dan coverage yang sama
  
  #viewing the layers in maps, optional?
  plot(comp_lyr, legend=F)
  
  #saving the rasterstack, necessary?
  #writeRaster(comp_lyr, filename="cov_lyrs.grd")
  
  
  #creating random background (bg) points within the study area (study area defined as 'mask')####
  ext <- extent(comp_lyr)
  
  #AD 30/03 : entry the function to subset the presence based on the extent here
  pres <- pt_correct(pres, ext)
  
  # AE comment 11: extent bisa diambil dari : (1) Rdata-see comment3 ; (2) user defined input ; (3) extent minimum dari data yang dimasukkan
  #set.seed(0)
  # bg <- randomPoints(comp_lyr, bg_cts, pres, excludep = T)
  bg <- readShapePoints(file.choose())
  bg <- as.data.frame(cbind(bg$coords.x1, bg$coords.x2))
  # bg <- bg [1:10000,]#/10,000 should be the upper limit under default setting
  #? Should the background points overlapping the presence record(s) be omitted? Here, YES. Note the
  # 'excludep'
  
  # AE comment 12: jumlah random point perlu ditentukan di awal atau dibuat interaktif, kenapa upper limitnya 10.000?
  # AE comment 13: processing speednya agak turun di bagian ini, perlu diexplore apakah untuk bagian ini sebaiknya dilempar keluar dari R
  # dan dilakukan di QGIS saja. Please note that processing time at most case will be doubled when the script is executed within
  # LUMENS environtment
  
  
  #divide the bg pts. into k number of groups randomly
  #set.seed(0)
  bgroup <- kfold(bg, k = fold)
  
  #====ADAPTED FROM MOLECOLOGIST:Randomly subsample the data (perhaps to address the SAC)####
  # create sequences of latitude and longitude values to define the grid	
  #longrid =seq(ext@xmin,ext@xmax,10000)#
  #latgrid =seq(ext@ymin,ext@ymax,10000)#
  # identify points within each grid cell, draw one at random
  #subs =c()#
  #for(i in 1:(length(longrid)-1)){#
  #  for(j in 1:(length(latgrid)-1)){#
  #    gridsq =subset(pres, latitude > latgrid[j] & latitude < latgrid[j+1] & longitude > longrid[i] & longitude < longrid[i+1])#
  #    if(dim(gridsq)[1]>0){#if there is at least one point within the defined grid
  #      subs =rbind(subs, gridsq[sample(1:dim(gridsq)[1],1 ), ]) #sample a point out of available points within defined grid randomly
  #and add the record to the existing set of records (subs)
  #print(paste(i,j, sep = ",")) only to identify grids with point(s)
  #    }
  #  }
  #}
  
  # AE comment 14: agak lost aku disini, apa fungsinya object 'subs'? 
  
  rep_auc <- numeric()
  comp_t_e <- list()
  comp_mod <- list()
  #looping function to assess the performance of the method applied####
  for(i in 1:fold){
    pres_train <- pres[group != i, ]
    pres_test <- pres[group == i, ]
    
    #Define 80% of the presence data as training dataset, while the 20% as test dataset
    bg_train <- bg[bgroup != i, ]
    bg_test <- bg[bgroup == i, ]
    
    
    
    #tools 'spatial_sync_raster'; package 'spatial.tools' -> refer to add planning unit script
    
    #====C. Model Parameterization & Execution####
    ###cat_varnames <- names(comp_lyr)[1] ###INPUT
    max_mod <- maxent(comp_lyr, p= pres_train, factors=cat_id,
                      a= bg_train, path = out, removeDuplicates = T, args=c("-P", "maximumiterations=1000")) #"replicates=5" excluded
    
    #SINI
    eval(parse(text=(paste0("comp_mod$jk",i," <- max_mod"))))
    #max_mod <- maxent(comp_lyr, p= pres_train, factors='papua_climate_landsystem_mercator',
    #                  a= bg_train, path = out, removeDuplicates = T, args= c("-P","maximumiterations=500",
    #                                                                         "randomseed=True",
    #                                                                         "-J",
    #                                                                         "writeplotdata=True"
    #                                                                         )) #"replicates=5" excluded
    #response(max_mod) #rsponse curves display only
    
    #factors = ...' is to declare categorical covariate layers. !!categorical layers should be able
    #to be distinguished and assigned into a variable
    #args to pass additional arguments to set the model run. Which arguments to be used?
    #bgpoints as value to be passed into the 'a = ...' arguments
    #if(i==1){
      #max_mod NOT WORKING
    #  browseURL(paste0(getwd(),"/",max_mod@html))
      #messagebox to allow user to decide if he/she would like to continue or edit the data (terminate process)
    #  pop <- as.data.frame("Please check the intermediate results opened in your default browser")
    #  mbox1 <- cbind("Do you wish to continue?", 1)
    #  colnames(mbox1) <- c("Message", "Yes(1)_No(0)")
    #  edit(pop)
    #  mbox1 <- edit(mbox1)
    #  if(!mbox1[1,2]==1) stop("Please provide the correct input data before restarting the whole process")
    #}
    #there was an issue regarding rJava. Solved by setting the right amount of memory (even number)
    #allocation and adding path directory to the system.
    
    #==== Model Evaluation
    e <- evaluate(pres_test, bg_test, max_mod, comp_lyr)
    eval(parse(text= paste0("comp_t_e$jk",i," <- e"))) #jk stands for jack knife
    rep_auc <- c(rep_auc, e@auc)
  }
  
  # AE comment 15: opening the html file to view intermediate results at this point of execution is a good option. That way user 
  # can change some parameters is the intermediate result is not satisfying. Maybe also displaying or summarizing
  # @kappa as one of the evaluation indicator? Event before maxent command are executed, a simple descriptive analysis might be
  # sufficient for user 
  
  
  #Evaluating the suitability of the method implemented using the measure of Area Under the Curve (AUC)
  m_rep_auc <- mean(rep_auc)
  
  if(m_rep_auc > auc_thres){
    #subsetting the presence and background data for calibration and validation. (Default is 70 30 for calibration and validation, respectively)
    setside <- function(x, sid = sample(1:100000, 1)){
      x <- as.data.frame(x)
      set.seed(sid)
      sampled_id <- sample(1:nrow(x), round(fin_eval_prop/100*nrow(x)))
      uns_id <- setdiff(1:nrow(x), sampled_id)
      sampled <- x[sampled_id,]
      uns <- x[uns_id,]
      res <- list(sid, sampled, uns)
      names(res) <- c("seed", "sampled", "unsampled")
      return(res)
    }
    psence <- setside(pres)
    bgraun <- setside(bg)
    fin_mod <- maxent(comp_lyr, p= psence$unsampled, factors=cat_id,
                      a= bgraun$unsampled, path = out, removeDuplicates = T, args= c("-P","maximumiterations=5000",
                                                                                     "randomseed=True",
                                                                                     "-J",
                                                                                     "writeplotdata=True"
                      )) #"replicates=5" excluded
    e_fin <- evaluate(psence$sampled, bgraun$sampled, fin_mod, comp_lyr)
    
    # AE comment 16: Again, displaying intermediate result at this point is good for user 
  }else print("The modeling method couldn't fit the data properly")
  #Displaying the final model results####
  browseURL(paste0(getwd(),"/",fin_mod@html))
  #edit(pop)
  
  #Ask if user would like to reselect the predictor layers####
  #  back <- cbind("Do you wish to reselect the predictor layers included within the analysis?", 0)
  #colnames(back)<- c("Message", "Yes(1)_No(0)")
  #back<- edit(back)
  #if(back[1,2]!=1) break
  #}


#processing the extrapolation rasetr layers (refer to extra_input_data.R)####
if(ex_dir!=""){
  ex_lyr <- list.files(ex_dir,pattern = ".tif", full.names = T) # only take .tif raster files into account
  ex_lyr <- grep(".tif\\z", ex_lyr, perl = T, value = T) #specifically extract only the file names that end with pattern ".tif"
  # AE comment 8: sama seperti comment3, list of files ditentukan melalui RData
  
  #====Selecting which layer to be excluded and which contains categorical variables
  ex_data <- as.data.frame(ex_lyr)
  ex_data$Notes <- "Souce: External dir."
  #adding the existing raster files inside the .Rdata
  colnames(in_dat) <- colnames(ex_data)
  ex_data <- rbind(ex_data, in_dat) #the displayed list is the raster names instead of the variable names
  
  
  
  ex_data$include <- 1
  ex_data$cat <- 0
  n <- nrow(ex_data)
  
  #(290216) AD: ex_data is the variable to be edited when user wants to change model parameterization after assessing the generated model performance####
  repeat{
    ex_data<-edit(ex_data)
    if(sum(ex_data$include)!=0){
      break
    }
  }
  
  ex_lyr <- as.data.frame(ex_data[ex_data$include==1,])
  ex_lyr$id <- 1:nrow(ex_lyr)
  cat_id <- ex_lyr[ex_lyr$cat==1,'id']
  ex_lyr$call<- NA
  row_ext <- grep (".tif\\z", ex_lyr$ex_lyr, perl = T)
  for(i in 1: nrow(ex_lyr)){
    if(i %in% row_ext) ex_lyr$call[i] <- paste("raster('",ex_lyr[i,1],"')", sep = "") else ex_lyr$call[i]<- paste(ex_lyr[i, 1])
  }
  ex_lyr <- as.character(ex_lyr[,'call'])
  #cat_lyr <- as.character(av_data[av_data$cat==1,1])
  #for(i in 1:length(cat_lyr)){
  #  if (i==1){
  #  cat_nm <- as.character(cat_lyr[i])
  #  }else cat_nm <- as.character(paste(cat_nm, cat_lyr[i], sep = ","))
  #}
  
  #Assessing the sameness of raster extent by matrix####
  identic.matrx <- data.frame()
  #lyr_id <- 1:length(ex_lyr)
  #identic.matrx <- as.data.frame(lyr_id)
  #identic.matrx$a <- 1:9
  g <- data.frame()
  for(a in 1: length(ex_lyr)){
    for(b in 1:length(ex_lyr)){
      eval(parse(text=paste("if(identical(extent(",ex_lyr[a],"),extent(",ex_lyr[b],")))identic.matrx[a,b] <- 'TRUE' else {identic.matrx[a,b] <- 'FALSE'
                            g <- rbind(g, c(a,b))
    }", sep = "")))
    }
  }
  colnames(identic.matrx)<- 1:length(ex_lyr)
  
  if(identical(dim(g), as.integer(c(0,0)))) {print("The input layers have the same spatial extent and resolution. Continuing process..")} else {
    print("At least one of the input layers has unmatched spatial extent and/or resolution. Adjusting extent...")
    print(c("Unmatched record found:\t"))
    #display the unmatched pair while also displaying the table to be edit>>after that, the edit table as input for the spatial adjustment.
    edit(identic.matrx)
    id <- 1:length(ex_lyr)
    l_edit <- as.data.frame(cbind(id,ex_lyr))
    l_edit$basemap <- 0
    l_edit$adj <- 0
    repeat{
      l_edit<-edit(l_edit)
      if(sum(l_edit$basemap)==1 & sum(l_edit$adj)!=0){
        break
      }
    }
    bmp <- as.integer(as.vector(l_edit[l_edit$basemap==1,1]))
    adjst <- as.integer(as.vector(l_edit[l_edit$adj==1,1]))
    for(i in 1:length(adjst)){
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-", ex_lyr[adjst[i]], sep = "")))
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-spatial_sync_raster(unsynced = adj_lyr_",adjst[i],", reference =",ex_lyr[bmp],")", sep = "")))
      #replacing the unsynced layer in the ex_lyr
      ex_lyr[adjst[i]] <- as.character(paste("adj_lyr_",adjst[i], sep = ""))
    }
  }
  #Create a rasterstack from the cov. layers. Include looping to import all of the rasters in a single command
  for (i in 1: length(ex_lyr)){
    if(i==1)lyr_call <- ex_lyr[1] else lyr_call <- paste(lyr_call,ex_lyr[i], sep =",")
    #  lyr_call <- paste(lyr_call,",raster(ex_lyr[",i,"])", sep = "")
  }
  
  eval(parse(text = paste("comp_ex_lyr <- stack(",lyr_call,")", sep = "")))
  
  #plot(comp_ex_lyr, legend=F)
  
  
  ex_ext <- extent(comp_ex_lyr)
  
  #creating the table to match extrapolation layer names with the calibration layer names####
  match <- as.data.frame(names(comp_ex_lyr), stringsAsFactors=F)
  for(i in 1: length(names(comp_lyr))){
    eval(parse(text = paste0("match$",names(comp_lyr)[i]," <- 0")))
  }
  colnames(match)[1]<- "extrapolate\\calibration"
  repeat{
    match <- edit(match)
    indicat <- integer()
    indic <- integer()
    for(i in 2: ncol(match)) indicat <- c(indicat, sum(match[,i]))
    for(i in 1: nrow(match)) indic <- c(indic, sum(match[i,2:ncol(match)]))
    if(unique(indicat)==1 & unique(indic)==1) break
  }
  
  c <- vector(length = nrow(match))
  for(i in 1:nrow(match)){
    names(comp_ex_lyr)[i]<- names(comp_lyr)[grep(1,match[i,2:ncol(match)])]
    c[grep(1,match[i,2:ncol(match)])] <- i
  }
  for(i in 1:nrow(match)) names(comp_ex_lyr)[i]<- names(comp_lyr)[grep(1,match[i,2:ncol(match)])] #duplicated due to some bugs within R.
  
  com_ex_lyr <- subset(comp_ex_lyr, order(c)) #cannot subset rasterstack into the same variable. hence, the var name has to be changed
    }


#Generating prediction map####

if(generate_map){if(ex_dir=="") px <- predict(comp_lyr, fin_mod, ext=ext, progress='', filename =paste0(out,"/",o_pred_filename)) else px <- predict(com_ex_lyr, fin_mod, ext=ex_ext, progress='', filename =paste0(out,"/",o_pred_filename))} # only conducted when user has been satisfied with the results
data("wrld_simpl")
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')
plot(wrld_simpl, add=TRUE, border="dark grey")

tr <- threshold(e_fin, 'spec_sens')
plot(px > tr, main='presence/absence')

plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

# AE comment 17: Object px and wrld_simpl needs to be converted to .tif as the final result and incoporated in RData
# AE comment 18: Since 'predict' takes a lot of time and computer resources, it should only executed once users are satifisfied with
# the model
# AE comment 19: At this point of execution, 2.55 GB of temporary data are stored in my hardisk
# this is problematic for 2 reasons: (1) it is stored in location where LUMENS might not have the authority to 
# save fil e ; (2) it depends on the availibility of hardisk space ; (3) the temporary file is not erased event when my R session is closed
# AE comment 20: My sense is this script needs to be splitted into 3 sub-module: (1) Build model; (2) Predict ; and (3) Analyze
# that way we have some possibility to include different method beside MaxEnt: GLM, GAM, randomforest, logreg, MARS are some
# of the possibility
# AE comment 21: IF we have enough testing result to say that landcover or planning unit is one of strongest predictors for any 
# models in MaxEnt, can we make it as a time series analysis where we can see the impact of land use/cover change to the model?

#Writing the end time of the processing at the logfile####
en <- as.character(Sys.time())
qb_log[sdm_id, "End"] <- en
write.table(qb_log, file = log, row.names = F)

#resaving necessary variables into the .lpj####
QUESB.sdm.index <-sdm_id
#resave(QUESB.sdm.index, file = proj.file)
###END

