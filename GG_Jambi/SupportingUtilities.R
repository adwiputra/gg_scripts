# Preparation script
# AD
# 25/09/2018

# translating lulc_lut into indonesian====
table_dir <- "D:/GG_Jambi/General/data/table/"
lut_lu <- read.csv(paste0(table_dir, "lut_lu.csv"), stringsAsFactors = FALSE)
trans_lu <- read.csv(paste0(table_dir, "trans_lcLut.csv"), stringsAsFactors = FALSE)
names(trans_lu)[1] <- "ID"
lut_lu <- merge(lut_lu, trans_lu, by = "ID", all.x = TRUE)
lut_lu$LEGEND <- lut_lu$Tutupan.lahan
lut_lu <- lut_lu[, c("ID", "LEGEND", "Traj")]
write.csv(lut_lu, paste0(table_dir, "land_requirements/table_dir/lut_lu.csv"), row.names = FALSE)

# translating lulc_lut into indonesian\end====


# pu_lut adjustment====
lut_pu <- read.csv(paste0(table_dir, "pu_lookup.csv"), stringsAsFactors = FALSE)
lut_pu <- lut_pu[, c(1, 3, 2)]
names(lut_pu) <- c("Value", "PU", "Count")
write.csv(lut_pu, paste0(table_dir, "land_requirements/table_dir/planning_unit.csv"), row.names = FALSE)
# pu_lut adjustment \end====

# Define unchanged step for all of the zones in the folder====
# require big_freqtab
# was run using the raw raw tables
tpm_rawDir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready35-40/"
setwd(tpm_rawDir)
transitionFiles <- list.files(tpm_rawDir, pattern = "^Single_step")
n_zones <- length(transitionFiles)

for(z in 1:n_zones){
  filterRate <- read.csv(transitionFiles[z], stringsAsFactors = FALSE)
  if(nrow(filterRate) != 0){
  fromFilter <- unique(filterRate[, 1])
  # define blanko for unchanged prop
  unchgProp <- numeric()
  sumChg <- numeric()
  for(f in 1:length(fromFilter)){
    sumChg <- c(sumChg, sum(filterRate[filterRate$From.== fromFilter[f], 3]))
  }
  unchgProp <- 1-sumChg
  # add existing unchanged lc lc class
  exIn <- unique(big_freqtab[big_freqtab$value >= z*100 & big_freqtab$value < (z+1)*100, "value"] %% 100)
  addFilter <- setdiff(exIn, fromFilter)
  fromFilter <- c(fromFilter, addFilter)
  unchgProp <- c(unchgProp, rep(1, length(addFilter)))
  unchg_table <- data.frame(From= fromFilter, To= fromFilter, Rate= unchgProp, stringsAsFactors = FALSE)
  names(unchg_table) <- c("From*", "To*", "Rate")
  write.csv(unchg_table, paste0("Unchanged_step_", z, ".csv"), row.names = FALSE, quote = FALSE)
  } else{# handler for table with no trajectory defined
    fromFilter <- numeric()
    unchgProp <- numeric()
    sumChg <- numeric()
    # add existing unchanged lc lc class
    exIn <- unique(big_freqtab[big_freqtab$value >= z*100 & big_freqtab$value < (z+1)*100, "value"] %% 100)
    addFilter <- setdiff(exIn, fromFilter)
    fromFilter <- c(fromFilter, addFilter)
    unchgProp <- c(unchgProp, rep(1, length(addFilter)))
    unchg_table <- data.frame(From= fromFilter, To= fromFilter, Rate= unchgProp, stringsAsFactors = FALSE)
    names(unchg_table) <- c("From*", "To*", "Rate")
    write.csv(unchg_table, paste0("Unchanged_step_", z, ".csv"), row.names = FALSE, quote = FALSE)
  }
}

# rbind the whole possible trajectories into the Single_step tables=====
tpm_rawDir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready35-40/"
tpm_dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready35-40/complete_trans/"#CHGABLE
setwd(tpm_rawDir)
dir.create(tpm_dir)
transitionFiles <- list.files(tpm_rawDir, pattern = "^Single_step")
n_zones <- length(transitionFiles)
tabl.dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/"
# load the lookup table
lc_indo_lu <- read.csv(paste0(tabl.dir, "lut_lu.csv"), stringsAsFactors = FALSE)
# table blanko
blank_table <- data.frame(From.= numeric(), To. = numeric(), Rate= numeric())
# loop to obtain all of the possible combinations
for(c in 1:nrow(lc_indo_lu)){
  for(hc in 1:nrow(lc_indo_lu)){
    blank_table <- rbind(blank_table, c(lc_indo_lu$ID[c], lc_indo_lu$ID[hc], 0))
  }
}
# define colnames
names(blank_table) <- c("From.", "To.", "Rate")
# removing rows representing static/unchanged condition
blank_table <- blank_table[blank_table$From.!=blank_table$To., ]
for(z in 1:n_zones){
  # clearing all of the Rate
  blank_table$Rate <- 0
  names(blank_table) <- c("From.", "To.", "Rate")
  #loading the original table
  filterRate <- read.csv(transitionFiles[z], stringsAsFactors = FALSE)
  if(nrow(filterRate) != 0){
    # looping per row to write down the value of change into the blanko
    for(w in 1:nrow(filterRate)){
      blank_table[blank_table$From.==filterRate[w, 1] & blank_table$To.==filterRate[w, 2], "Rate"] <- filterRate[w, 3]
    }
  } 
  # write the table
  names(blank_table) <- c("From*", "To*", "Rate")
  if(z < 10){
    write.csv(blank_table, paste0(tpm_dir, "Single_step00000", z, ".csv"), row.names = FALSE, quote = FALSE)
  } else{
    write.csv(blank_table, paste0(tpm_dir, "Single_step0000", z, ".csv"), row.names = FALSE, quote = FALSE)
  }
}


# rbind the whole possible trajectories into the Unchanged_tables=====
# tpm_rawDir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/transition_filter2409/"
# setwd(tpm_rawDir)
# dir.create("complete_trans")
transitionFiles <- list.files(tpm_rawDir, pattern = "^Unchanged")
# n_zones <- length(transitionFiles)
tabl.dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/"
# load the lookup table
lc_indo_lu <- read.csv(paste0(tabl.dir, "lut_lu.csv"), stringsAsFactors = FALSE)
# table blanko
blank_table <- data.frame(From.= numeric(), To. = numeric(), Rate= numeric())
# loop to obtain all of the possible combinations
for(c in 1:nrow(lc_indo_lu)){
    blank_table <- rbind(blank_table, c(lc_indo_lu$ID[c], lc_indo_lu$ID[c], 0))
}
# define colnames
names(blank_table) <- c("From.", "To.", "Rate")
# removing rows representing static/unchanged condition
# blank_table <- blank_table[blank_table$From.!=blank_table$To., ]
for(z in 1:n_zones){
  # clearing all of the Rate
  blank_table$Rate <- 0
  names(blank_table) <- c("From.", "To.", "Rate")
  #loading the original table
  filterRate <- read.csv(transitionFiles[z], stringsAsFactors = FALSE)
  if(nrow(filterRate) != 0){
    # looping per row to write down the value of change into the blanko
    for(w in 1:nrow(filterRate)){
      blank_table[blank_table$From.==filterRate[w, 1] & blank_table$To.==filterRate[w, 2], "Rate"] <- filterRate[w, 3]
    }
  } 
  # write the table
  names(blank_table) <- c("From*", "To*", "Rate")
  if(z < 10){
    write.csv(blank_table, paste0(tpm_dir, "Unchanged_step_", z, ".csv"), row.names = FALSE, quote = FALSE)
  } else{
    write.csv(blank_table, paste0(tpm_dir, "Unchanged_step_", z, ".csv"), row.names = FALSE, quote = FALSE)
  }
}

# Filtering final Single_step_Plan====
file_lookup_transition <- "D:/GG_Jambi/General/data/table/allow_transition.csv"
lookup_transition <- read.csv(file_lookup_transition, stringsAsFactors = FALSE, header = FALSE) # first two columns contain information on "ID" and "class name". No header
subLookup_trans <- lookup_transition[, 3:ncol(lookup_transition)]
rate_dir_raw <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready35-40/complete_trans/"
rate_dir <- "D:/GG_Jambi/General/data/table/land_requirements/table_dir/run_ready40-45/"
if(!dir.exists(rate_dir))dir.create(rate_dir)

# Rate filtering mechanism
zone_rates <- list.files(rate_dir_raw, pattern = "^Single_step_Plan", recursive = FALSE, full.names = TRUE)
for(z in 1:length(zone_rates)){
  # creating z_id variable to be pasted in the saving process
  if(z < 10) z_id <- paste0("00000", z) else z_id <- paste0("0000", z)
  # reading the raw rate table
  z_rate <- read.csv(zone_rates[z], stringsAsFactors = FALSE)
  z_rate <- z_rate[z_rate$Rate > 0, ]
  if(nrow(z_rate) > 0){
  # loop to filter rates not allowed
  z_rate$allow <- 0
  for(rr in 1: nrow(z_rate)){
    if(eval(parse(text= paste0("subLookup_trans[", (z_rate[rr, 1]+1), ", ", (z_rate[rr, 2]+1), "] == 1")))) z_rate[rr, "allow"] <- 1
    # if(z_rate[rr, 2] == id_oilpalm) z_rate[rr, 3] <- adj_coef * z_rate[rr, 3]#ADCHECK
  }
  z_rate <- z_rate[z_rate$allow==1, 1:3]
  z_rate$a <- ""
  } else {
    z_rate <- data.frame(From.= numeric(), To. = numeric(), Rate = numeric(), a= character())
  }
  names(z_rate) <- c("From*", "To*", "Rate", "")
  write.table(z_rate[, 1:4], file= paste0(rate_dir, "/Single_step_Plan_", z_id, ".csv"), row.names = FALSE, quote = FALSE, sep = ", ")
}

