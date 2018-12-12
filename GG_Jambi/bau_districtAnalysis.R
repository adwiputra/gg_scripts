# GG_Jambi
# 02/12/2018

# script to segregate the landuse transition per district
library(tidyverse)

bauH_file <- "data/bauH_emission_LCdynamics.csv"
bauP_file <- "data/bauP_emission_LCdynamics.csv"
luClass_file <- "data/lut_lu.csv"

# Read files====
bauH <- read.csv(bauH_file, stringsAsFactors= FALSE)
bauP <- read.csv(bauP_file, stringsAsFactors= FALSE)
luClass <- read.csv(luClass_file, stringsAsFactors = FALSE)

# commodities in focus: Padi(20), Sawit(13), HOrtikultur(22), Karet(11-12, AF-mono), Kopi(10), Kelapa Dalam (17, 18; AF-mono), Kulit manis (15-16; AF-mono), Pinang (17), Akasia (8)

# Table to contains columns: Type, Scen, Year, Area

# testing routine to obtain specified table====
# 0. Define temporal scope(1990: 2045, by 5 years)====
years <- bauH %>% dplyr::select(PERIOD) %>% pull() %>% unique()
years <- years %>% substr(., 1, 4) %>% as.numeric() %>% c(., 2045)
# 0. Define... \ends----

# L0: per scenario loop; 1 = Historical| 2: Plan====
for(sc in 1:2){
  # IF_1: the bauH's case====
  if(sc == 1){
    l_marker <- length(years) %/% 2 + length(years) %% 2
    scenTab <- bauH
    scenString <- "Historis"
    } else{
      l_marker <- length(years[which(years > 2015)]) %/%2
      scenTab <- bauP
      scenString <- "Rencana"
    }
  # IF_1 \ends----
  # L1: per period simulation====
  for(l in 1:l_marker){
    # IF 1.5====
    if(sc== 1 & l != l_marker){
      # periodTab <- scenTab %>% filter(grepl(paste0(years[(l*2-1)], "-", years[(l*2)]), PERIOD))
      year1 <- years[(l*2-1)]
      year2 <- years[(l*2)]
      } else if(sc==1 & l == l_marker){
        # periodTab <- scenTab %>% filter(grepl(paste0(years[(l*2-2)], "-", years[(l*2-1)]), PERIOD))
        year1 <- years[(l*2-2)]
        year2 <- years[(l*2-1)]
      } else if(sc==2) {
        # periodTab <- scenTab %>% filter(grepl(paste0(years[5+(l*2-1)], "-", years[5+(l*2)]), PERIOD))
        year1 <- years[5+(l*2-1)]
        year2 <- years[5+(l*2)]
      }
    periodTab <- scenTab %>% filter(grepl(paste0(year1, "-", year2), PERIOD))
      # IF 1.5\ends----
    # IF_2:  & l != l_marker, calculate both t1 and t2; also if sc == 1?
    if((sc== 1 & l != l_marker)| sc==2){
      # t1 table calculation====
      tabT1 <- periodTab %>% mutate(Comm1 = case_when(ID_T1 == 20 ~ "Sawah",
                                                      ID_T1 == 13 ~ "Sawit",
                                                      ID_T1 == 22 ~ "Hortikultura",
                                                      ID_T1 %in% c(11,12) ~ "Karet",
                                                      ID_T1 == 10 ~ "Kopi",
                                                      ID_T1 %in% c(17, 18) ~ "Kelapa Dalam",
                                                      ID_T1 %in% c(15, 16) ~ "Kulit Manis",
                                                      ID_T1 == 8 ~ "Akasia",
                                                      TRUE~"")) %>% mutate(Pinang1 = case_when(ID_T1 == 17 ~ "Pinang",
                                                                                               TRUE~""))
      # summarise comms other than pinang
      summTabT1 <- tabT1 %>% group_by(IDADM, ADMIN_UNIT, eval(parse(text=paste0("Comm", 1)))) %>% summarize_at(vars(count), funs(sum)) %>% rename_at(vars(3), ~ "Commodity")%>% mutate(year = year1) %>% mutate(scen = scenString) %>% filter_at(vars(3), all_vars(.!=""))
      pinangTabT1 <- tabT1 %>% group_by(IDADM, ADMIN_UNIT, eval(parse(text=paste0("Pinang", 1)))) %>% summarize_at(vars(count), funs(sum)) %>% rename_at(vars(3), ~ "Commodity")%>% mutate(year = year1) %>% mutate(scen = scenString) %>% filter_at(vars(3), all_vars(.!=""))
      # bind the tables
      summTabT1 <- summTabT1 %>% bind_rows(pinangTabT1)
      # t1 table calc.\ends-----
      # defining the compilation table
      if(sc==1 & l == 1){
        compileTab <- summTabT1
      } else{
        compileTab <- compileTab %>% bind_rows(summTabT1)
      }
    }
      tabT2 <- periodTab %>% mutate(Comm1 = case_when(ID_T2 == 20 ~ "Sawah",
                                                      ID_T2 == 13 ~ "Sawit",
                                                      ID_T2 == 22 ~ "Hortikultura",
                                                      ID_T2 %in% c(11,12) ~ "Karet",
                                                      ID_T2 == 10 ~ "Kopi",
                                                      ID_T2 %in% c(17, 18) ~ "Kelapa Dalam",
                                                      ID_T2 %in% c(15, 16) ~ "Kulit Manis",
                                                      ID_T2 == 8 ~ "Akasia",
                                                      TRUE~"")) %>% mutate(Pinang1 = case_when(ID_T2 == 17 ~ "Pinang",
                                                                                               TRUE~""))
      # summarise comms other than pinang
      summTabT2 <- tabT2 %>% group_by(IDADM, ADMIN_UNIT, eval(parse(text=paste0("Comm", 1)))) %>% summarize_at(vars(count), funs(sum)) %>% rename_at(vars(3), ~ "Commodity")%>% mutate(year = year2) %>% mutate(scen = scenString) %>% filter_at(vars(3), all_vars(.!=""))
      pinangTabT2 <- tabT2 %>% group_by(IDADM, ADMIN_UNIT, eval(parse(text=paste0("Pinang", 1)))) %>% summarize_at(vars(count), funs(sum)) %>% rename_at(vars(3), ~ "Commodity")%>% mutate(year = year2) %>% mutate(scen = scenString) %>% filter_at(vars(3), all_vars(.!=""))
      # bind the tables
      summTabT2 <- summTabT2 %>% bind_rows(pinangTabT2)
      compileTab <- compileTab %>% bind_rows(summTabT2)
  }# L1\ends----
} # L0\ends----

# plotting function====
admin_All <- compileTab %>% dplyr::select(ADMIN_UNIT) %>% pull() %>% unique()
comm_All <- compileTab %>% dplyr::select(Commodity) %>% pull() %>% unique()
# L2: per administrative boundary
for(a in 1:length(admin_All)){
  wdir <- admin_All[a]
  dir.create(wdir) #storing the plots
  # L3: per commodity
  for(c in 1:length(comm_All)){
    plotTable <- compileTab %>% filter(ADMIN_UNIT == wdir) %>% filter(Commodity == comm_All[c])
    plotArea_total <- ggplot(plotTable, aes(x = year, y = count, fill = scen)) + geom_bar(stat = "identity", position = 'dodge2') + ylab("HEKTAR")+ xlab("TAHUN")+ scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks=years) + theme_classic() + theme(legend.position="none", axis.title.x = element_blank(), axis.text = element_text(size= 38), axis.title.y = element_text(size = 40))#, plot.title = element_text(hjust = 0.41, size = 18))#+ guides(fill=guide_legend(title="Land Cover")) #position = 'stack' from geom_bar()
    ggsave(paste0(admin_All[a], "/", admin_All[a], "_", comm_All[c], ".png"), width=18,height=6,dpi=150)
  }
}
# plotting \ends----