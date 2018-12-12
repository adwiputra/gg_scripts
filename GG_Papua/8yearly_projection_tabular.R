# AD
# 19/11/18
options(digits = 20)
library(tidyverse)
# Script to generate the 8 yearly land use change projection using the lumensMainDatabase
projSteps <- 4
periodLength <- 8

# 0. Read the lumensMainDatabase from table
lumensMainDatabase <- read.csv("D:/GG_Papua/process/lumens_dir/IDH/Result/lumensMainDatabase.csv", stringsAsFactors = FALSE)

# 1. Calculate Transition Rate per 8 years (T2-T1)=====
# Calculate the source's stock for each lc class for each zone. 
lu_initStock <- lumensMainDatabase %>% dplyr::select(ID_LC_T1, ID_Z, COUNT) %>% group_by(ID_LC_T1, ID_Z) %>% summarise_at("COUNT", funs(sum)) %>% data.frame()
lu_initStock <- lu_initStock %>% dplyr::rename(!!"COUNT.stock":= COUNT) # COUNT.stock at 2010 (t0); equivalent to COUNT.stock0
lumensMainDatabase <- lumensMainDatabase %>% left_join(lu_initStock) %>% data.frame()
# Rate calculation
lumensMainDatabase <- lumensMainDatabase %>% mutate(Rate = COUNT/COUNT.stock) %>% data.frame
lumensMainDatabase <- lumensMainDatabase %>% mutate_at("Rate", funs(replace_na(., 0))) %>% data.frame
# lumensMainDatabase <- lumensMainDatabase %>% mutate(COUNT.stockNew = case_when(COUNT == 0 ~ 1,
                                                                            # TRUE ~ as.numeric(COUNT.stock)))
# handler for unavailable Rates
unavTraj <- lumensMainDatabase %>% dplyr::select(ID_LC_T1, ID_Z, Rate) %>% group_by(ID_LC_T1, ID_Z) %>% summarise_at("Rate", funs(sum)) %>% filter(Rate == 0) %>% data.frame()# identify source classes with no predefined rates
# L0 Loop per row of unavTraj====
for(u in 1:nrow(unavTraj)){
  #
  currID_T1 <- unavTraj %>% dplyr::slice(u) %>% dplyr::select(ID_LC_T1) %>% pull()
  currID_Z <- unavTraj %>% dplyr::slice(u) %>% dplyr::select(ID_Z) %>% pull()
  if(currID_T1 != 0) lumensMainDatabase <- lumensMainDatabase %>% mutate(Rate = case_when(ID_LC_T1 == ID_LC_T2 & ID_LC_T1 == currID_T1 & ID_Z == currID_Z ~ 1,
                                                                                          TRUE ~ Rate)) %>% data.frame()
} # L0\ends----
# 1. Calc \ends----

# 2. Apply looping to get the count for each of the next period====
# L1 Loop per projection period
for(p in 1: projSteps){
  colID <- p+1
  # calculate stock at previous step
  if(p>1) {
    # lu_prevStock <- lumensMainDatabase %>% dplyr::select(ID_LC_T2, ID_Z, paste0("COUNT", p)) %>% group_by(ID_LC_T2, ID_Z) %>% eval(parse(text= paste0("summarise_at(COUNT", p, ", funs(sum))")))
    lu_prevStock <- lumensMainDatabase %>% dplyr::select(ID_LC_T2, ID_Z, paste0("COUNT", p)) %>% group_by(ID_LC_T2, ID_Z) %>% summarise_at(paste0("COUNT", p), funs(sum)) %>% data.frame()
    lu_prevStock <- lu_prevStock %>% dplyr::rename(!!paste0("COUNT.stock", p):= paste0("COUNT", p)) %>% data.frame()
  } else{
    lu_prevStock <- lumensMainDatabase %>% dplyr::select(ID_LC_T2, ID_Z, COUNT) %>% group_by(ID_LC_T2, ID_Z) %>% summarise_at("COUNT", funs(sum)) %>% data.frame()
    lu_prevStock <- lu_prevStock %>% dplyr::rename(!!paste0("COUNT.stock", p):= COUNT) %>% data.frame()
  }
  lu_prevStock <- lu_prevStock %>% dplyr::rename(!!"ID_LC_T1":= ID_LC_T2) %>% data.frame()
  # ADcheck
  lu_prevStock %>% dplyr::select(paste0("COUNT.stock", p)) %>% pull() %>% sum() %>% print()
  # merge with the lumensMainDatabase
  # lumensMainDatabase <- lumensMainDatabase %>% left_join(lu_prevStock) %>% mutate(!!paste0("COUNT", colID):= COUNT/COUNT.stockNew * eval(parse(text= paste0("COUNT.stock", p))))
  lumensMainDatabase <- lumensMainDatabase %>% left_join(lu_prevStock) %>% mutate(!!paste0("COUNT", colID):= Rate * eval(parse(text= paste0("COUNT.stock", p)))) %>% data.frame()
  # lumensMainDatabase <- 
  
}# L1 \ends----
# 2. Apply loop\ends----

# 3. Rename colnames =====
oldCnames <- lumensMainDatabase %>% select(starts_with("COUNT", ignore.case = FALSE)) %>% select(which(!grepl("stock", names(.)))) %>% names()
newCnames <- paste0("COUNT", seq(2018, 2050, 8))
lumensMainDatabase <- lumensMainDatabase %>% rename_at(vars(oldCnames), ~ newCnames)

# 3. Rename \ends-----


# 4. Rearrange columns=====
lumensMainDatabase <- lumensMainDatabase %>% select(2,1,3, 5,6,7,8,9,12, 10, 13, 11, 14,15, 16, 18, 17, 4, 19:26)
# 4. Rearr\ends----
# checking area constancy


# 5. Emission calculation====
# LUC karbon delta
lumensMainDatabase <- lumensMainDatabase %>% mutate(C_diff = C_T2 - C_T1)

# L2 looping per period
newCnames <- newCnames %>% gsub("COUNT", "", .)
for(t in 1: projSteps){
  lumensMainDatabase <- lumensMainDatabase %>% mutate(!!paste0("EmPrd", t):= C_diff * -3.67 * eval(parse(text= paste0("COUNT", newCnames[t+1]))))
} # L2 \ends----

# Sequestration
for(t in 1: projSteps){
  lumensMainDatabase <- lumensMainDatabase %>% mutate(!!paste0("SeqPrd", t):= case_when(eval(parse(text= paste0("EmPrd", t))) < 0 ~ eval(parse(text= paste0("EmPrd", t)))* -1,
                                                                                        TRUE ~ 0))
}

# Peat Emission
for(t in 1: projSteps){
  lumensMainDatabase <- lumensMainDatabase %>% mutate(!!paste0("Em_peatPrd", t):= case_when(PEAT == "Peat" ~ P_T1*eval(parse(text= paste0("COUNT", newCnames[t+1])))*periodLength/2 + P_T2*eval(parse(text= paste0("COUNT", newCnames[t+1])))*periodLength/2,
                                                                                        TRUE ~ 0))
}

lumensMainDatabase <- lumensMainDatabase %>% mutate_at(28:ncol(.), funs(if_else(. < 0, 0, .)))

# reshaping emission
emission_reshape <- lumensMainDatabase %>% select(ADMIN, PLAN, PEAT, contains("Em"), contains("Seq"))
emission_reshape <- emission_reshape %>% gather("Categories", "TonC.Eq", 4:ncol(.))
# add period
emission_reshape <- emission_reshape %>% mutate(Period = case_when(grepl("1", Categories) ~ paste0(newCnames[1], "-", newCnames[2]),
                                                                   grepl("2", Categories) ~ paste0(newCnames[2], "-", newCnames[3]),
                                                                   grepl("3", Categories) ~ paste0(newCnames[3], "-", newCnames[4]),
                                                                   TRUE ~ paste0(newCnames[4], "-", newCnames[5])))
# Redefine categories
emission_reshape <- emission_reshape %>% mutate(Categories = if_else(grepl("^EmPrd", Categories), "Emission", if_else(grepl("^Em_peat", Categories), "Peat Emission", "Sequestration")))
# arranging the table
emission_reshape <- emission_reshape %>% select(ADMIN, PLAN, PEAT, Period, Categories, TonC.Eq)
# turn into negative the sequestration
emission_reshape <- emission_reshape %>% mutate_at("TonC.Eq", funs(if_else(Categories == "Sequestration", TonC.Eq * -1, TonC.Eq)) )

# finishing steps
# reshape, change unit into million USD
emission_reshape <- emission_reshape %>% mutate(TonC.Eq = TonC.Eq/10^6) # unit in MillTonC.Eq
emission_reshape <-emission_reshape %>% filter(PLAN != "No Data")
# reshape \ends----

# visualization: geom_bar(stat = "identity", position = "dodge")
plot_total <- ggplot(emission_reshape, aes(x = Period, y = TonC.Eq, fill = Categories)) + geom_bar(stat = "identity", position = "stack") + ylab("million Ton C-eq") + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
ggsave("EmissionTotalPapuamillC-Eq.png", width=6,height=6,dpi=150)
plot_peatMineral <- plot_total + facet_grid(~ PEAT)
ggsave("EmissionpeatPapuamillC-Eq.png", width=6,height=6,dpi=150)
plotAdat <- plot_total + facet_grid(~ PLAN)
ggsave("EmissionadatPapuamillC-Eq.png", width=14,height=6,dpi=150)
plotrtr <- plot_total + facet_grid(~ ADMIN)
ggsave("EmissionrtrPapuamillC-Eq.png", width=11,height=6,dpi=150)
# 5 Emission \ends-----


# check rate sum
cek_rate <- lumensMainDatabase %>% dplyr::select(ID_LC_T1, ID_Z, Rate) %>% group_by(ID_LC_T1, ID_Z) %>% summarise_at("Rate", funs(sum))

lumensMainDatabase %>% dplyr::select(COUNT2, ID_LC_T2, ID_Z) %>% group_by(ID_LC_T2) %>% summarise_at("COUNT2", funs(sum)) %>% print()
