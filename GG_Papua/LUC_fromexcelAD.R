# LUC analysis
# GG Papua
# to extract the macroindicators: 1) % of AF cover relative to the whole land area 2) Deforestation rate (thousands ha/year) 3) % treee cover outside forest relative to the whole land area



library(tidyverse)
# library(officer)
setwd("D:/GG_Papua/process/lumens_dir/IDH/Result/")

# 0. getting hands on read_excel function=====
# mother_tab <- readxl::read_excel("LUMENSHist_database_5yearly.xlsx", sheet = 1) %>% data.frame(stringsAsFactors = FALSE)
# 0. \end====
mother_tabNames <- mother_tab %>% select(-starts_with("COUNT")) %>% names()
mother_tab <- lumensMainDatabase %>% select(c(mother_tabNames,starts_with("COUNT"))) %>% select(-contains(".stock"))

# 1. Percentage of AF====
AF_tab_pct <- mother_tab %>% filter_at(.vars = vars(ID_LC_T1, ID_LC_T2), all_vars(! . %in% c(0,16,17))) %>% mutate_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(.*100/sum(.))) %>% filter(LC_T2 == "Agroforest") %>% select(ADMIN, PLAN, PEAT, LC_T2, !!paste0("COUNT", seq(2018, 2050, by = 8)))

AF_tab_count <- mother_tab %>% filter_at(.vars = vars(ID_LC_T1, ID_LC_T2), all_vars(! . %in% c(0,16,17))) %>% filter(LC_T2 == "Agroforest") %>% select(ADMIN, PLAN, PEAT, LC_T2, !!paste0("COUNT", seq(2018, 2050, by = 8)))

# renaming year columns====
oldCnames <- AF_tab_count %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- AF_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# AF_tabRename <- AF_tab %>% rename_at(newCnames)
AF_tab_count <- AF_tab_count %>% rename_at(vars(oldCnames), ~ newCnames)

# per area classification (in million hectares)====
AF_count_reshape <- AF_tab_count %>% gather(year, ha_AF, -c(1:4))
AF_count_reshape <- AF_count_reshape %>% mutate(ha_AF = ha_AF * 1/10^6)
# filtering NA
AF_count_reshape <- AF_count_reshape %>% filter_at(.vars = vars(PLAN, ADMIN), all_vars(!is.na(.))) %>% filter(PLAN != "No Data")
write.csv(AF_count_reshape, "AF_milHA.csv", row.names = FALSE)

# visualization: geom_bar(stat = "identity", position = "dodge")
plotAF_total <- ggplot(AF_count_reshape, aes(x = year, y = ha_AF)) + geom_bar(stat = "identity") + ylab("million Hectares")
ggsave("AFtotalPapuaHA.png", width=6,height=6,dpi=150) 
plotAF_peatMineral <- plotAF_total + facet_grid(~ PEAT)
ggsave("AFpeatPapuaHA.png", width=6,height=6,dpi=150)
plotAFadat <- plotAF_total + facet_grid(~ PLAN)
ggsave("AFadatPapuaHA.png", width=14,height=6,dpi=150)
plotAFrtr <- plotAF_total + facet_grid(~ ADMIN)
ggsave("AFrtrPapuaHA.png", width=11,height=6,dpi=150)
# per area \ends----

# summary table====
AF_total_pct <- AF_tab_pct %>% group_by(LC_T2) %>% summarize_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(sum))

# renaming AF_tab
oldCnames <- AF_total_pct %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- AF_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# AF_tabRename <- AF_tab %>% rename_at(newCnames)
AF_total_pct <- AF_total_pct %>% rename_at(vars(oldCnames), ~ newCnames)
write.csv(AF_total_pct, "AFtotPct.csv", row.names = FALSE)
# summary table \ends----
# 1. AF \ends----

# 3. Percentage of Tree Cover====
tree_tab <- mother_tab %>% left_join(treeClass_lu, by = "LC_T2") %>% filter_at(.vars = vars(ID_LC_T1, ID_LC_T2), all_vars(! . %in% c(0,16,17)))

tree_tab_pct <- tree_tab %>% mutate_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(.*100/sum(.))) %>% filter(TreeCategory == "Tree") %>% select(ADMIN, PLAN, PEAT, TreeCategory, !!paste0("COUNT", seq(2018, 2050, by = 8)))

tree_tab_count <- tree_tab %>% filter(TreeCategory == "Tree") %>% select(ADMIN, PLAN, PEAT, LC_T2, !!paste0("COUNT", seq(2018, 2050, by = 8)))

# renaming year columns====
oldCnames <- tree_tab_count %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# tree_tabRename <- tree_tab %>% rename_at(newCnames)
tree_tab_count <- tree_tab_count %>% rename_at(vars(oldCnames), ~ newCnames)

# per area classification (in million hectares)====
tree_count_reshape <- tree_tab_count %>% gather(year, ha_tree, -c(1:4))
tree_count_reshape <- tree_count_reshape %>% mutate(ha_tree = ha_tree * 1/10^6)
# filtering NA
tree_count_reshape <- tree_count_reshape %>% filter_at(.vars = vars(PLAN, ADMIN), all_vars(!is.na(.))) %>% filter(PLAN != "No Data")
write.csv(tree_count_reshape, "tree_milHA.csv", row.names = FALSE)

# visualization: geom_bar(stat = "identity", position = "dodge")
plottree_total <- ggplot(tree_count_reshape, aes(x = year, y = ha_tree, fill = LC_T2)) + geom_bar(stat = "identity", position = "stack") + ylab("million Hectares") + guides(fill=guide_legend(title="Land Cover"))
ggsave("treetotalPapuaHA.png", width=6,height=6,dpi=150)
plottree_peatMineral <- plottree_total + facet_grid(~ PEAT)
ggsave("treepeatPapuaHA.png", width=7.5,height=6,dpi=150)
plottreeadat <- plottree_total + facet_grid(~ PLAN)
ggsave("treeadatPapuaHA.png", width=16,height=6,dpi=150)
plottreertr <- plottree_total + facet_grid(~ ADMIN)
ggsave("treertrPapuaHA.png", width=14,height=6,dpi=150)
# per area \ends----

# summary table====
tree_total_pct <- tree_tab_pct %>% group_by(TreeCategory) %>% summarize_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(sum))

# renaming tree_tab
oldCnames <- tree_total_pct %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# tree_tabRename <- tree_tab %>% rename_at(newCnames)
tree_total_pct <- tree_total_pct %>% rename_at(vars(oldCnames), ~ newCnames)
write.csv(tree_total_pct, "treetotPct.csv", row.names = FALSE)
# summary table \ends----
# 1. tree \ends----

# misc. porcess====
treeClass_lu <- mother_tab %>% select(LC_T2) %>% group_by(LC_T2) %>% summarise() %>% mutate(TreeCategory = "") %>% edit()
# mis. proc 1\end-----
# ADhere
# left_join to assign categories to the lc_t2====
tree_tab <- tree_tab %>% left_join(commClass_lu, by = "LC_T2")
tree_tab <- mother_tab %>% mutate_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(.*100/sum(.))) %>% filter(LC_T2 == "Agroforest")
tree_tab <- tree_tab %>% group_by(LC_T2) %>% summarize_at(.vars = which(grepl("^COUNT2", names(.))), .funs = funs(sum))

# renaming tree_tab
oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# tree_tabRename <- tree_tab %>% rename_at(newCnames)
tree_tab <- tree_tab %>% rename_at(vars(oldCnames), ~ newCnames)
# 3. \ends----

# 2. Deforestation====
setwd("D:/GG_Papua/process/lumens_dir/IDH/Result/")
# transition from classes 1:6 into >= 7; after omission of nodata etc.
deforTab <- mother_tab %>% filter_at(.vars = vars(ID_LC_T1, ID_LC_T2), all_vars(! . %in% c(0,16,17))) %>% mutate(def_BL = case_when(ID_LC_T1 < 7 & ID_LC_T2 >= 7 ~ "Deforestasi", TRUE ~ ""))
deforTab_count <- deforTab %>% filter(def_BL == "Deforestasi") %>% select(ADMIN, PLAN, PEAT, LC_T1, LC_T2, !!paste0("COUNT", seq(2018, 2050, by = 8)), def_BL)

# Calculate the annual rate (Ha/year)====
deforTab_rate <- deforTab_count %>% mutate_at(.vars = vars(!!paste0("COUNT", seq(2018, 2050, by = 8))), .funs = funs(.*1/5))
# deforTab_rate <- deforTab_rate %>% mutate(COUNT2018 = COUNT2018*1/8)
# # annual rate\ends----


# renaming year columns====
oldCnames <- deforTab_rate %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# tree_tabRename <- tree_tab %>% rename_at(newCnames)
deforTab_rate <- deforTab_rate %>% rename_at(vars(oldCnames), ~ newCnames)

# total sumarry====
deforTot_annualHa <- deforTab_rate %>% group_by(def_BL) %>% summarize_at(.vars = which(grepl("^2", names(.))), .funs = funs(sum))
# saving
write.csv(deforTot_annualHa, "annualDefor_summary.csv", row.names = FALSE)
# total summary\ends----

# per area classification (in hectares/year)====
deforHa_reshape <- deforTab_rate %>% select(-ncol(.)) %>% gather(year, annHA, -c(1:5)) %>% filter_at(.vars = vars(PLAN, ADMIN), all_vars(!is.na(.))) %>% filter(PLAN != "No Data")
write.csv(deforHa_reshape, "annualHa_defor_all.csv", row.names = FALSE)

# plotting====
# visualization: geom_bar(stat = "identity", position = "dodge")
plotdefor_total <- ggplot(deforHa_reshape, aes(x = year, y = annHA, fill = LC_T2)) + geom_bar(stat = "identity", position = "stack") + ylab("Hectares/year") + guides(fill=guide_legend(title="Land Cover"))
ggsave("defortotalPapuaHA.png", width=6,height=6,dpi=150)
plotdefor_peatMineral <- plotdefor_total + facet_grid(~ PEAT)
ggsave("deforpeatPapuaHA.png", width=7.5,height=6,dpi=150)
plotdeforadat <- plotdefor_total + facet_grid(~ PLAN)
ggsave("deforadatPapuaHA.png", width=16,height=6,dpi=150)
plotdeforrtr <- plotdefor_total + facet_grid(~ ADMIN)
ggsave("deforrtrPapuaHA.png", width=14,height=6,dpi=150)
# per area \ends----



# 2. \ends----

# 2. Degradation====
# transition from classes 1:6 into >= 7; after omission of nodata etc.
degradTab <- mother_tab %>% filter_at(.vars = vars(ID_LC_T1, ID_LC_T2), all_vars(! . %in% c(0,16,17))) %>% mutate(def_BL = case_when(ID_LC_T1 %in% c(1,3,5) & ID_LC_T2 %in% c(2,4,6) ~ "Degradation", ID_LC_T1 %in% c(2, 4, 6) & ID_LC_T2 == 11 ~ "Degradation", TRUE ~ ""))
degradTab_count <- degradTab %>% filter(def_BL == "Degradation") %>% select(ADMIN, PLAN, PEAT, LC_T1, LC_T2, !!paste0("COUNT", seq(2018, 2050, by = 8)), def_BL)

# Calculate the annual rate (Ha/year)====
degradTab_rate <- degradTab_count %>% mutate_at(.vars = vars(!!paste0("COUNT", seq(2018, 2050, by = 8))), .funs = funs(.*1/5))
# degradTab_rate <- degradTab_rate %>% mutate(COUNT2018 = COUNT2018*1/8)
# # annual rate\ends----


# renaming year columns====
oldCnames <- degradTab_rate %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- tree_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# tree_tabRename <- tree_tab %>% rename_at(newCnames)
degradTab_rate <- degradTab_rate %>% rename_at(vars(oldCnames), ~ newCnames)

# total sumarry====
degradTot_annualHa <- degradTab_rate %>% group_by(def_BL) %>% summarize_at(.vars = which(grepl("^2", names(.))), .funs = funs(sum))
# saving
write.csv(degradTot_annualHa, "annualDegrad_summary.csv", row.names = FALSE)
# total summary\ends----

# per area classification (in hectares/year)====
degradHa_reshape <- degradTab_rate %>% select(-ncol(.)) %>% gather(year, annHA, -c(1:5)) %>% filter_at(.vars = vars(PLAN, ADMIN), all_vars(!is.na(.))) %>% filter(PLAN != "No Data")
write.csv(degradHa_reshape, "annualHa_degrad_all.csv", row.names = FALSE)

# plotting====
# visualization: geom_bar(stat = "identity", position = "dodge")
plotdegrad_total <- ggplot(degradHa_reshape, aes(x = year, y = annHA, fill = LC_T2)) + geom_bar(stat = "identity", position = "stack") + ylab("Hectares/year") + guides(fill=guide_legend(title="Land Cover"))
ggsave("degradtotalPapuaHA.png", width=6,height=6,dpi=150)
plotdegrad_peatMineral <- plotdegrad_total + facet_grid(~ PEAT)
ggsave("degradpeatPapuaHA.png", width=7.5,height=6,dpi=150)
plotdegradadat <- plotdegrad_total + facet_grid(~ PLAN)
ggsave("degradadatPapuaHA.png", width=16,height=6,dpi=150)
plotdegradrtr <- plotdegrad_total + facet_grid(~ ADMIN)
ggsave("degradrtrPapuaHA.png", width=14,height=6,dpi=150)
# per area \ends----



# 2. \ends----

# saving the deforTab_rate & degradTab_rate in long format
write.csv(deforTab_rate, "deforAnnRate_wide.csv", row.names = FALSE)
write.csv(degradTab_rate, "degradAnnRate_wide.csv", row.names = FALSE)


