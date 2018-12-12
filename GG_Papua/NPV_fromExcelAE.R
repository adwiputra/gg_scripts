# GG Papua
# Analysis of tables to derive the profitability

library(tidyverse)
# library(officer)
setwd("D:/GG_Papua/data/tabular/")

# 0. getting hands on read_excel function=====
mother_tab <- readxl::read_excel("LUMENSHist_database_5yearly.xlsx", sheet = 1) %>% data.frame(stringsAsFactors = FALSE)
# 0. \end====

# left_join(), mutate_at + .funs = funs(), group_by + summarize(), 
NPV_tab <- mother_tab %>% mutate_at(which(grepl("^COUNT", names(.))), .funs = funs(. * PROF_T2))
# rename columns
# define vectors to assign the naming
oldCnames <- NPV_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
newCnames <- oldCnames %>% gsub("COUNT", "", .)
# oldCnames <- NPV_tab %>% select(which(grepl("^COUNT2", names(.)))) %>% names()
# NPV_tabRename <- NPV_tab %>% rename_at(newCnames)
NPV_tab <- NPV_tab %>% rename_at(vars(oldCnames), ~ newCnames)
# Select prior reshape
NPV_tab <- NPV_tab %>% select(ADMIN, PLAN, PEAT, LC_T2, !!paste0(seq(2018, 2048, by = 5)))


# Mis. process 1: generate classification scheme for commodity====
commClass_lu <- mother_tab %>% select(LC_T2) %>% group_by(LC_T2) %>% summarise() %>% mutate(Category = "") %>% edit()
# mis. proc 1\end-----

# left_join to assign categories to the lc_t2====
NPV_tab <- NPV_tab %>% left_join(commClass_lu, by = "LC_T2") %>% filter_at(.vars = vars(PLAN, ADMIN), all_vars(!is.na(.))) %>% filter(Category != "")
NPV_tab <- NPV_tab %>% mutate_at(.vars = vars(!!paste0(seq(2018, 2048, by = 5))), .funs = funs(if_else(. < 0, 0, .)))
write.csv(NPV_tab, "NPV_wide.csv", row.names = FALSE)
# left_join\ends----

# reshape, change unit into million USD
NPV_tabRes <- NPV_tab %>% gather(year, npv, -c(1:4, 12)) %>% mutate(npv = npv/10^6)

# reshape \ends----

# visualization: geom_bar(stat = "identity", position = "dodge")
plot_total <- ggplot(NPV_tabRes, aes(x = year, y = npv, fill = Category)) + geom_bar(stat = "identity", position = "stack") + ylab("million USD") + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
ggsave("NPVtotalPapuamillUSD.png", width=6,height=6,dpi=150)
plot_peatMineral <- plot_total + facet_grid(~ PEAT)
ggsave("NPVpeatPapuamillUSD.png", width=6,height=6,dpi=150)
plotAdat <- plot_total + facet_grid(~ PLAN)
ggsave("NPVadatPapuamillUSD.png", width=14,height=6,dpi=150)
plotrtr <- plot_total + facet_grid(~ ADMIN)
ggsave("NPVrtrPapuamillUSD.png", width=11,height=6,dpi=150)
