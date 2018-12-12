# Edit table reference
# Go to pgadmin --> refresh db --> find reference table, that want to be refine
# Example to refine cstock
# Schemas --> public --> table --> list_of_data_lut --> check table cstock (in_lut_3)

# Setting up basic database parameters====
proj.file <- choose.files(default = "D:/GG_Jambi/process/lumens_dir/trial/bau_Jambi/bau_Jambi.lpj")
# Select the .lpj based on which the project shall be executed
load(proj.file)

driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname="bau_Jambi", host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

# Replace cstock table
in_lut_3 <- read.csv("D:/GG_Jambi/General/data/table/cstock_jambi.csv")
dbWriteTable(DB, "in_lut3", in_lut_3, append=TRUE, row.names=FALSE)

#done
