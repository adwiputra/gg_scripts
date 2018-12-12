##SCIENDO=group
##workingDirectory=folder
##areaOfAgricTable=file
##periodOfStock=number 2010
##iteration=number 10


library(reshape)

# set initial variable
setwd('C:/2_Recent/SCIENDO/Livestock_Agriculture/')
areaOfAgricTable <- read.table('A_SCIENDO_Luas_sawah.csv', header=T, sep=',')

# periodOfStock <- 2010
# iteration <- 10

# uniform table's name
colnames(areaOfAgricTable)<-c('TAHUN', 'SAWAH_IRIGASI', 'TADAH_HUJAN', 'LUAS_PANEN')

# TOTAL LUAS SAWAH (ha/yr) 
# A = Luas Sawah Irigasi + Luas Sawah Tadah Hujan
areaOfAgricTable$LUAS_TOTAL <- areaOfAgricTable$SAWAH_IRIGASI + areaOfAgricTable$TADAH_HUJAN

# MASA TANAM DALAM SETAHUN (Index Penanaman Irigasi)
# IP Irigasi (BASELINE) = (Luas Panen - Luas Tadah Hujan) / Luas Sawah Irigasi 
areaOfAgricTable$IP_IRIGASI1 <- (areaOfAgricTable$LUAS_PANEN - areaOfAgricTable$TADAH_HUJAN) / areaOfAgricTable$SAWAH_IRIGASI
# IP Irigasi (PROYEKSI BASELINE) = Luas Panen / Luas Sawah Irigasi 
areaOfAgricTable$IP_IRIGASI2 <- areaOfAgricTable$LUAS_PANEN / areaOfAgricTable$SAWAH_IRIGASI

# SFw = Faktor skala lahan sawah irigasi intermitten (untuk perbedaan rejim air selama pertanaman) 
#       0.49 x Luas Sawah Tadah Hujan + (Luas Panen - Luas Sawah Tadah Hujan) x 1 
# SFW = -------------------------------------------------------------------------
#                                       15
areaOfAgricTable$