##SCIENDO=group
##workingDirectory=folder
##numberOfStockTable=file
#persenPertambahan=number 1.006   <-- future parameter
##emissionFactorCH4FromEntericTable=file
##emissionFactorCH4FromManureTable=file
##baseYear=number 2010
##timeStep=number 1
##iteration=number 30
##directEmissionN2OLUT=file
##indirectEmissionN2OLUT=file

#projectionMode=selection Linear; Exponential; Quadratic; Absolute value <-- future parameter


library(reshape)

# set initial variable
setwd('C:/2_Recent/SCIENDO/Livestock_Agriculture/')
numberOfStockTable <- read.table('L_SCIENDO_JumlahTernak.csv', header=T, sep=',')
emissionFactorCH4FromEntericTable <- read.table('L_SCIENDO_Faktor_emisiCH4_fermentasi_enterik.csv', header=T, sep=',')
emissionFactorCH4FromManureTable <- read.table('L_SCIENDO_Faktor_emisiCH4_pengelolaan_kotoran_ternak.csv', header=T, sep=',')
baseYear <- 2010
timeStep <- 1
iteration <- 20
directEmissionN2OLUT <- read.table('L_SCIENDO_DirectEmissionN2O_LUT.csv', header=T, sep=',')
indirectEmissionN2OLUT <- read.table('L_SCIENDO_IndirectEmissionN2O_LUT.csv', header=T, sep=',')

projectionMode <- 0 # c('linear', 'exponential', 'quadratic', 'absolute' )


# uniform table's name
colnames(numberOfStockTable)<-c('ID', 'TERNAK', baseYear:(baseYear+iteration))
colnames(emissionFactorCH4FromEntericTable)<-c('ID', 'TERNAK', 'ENTERIC')
colnames(emissionFactorCH4FromManureTable)<-c('ID', 'TERNAK', 'MANURE')
colnames(directEmissionN2OLUT)<-c('ID', 'TERNAK', 'EKSKRESI', 'MASSA', 'FRAC_EX', 'DIRECT')
colnames(indirectEmissionN2OLUT)<-c('ID', 'TERNAK', 'FRAC_GASMS', 'INDIRECT')

# All about projection
# baseline projection
if(projectionMode == 0) {
  projectionModeType = 'linear'
  
  # increasing about 10% of livestock for each iteration 
  # for(i in 0:(iteration-1)){
  #   stockForNextYear <- numberOfStockTable[,3+i] + numberOfStockTable[,3+i] * 0.1
  #   numberOfStockTable <- cbind(numberOfStockTable, stockForNextYear)
  #   colnames(numberOfStockTable)[3+i+1] <- periodOfStock + i +  1
  # }
}

if(projectionMode == 1) {
  projectionModeType = 'exponential'
}

if(projectionMode == 2) {
  projectionModeType = 'quadratic'
}

if(projectionMode == 3) {
  projectionModeType = 'absolute'
}

numberOfStockTableMelt <- melt(data = numberOfStockTable, id.vars=c('ID', 'TERNAK'))

#  PERHITUNGAN EMISI METANA DARI FERMENTASI ENTERIK DAN 
#  PENGELOLAAN LIMBAH TERNAK
# -------------------------------------------------------------
#
#  T = Jenis Ternak T
#  N(T) = Jumlah Ternak sebanyak N 
#  EF(T) = Emission Factor dari Fermentasi Enterik (kg/ekor/th)
#  
#
#  Emisi CH4 dari Fermentasi Enterik (Gg CH4/th)
#      CH4(enterik) = N(T)*EF(T)*10^-6 
#
#  Emisi CH4 dari Pengelolaan Kotoran Ternak (Gg CH4/th)
#      CH4(manure) = N(T)*EF(T)*10^-6 
#
#  Total Emisi CH4(total) = CH4(enterik) + CH4(manure)
#
#
# Calculate emission
# CH4 emission from enteric fermentation
numberOfStockTableMeltAndEnteric <- merge(numberOfStockTableMelt, emissionFactorCH4FromEntericTable, by=c('ID','TERNAK'))
numberOfStockTableMeltAndEnteric$emission <- numberOfStockTableMeltAndEnteric$value * numberOfStockTableMeltAndEnteric$ENTERIC * 10^-6
numberOfStockTableMeltAndEnteric$value<-numberOfStockTableMeltAndEnteric$ENTERIC<-NULL
colnames(numberOfStockTableMeltAndEnteric)[4] <- 'value'
# emissionResultFromEnteric <- cast(numberOfStockTableMeltAndEnteric)

# CH4 emission from manure management
numberOfStockTableMeltAndManure <- merge(numberOfStockTableMelt, emissionFactorCH4FromManureTable, by=c('ID','TERNAK'))
numberOfStockTableMeltAndManure$emission <- numberOfStockTableMeltAndManure$value * numberOfStockTableMeltAndManure$MANURE * 10^-6
numberOfStockTableMeltAndManure$value<-numberOfStockTableMeltAndManure$MANURE<-NULL
colnames(numberOfStockTableMeltAndManure)[4] <- 'value'
# emissionResultFromManure <- cast(numberOfStockTableMeltAndManure)

totalEmissionCH4<-merge(numberOfStockTableMeltAndEnteric, numberOfStockTableMeltAndManure, by=c("ID", "TERNAK", "variable"), all=TRUE)
colnames(totalEmissionCH4)[4] <- 'Emisi_FE'
colnames(totalEmissionCH4)[5] <- 'Emisi_MM'
totalEmissionCH4[is.na(totalEmissionCH4)] <- 0
totalEmissionCH4$value <- totalEmissionCH4$Emisi_FE + totalEmissionCH4$Emisi_MM

emissionResult <- subset(totalEmissionCH4, select=c(ID, TERNAK, variable, value))
emissionResult <- cast(emissionResult)



#  PERHITUNGAN EMISI LANGSUNG DAN TAK LANGSUNG N20 DARI  
#  PENGELOLAAN LIMBAH TERNAK
# -------------------------------------------------------------
#
#  T = Jenis Ternak T
#  N(T) = Jumlah Ternak sebanyak N 
#  N-rate(T) = Laju ekskresi Nitrogen (kg N/1000kg berat ternak/hari)
#  TAM = Massa jenis per ternak (kg)
#  N-ex(T) = Rata-rata tahunan N yang diekskresikan (kg N/ternak/th)
#     N-ex(T) = N-rate(T)*(TAM/1000)*365
#  MS(T,S) = Fraksi N total tahunan dari ekskresi untuk setiap spesies
#  NE(MMS) = Total N dari ekskresi MMS (kg N/th)
#     NE(MMS) = N(T)*N-ex(T)*MS(T,S)
#  EF3(S) = Emission Factor dari Emisi N2O-N secara langsung dari MMS (kg N2O-N/kg N dalam MMS)
# 
#  Emisi N2O langsung tahunan dari pengelolaan kotoran hewan ternak untuk pupuk (Gg N2O/th)
#     N2O-D(MM) = NE(MMS)*EF3(S)*44/28
#
# direct
directEmissionN2OLUT$RATE <- directEmissionN2OLUT$EKSKRESI * (directEmissionN2OLUT$MASSA/10^3) * 365 
directEmissionN2OLUT$EKSKRESI<-directEmissionN2OLUT$MASSA<-NULL
numberOfStockTableMeltAndNO2Direct <- merge(numberOfStockTableMelt, directEmissionN2OLUT, by=c('ID','TERNAK'))
# numberOfStockTableMeltAndNO2Direct$FRAC <- 0.02 # 2 %
# Total_N_from_excretion_MMS = sigma( N(T)*Nex(T)*Frac ) 
numberOfStockTableMeltAndNO2Direct$TOTAL_N <- numberOfStockTableMeltAndNO2Direct$value * numberOfStockTableMeltAndNO2Direct$RATE * numberOfStockTableMeltAndNO2Direct$FRAC_EX
numberOfStockTableMeltAndNO2Direct$NO2Direct <- numberOfStockTableMeltAndNO2Direct$TOTAL_N * numberOfStockTableMeltAndNO2Direct$DIRECT * 44/28
emissionResultFromNO2Direct <- subset(numberOfStockTableMeltAndNO2Direct, select=c(ID, TERNAK, variable, NO2Direct))
colnames(emissionResultFromNO2Direct)[4] <- 'value'
emissionResultFromNO2Direct <- cast(emissionResultFromNO2Direct)

#  Frac(GasMS) = Fraksi N dari pengelolaan kotoran hewan untuk pupuk yang tervolatilisasi
#  EF4(S) = emission factor N2O (kg N2O-N [kgNH3-N+NOx-N])
#  N-volatilisasi MMS = Jumlah N yang hilang karena tervolatilisasi menjadi NH3 & NOx    
#     N-volatilisasi MMS = NE(MMS)*Frac(GasMS)
# 
#  Emisi N2O tidak langsung karena tervolatilisasi dari pengelolaan kotoran untuk pupuk
#     N2O-G(MM) = N-volatilisasi MMS*EF4*44/28
#
# indirect
numberOfStockTableMeltAndNO2Indirect <- merge(numberOfStockTableMeltAndNO2Direct, indirectEmissionN2OLUT, by=c('ID','TERNAK'))
numberOfStockTableMeltAndNO2Indirect$NO2Indirect <- numberOfStockTableMeltAndNO2Indirect$TOTAL_N * numberOfStockTableMeltAndNO2Indirect$FRAC_GASMS * numberOfStockTableMeltAndNO2Indirect$INDIRECT * 44/28
emissionResultFromNO2Indirect <- subset(numberOfStockTableMeltAndNO2Indirect, select=c(ID, TERNAK, variable, NO2Indirect))
colnames(emissionResultFromNO2Indirect)[4] <- 'value'
emissionResultFromNO2Indirect <- cast(emissionResultFromNO2Indirect)

# write report
