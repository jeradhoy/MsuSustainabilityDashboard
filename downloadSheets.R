#Download R data
library(googlesheets)
library(magrittr)

##Read in data from google sheets gs_auth(token = "shiny_app_token.rds")
allDataSheet <- gs_title("allData")
dataDir <- "./data/Gdrive/"

file.remove(paste0(dataDir, list.files(dataDir)))

for(sheet in gs_ws_ls(allDataSheet)){
  write.csv(as.data.frame(allDataSheet %>% gs_read_csv(ws=sheet)), file=paste0(dataDir, sheet, ".csv"), row.names=F)
  Sys.sleep(5)
}



dataDir <- "./data/Gdrive/"


####DECLARE ANY FUNCTIONS FOR APPJ
#Get trend timeseries for plotting
getTrendSeries <- function(timeSeries, startTs=c(2005, 1), freq=12){
  ts(as.data.frame(lapply(timeSeries, function(timeSeries){
    fit <- lm(timeSeries ~ c(1:length(timeSeries)))
    seq(from=coef(fit)[1], by=coef(fit)[2], length.out=length(timeSeries))
  })),frequency=12, start=startTs)
}




appData <- list()

#Read in data from each file in ./data/Gdrive (dataDir) folder
for(file in list.files(dataDir)){
  appData[[tools::file_path_sans_ext(file)]] <- read.csv(paste0(dataDir, file), stringsAsFactors=F)
}

#Process energy data, convert to time series, convert dkt to kwh, calculate energy trends
#energyTimeSeries <- ts(energy[,-c(1,2,3,6,9)], frequency=12, start=c(2005, 1)) #Convert to time series
appData$energy$GAS.KWH <- round(appData$energy$GAS.DKT.Units/0.0034129563407) #Convert DKT to KWH

appData$energyTs <- ts(appData$energy[,-c(1,2)], frequency=12, start=c(appData$energy$ACCTYR[1], appData$energy$ACCTMO[1])) #Convert to time series

#annualEnergyTs <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

#pcEnergy <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

#energyTarget <- ts(aggregate(energyTs, nfrequency=1, FUN=mean)*1.0025, frequency=1, start=c(2011, 1))

######  Waste Data  #####
appData$waste$recycle <- as.numeric(format(round(appData$waste$recycle/2000, 2), nsmall=2))
appData$waste$landfill <-as.numeric(format(round(appData$waste$landfill/2000, 2), nsmall=2))
appData$waste$compost <- as.numeric(format(round(appData$waste$compost/2000, 2), nsmall=2))
appData$wasteTs <- ts(appData$waste[,-c(1, 2)], frequency=12, start=c(2006, 1))
#recycletimeseries <- ts(appData$waste[,-c(1, 3)], frequency = 12, start = c(2006, 1))

### Per capita waste data
#perCapita$FY <- as.numeric(perCapita$FY)
#perCapita$pcrecycle <- as.numeric(format(round(perCapita$recycling/perCapita$fallpop, 2), nsmall=2))
#perCapita$pcwaste <- as.numeric(format(round(perCapita$waste/perCapita$fallpop, 2), nsmall=2))
#perCapita$pccompost <- as.numeric(format(round(perCapita$compost/perCapita$fallpop, 2), nsmall=2))


appData$energyTrends <- getTrendSeries(appData$energyTs, startTs=c(2005, 1), freq=12)
appData$wasteTrends <- getTrendSeries(appData$wasteTs, startTs = c(2006, 1), freq=12)

appData$buildingShapes <- rgdal::readOGR("./data/Buildings/building.shp", layer="building", verbose=F)

bld <- appData$buildingUtilities
appData$bldTs <- list()

for(num in as.character(na.omit(unique(bld$Bldg.No)))){
  print(num)
  appData$bldTs[[num]] <- with(subset(bld, Bldg.No == num), ts(data=cbind(KWH.QTY, TOTAL.GAS..DKT, WATER.MCF, STEAM.LBS), start=c(ACCTYR[1], ACCTMO[1]), frequency=12))
}

save(appData, file="./data/appData.RData")
