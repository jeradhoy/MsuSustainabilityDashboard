#Download R data
library(googlesheets)
library(tidyverse)
library(zoo)
library(magrittr)

####DECLARE ANY FUNCTIONS FOR APP
#Get trend timeseries for plotting
getTrendSeries <- function(timeSeries){
 as.ts(zoo(as.data.frame(lapply(timeSeries, function(timeSeries){
    fit <- lm(timeSeries ~ c(1:length(timeSeries)))
    seq(from=coef(fit)[1], by=coef(fit)[2], length.out=length(timeSeries))
  })), order.by=index(timeSeries)))
}

##Read in data from google sheets gs_auth(token = "shiny_app_token.rds")
allDataSheet <- gs_title("allData")
appData <- list()

for(sheet in gs_ws_ls(allDataSheet)){
  appData[[sheet]] <- allDataSheet %>% gs_read_csv(ws=sheet) %>% as.data.frame()
  Sys.sleep(5)
}

#Process energy data, convert to time series, convert dkt to kwh, calculate energy trends
appData$energy$GasKWH <- round(appData$energy$GasDKT/0.0034129563407) #Convert DKT to KWH

appData$energy %<>% mutate(Date = as.yearmon(ACCTYR + (ACCTMO-1)/12))

appData$energyTs <- as.ts(zoo(select(appData$energy, ElecKW:GasKWH), order.by=appData$energy$Date))

#annualEnergyTs <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

#pcEnergy <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

#energyTarget <- ts(aggregate(energyTs, nfrequency=1, FUN=mean)*1.0025, frequency=1, start=c(2011, 1))

######  Waste Data  #####
appData$waste$Recycle <- as.numeric(format(round(appData$waste$Recycle/2000, 2), nsmall=2))

appData$waste$Landfill <-as.numeric(format(round(appData$waste$Landfill/2000, 2), nsmall=2))

appData$waste$Compost <- as.numeric(format(round(appData$waste$Compost/2000, 2), nsmall=2))

appData$waste %<>% mutate(Date = as.yearmon(Year + (Month-1)/12))
appData$wasteTs <- as.ts(zoo(select(appData$waste, Recycle:Compost), order.by=appData$waste$Date))

### Per capita waste data
#perCapita$FY <- as.numeric(perCapita$FY)
#perCapita$pcrecycle <- as.numeric(format(round(perCapita$recycling/perCapita$fallpop, 2), nsmall=2))
#perCapita$pcwaste <- as.numeric(format(round(perCapita$waste/perCapita$fallpop, 2), nsmall=2))
#perCapita$pccompost <- as.numeric(format(round(perCapita$compost/perCapita$fallpop, 2), nsmall=2))


appData$energyTrends <- getTrendSeries(appData$energyTs)

appData$wasteTrends <- getTrendSeries(appData$wasteTs)

appData$buildingShapes <- rgdal::readOGR("./data/Buildings/building.shp", layer="building", verbose=F)

appData$buildingUtilities %<>% mutate(Date = as.yearmon(ACCTYR + (ACCTMO-1)/12))

bld <- appData$buildingUtilities %>% nest(-BldgNo, -BldgName)

bld$data <- lapply(bld$data, function(dat){
  tryCatch({
    as.ts(zoo(select(dat, ElecKW:WaterMCF), dat$Date))
  }, error=function(cond){NULL}
  )
})

appData$bld <- bld

#appData$bldTs <- list()
#
#for(num in as.character(na.omit(unique(bld$Bldg.No)))){
#  print(num)
#  appData$bldTs[[num]] <- with(subset(bld, Bldg.No == num), ts(data=cbind(KWH.QTY, TOTAL.GAS..DKT, WATER.MCF, STEAM.LBS), start=c(ACCTYR[1], ACCTMO[1]), frequency=12))
#}

save(appData, file="./data/appData.RData")
