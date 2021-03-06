#Download R data
library(magrittr)
library(googlesheets)
library(tidyverse)
library(zoo)

####DECLARE ANY FUNCTIONS FOR APP
#Get trend timeseries for plotting
getTrendSeries <- function(timeSeries){
 as.ts(zoo(as.data.frame(
   lapply(timeSeries, function(timeSeries){
    fit <- lm(timeSeries ~ c(1:length(timeSeries)))
    seq(from=coef(fit)[1], by=coef(fit)[2], length.out=length(timeSeries))
    }
  )), order.by=index(timeSeries)))
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
appData$energy$GasMMBTU <- round(appData$energy$GasDKT*999761/1e6) #Convert DKT to KWH

appData$energy %<>% mutate(Date = as.yearmon(ACCTYR + (ACCTMO-1)/12))

appData$energyTs <- as.list(as.ts(zoo(select(appData$energy, ElecKW:GasMMBTU), order.by=appData$energy$Date)))

#annualEnergyTs <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)
#pcEnergy <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)
#energyTarget <- ts(aggregate(energyTs, nfrequency=1, FUN=mean)*1.0025, frequency=1, start=c(2011, 1))

######  Waste Data  #####
appData$waste$Recycle <- as.numeric(format(round(appData$waste$Recycle/2000, 2), nsmall=2))

appData$waste$Landfill <-as.numeric(format(round(appData$waste$Landfill/2000, 2), nsmall=2))

appData$waste$Compost <- as.numeric(format(round(appData$waste$Compost/2000, 2), nsmall=2))

appData$waste %<>% mutate(Date = as.yearmon(Year + (Month-1)/12))
appData$wasteTs <- as.list(as.ts(zoo(select(appData$waste, Recycle:Compost), order.by=appData$waste$Date)))

### Per capita waste data
#perCapita$FY <- as.numeric(perCapita$FY)
#perCapita$pcrecycle <- as.numeric(format(round(perCapita$recycling/perCapita$fallpop, 2), nsmall=2))
#perCapita$pcwaste <- as.numeric(format(round(perCapita$waste/perCapita$fallpop, 2), nsmall=2))
#perCapita$pccompost <- as.numeric(format(round(perCapita$compost/perCapita$fallpop, 2), nsmall=2))


appData$buildingShapes <- rgdal::readOGR("./data/Buildings/building.shp", layer="building", verbose=F)

appData$buildingUtilities %<>% mutate(Date = as.yearmon(ACCTYR + (ACCTMO-1)/12))

bld <- appData$buildingUtilities %>% nest(-BldgNo, -BldgName)

bld$data <- lapply(bld$data, function(dat){
  tryCatch({
    as.list(as.ts(zoo(select(dat, ElecKW:WaterMCF), dat$Date)))
  }, error=function(cond){NULL}
  )
})

appData$bld <- bld

save(appData, file="./data/appData.RData")
