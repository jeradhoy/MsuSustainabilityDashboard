############ global.R #############
#### All processing that could be done once goes here, because processing in server.R is done everytime the app loads, and drastically slows it down
#### Objects created here are also available to both the ui.R script and the server.R script

library(magrittr)
library(tidyverse)
dataDir <- "./data/Gdrive/"

enableBookmarking(store="url")


####DECLARE ANY FUNCTIONS FOR APPJ
#Get trend timeseries for plotting
getTrendSeries <- function(timeSeries, startTs=c(2005, 1), freq=12){
	ts(as.data.frame(lapply(timeSeries, function(timeSeries){
		fit <- lm(timeSeries ~ c(1:length(timeSeries)))
		seq(from=coef(fit)[1], by=coef(fit)[2], length.out=length(timeSeries))
	})),frequency=12, start=startTs)
}


##########SETUP Google Sheets
## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_token.rds")

## if you version control your app, don't forget to ignore the token file!
# e.g., put it into .gitignore


#Read in data from each file in ./data/Gdrive (dataDir) folder
for(file in list.files(dataDir)){
  assign(x=tools::file_path_sans_ext(file), value=read.csv(paste0(dataDir, file), stringsAsFactors=F))
  #assign(x=tools::file_path_sans_ext(file), value=read_csv(paste0(dataDir, file)))
}

#Process energy data, convert to time series, convert dkt to kwh, calculate energy trends
#energyTimeSeries <- ts(energy[,-c(1,2,3,6,9)], frequency=12, start=c(2005, 1)) #Convert to time series
energy$GAS.KWH <- round(energy$GAS.DKT.Units/0.0034129563407) #Convert DKT to KWH

energyTs <- ts(energy[,-c(1,2)], frequency=12, start=c(energy$ACCTYR[1], energy$ACCTMO[1])) #Convert to time series

#colnames(energyTimeSeries) <- c("elecKWH", "gasKWH", "elecExpend", "gasExpend") #

#waterSewerTimeSeries <- ts(energy[,c(6,9)], frequency=12, start=c(2005, 1)) #Convert to time series
#colnames(waterSewerTimeSeries) <- c("waterMCF", "waterSewerExpend") #

#energyTimeSeries <- ts(energy[,-c(1,2,3,6,9,10,11)], frequency=12, start=c(2005, 1)) #Convert to time series
#energyTimeSeries[,2] <- round(energyTimeSeries[,2]/0.0034129563407) #Convert DKT to KWH
#colnames(energyTimeSeries) <- c("elecKWH", "gasKWH", "elecExpend", "gasExpend") #

annualEnergyTs <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

pcEnergy <- round(aggregate(energyTs, nfrequency=1, FUN=sum)/perCapita[5:10,2],2)

energyTarget <- ts(aggregate(energyTs, nfrequency=1, FUN=mean)*1.0025, frequency=1, start=c(2011, 1))

######  Waste Data  #####
waste$recycle <- as.numeric(format(round(waste$recycle/2000, 2), nsmall=2))
waste$landfill <-as.numeric(format(round(waste$landfill/2000, 2), nsmall=2))
waste$compost <- as.numeric(format(round(waste$compost/2000, 2), nsmall=2))
wasteTs <- ts(waste[,-c(1, 2)], frequency=12, start=c(2006, 1))
#recycletimeseries <- ts(waste[,-c(1, 3)], frequency = 12, start = c(2006, 1))

### Per capita waste data
perCapita$FY <- as.numeric(perCapita$FY)
perCapita$pcrecycle <- as.numeric(format(round(perCapita$recycling/perCapita$fallpop, 2), nsmall=2))
perCapita$pcwaste <- as.numeric(format(round(perCapita$waste/perCapita$fallpop, 2), nsmall=2))
perCapita$pccompost <- as.numeric(format(round(perCapita$compost/perCapita$fallpop, 2), nsmall=2))

#recyclefit <- getTrendSeries(recycletimeseries[,1], startTs = c(2006,1))
#compostfit <- getTrendSeries(composttimeseries[,3], startTs = c(2006,1))


library(rgdal)
buildingShapes <- readOGR("./data/Buildings/building.shp", layer="building", verbose=F)

#str(buildingUtilities)
#buildingUtilities[,c("KWH.QTY", "TOTAL.GAS..DKT", "WATER.MCF")]
#buildingUtilities$KWH.QTY

#buildingUtilities$KWH.QTY <- as.numeric(gsub(",", "", buildingUtilities$KWH.QTY))
#buildingUtilities$WATER.MCF <- as.numeric(buildingUtilities$WATER.MCF)

#buildingShapes <- SpatialPolygonsDataFrame(buildingShapes, as.data.frame(buildingShapes))

#buildingShapes$BLGNUM %in% unique(buildingUtilities$Bldg.No)

#buildingShapes[,1]

#buildingNoData <- subset(buildingShapes, subset=!(BLGNUM %in% unique(buildingUtilities$Bldg.No)))


#buildingNoData <- data.frame(Bldg.No=buildingNoData$BLGNUM, Building.Name=buildingNoData$Name)
#buildingUtilities <- plyr::rbind.fill(buildingUtilities, buildingNoData)
#tail(buildingUtilities)


#str(buildingUtilities)
#buildingShapes$Name

