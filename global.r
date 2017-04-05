############ global.R #############
#### All processing that could be done once goes here, because processing in server.R is done everytime the app loads, and drastically slows it down
#### Objects created here are also available to both the ui.R script and the server.R script

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

print("Running global.R...")

library(magrittr)
library(shiny)
library(highcharter)
library(leaflet)
library(shinythemes)
library(zoo)
library(sp)
#library(rgdal)
#Also depends on shinyjs, DT

#source("Modules/wasteModule.R")
#source("Modules/energyModule.R")
source("modules.R")

# Set thousands seperator in highcharts graph
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


enableBookmarking(store="url")


##########SETUP Google Sheets
## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_token.rds")

## if you version control your app, don't forget to ignore the token file!
# e.g., put it into .gitignore

load(file="./data/appData.RData")


  mapIconSize <- 40

  mapIcons <- iconList(
    leed = makeIcon(iconUrl="assets/UWIcons/1l0-e0-e0-d-certification-icon.png",
                    iconWidth=mapIconSize, iconHeight = mapIconSize,
                    iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),

    solar = makeIcon(iconUrl="assets/UWIcons/1solar-panels-icon.png",
                     iconWidth=mapIconSize, iconHeight = mapIconSize,
                     iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),

    garden = makeIcon(iconUrl="assets/UWIcons/3garden-icon.png",
                      iconWidth=mapIconSize, iconHeight = mapIconSize,
                      iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),

    tree = makeIcon(iconUrl="assets/UWIcons/3brockman-tree-tour-icon.png",
                    iconWidth=mapIconSize, iconHeight = mapIconSize,
                    iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),

    compost = makeIcon(iconUrl="assets/UWIcons/6on-site-composting-icon.png",
                       iconWidth=mapIconSize, iconHeight = mapIconSize,
                       iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize)
  )

  leedImages <- list(
    "LEED Gold" = "assets/UWIcons/leedGold.png",
    "LEED Silver" = "assets/UWIcons/leedSilver.jpg"
  )
