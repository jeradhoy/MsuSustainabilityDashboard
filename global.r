############ global.R #############
#### All processing that could be done once goes here, because processing in server.R is done everytime the app loads, and drastically slows it down
#### Objects created here are also available to both the ui.R script and the server.R script

print("Running global.R...")

library(magrittr)

library(shiny)
library(highcharter)
library(leaflet)
library(shinythemes)

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


####DECLARE ANY FUNCTIONS FOR APPJ
#Get trend timeseries for plotting
getTrendSeries <- function(timeSeries, startTs=c(2005, 1), freq=12){
  ts(as.data.frame(lapply(timeSeries, function(timeSeries){
    fit <- lm(timeSeries ~ c(1:length(timeSeries)))
    seq(from=coef(fit)[1], by=coef(fit)[2], length.out=length(timeSeries))
  })),frequency=12, start=startTs)
}
