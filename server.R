############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R
source("modules.R")
library(tidyverse)

options(shiny.port=5555)

#Set waste goals
wasteCAP2020 <- (3933386*.75)/(2000*12)
wasteCAP2030 <- (3933386*.5)/(2000*12)
wasteCAP2040 <- (3933386*.35)/(2000*12)
wasteCAP2050 <- (3933386*.2)/(2000*12)

simpleCap <- function(x) {
  s <- strsplit(as.character(x), " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

######## START Shiny Server ###################
server <- function(input, output, session) {

  callModule(highLinePlot, "energyUsage",
             dataTs=appData$energyTs[c("ElecKWH", "GasKWH")],
             trends=T,
             plotTitle="<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Usage in KWH", colors=c("gold", "darkorange"),
             toolSuffix=" KWH", toolPrefix=NULL)

  callModule(highLinePlot, "energyExpend",
             dataTs=appData$energyTs[c("ElecExpend", "GasExpend")],
             trends=T,
             plotTitle="<b>MSU Electricity and Gas Expenditure</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Expenditure in Dollars",
             colors=c("gold", "darkorange"),
             toolSuffix=NULL, toolPrefix="$")


  callModule(highLinePlot, "wasteLine",
             dataTs=appData$wasteTs,
             trends=T,
             plotTitle="<b>MSU Waste</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
             ylab="Waste in Tons", colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL)

  callModule(highAreaPlot, "wasteArea",
             dataTs=appData$wasteTs[c(2,1,3)],
             plotTitle="<b>Percent of MSU Waste Diverted</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
             ylab="Waste in Tons",
             colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL)

  callModule(highLinePlot, "waterUse",
             dataTs=appData$energyTs["WaterMCF"],
             trends=T,
             plotTitle="<b>MSU Water Usage in Million Cubic Feet per Month</b>",
             tsNames="Water",
             ylab="Usage in Million Cubic Feet (MCF)",
             colors="blue",
             toolSuffix=" MCF", toolPrefix=NULL)

  callModule(highLinePlot, "waterSewerExpend",
             dataTs=appData$energyTs["WaterSewerExpend"],
             trends=T,
             plotTitle="<b>Msu Water/Sewer Expenditure In Dollars</b>",
             tsNames="Water/Sewer",
             ylab="Expenditure in Dollars",
             colors="green",
             toolSuffix=NULL, toolPrefix="$")

  bldSelected <- callModule(buildingMap, "leafletMap",
             bldShape = appData$buildingShapes[which(appData$buildingShapes$BLGNUM %in% appData$bld$BldgNo),],
             lndScpData = appData$landscaping,
             leedImages = leedImages,
             mapIcons = mapIcons,
             leedBldData = appData$leed,
             projectData = appData$projectMap)

  observeEvent(bldSelected(), {

    bldName <- if(length(bldSelected()) > 0){

      appData$bld %>%
        filter(BldgNo == bldSelected()) %>%
        select(BldgName) %>%
        simpleCap()

    } else {
      NULL
    }

    print(bldSelected())
    callModule(highLinePlot, "bldEnergy",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["ElecKWH"],
               trends=T,
               plotTitle=paste0("<b>", bldName, " Electricity", " Usage", "</b>"),
               tsNames=c("Electricity"),
               ylab="Usage in KWH",
               colors="gold",
               toolSuffix=" KWH", toolPrefix=NULL)

    callModule(highLinePlot, "bldGas",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["TotalGasDKT"],
               trends=T,
               plotTitle=paste0("<b>", bldName, " Gas", " Usage", "</b>"),
               tsNames="Gas",
               ylab="Usage in DKT",
               colors="darkorange",
               toolSuffix=" DKT", toolPrefix=NULL)

    callModule(highLinePlot, "bldWater",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["WaterMCF"],
               trends=T,
               plotTitle=paste0("<b>", bldName, " Water", " Usage", "</b>"),
               tsNames="Water",
               ylab="Usage in MCF",
               colors="blue",
               toolSuffix=" MCF", toolPrefix=NULL)

    callModule(highLinePlot, "bldSteam",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["SteamLBS"],
               trends=T,
               plotTitle=paste0("<b>", bldName, " Steam", " Usage", "</b>"),
               tsNames="Steam",
               ylab="Usage in lbs",
               colors="darkgrey",
               toolSuffix=" lbs", toolPrefix=NULL)
  })

  callModule(dataSource, "dataSources", appData)

  callModule(montanaMadeColumn, "montMadePurch", appData$food)

  callModule(montanaMadeBar, "montMadeTotal", appData$food)



  observeEvent(input$showBuildingGraphsButton, {
    shinyjs::hide("buildingGraphs", anim=T)
  })

  observeEvent(bldSelected(), {
    print(length(bldSelected()))

    if(length(bldSelected()) == 0){
      shinyjs::hide("buildingGraphs", anim=T, animType="slide")
    } else {
      shinyjs::show("buildingGraphs", anim=T, animType="slide")
    }
  })


#  lapply(
#    X = c("Energy", "Waste", "GHG", "Water", "Map", "Projects", "Food", "About"),
#    FUN = function(i){
#      observeEvent(input[[paste0("openTab", i)]], {
#        updateNavbarPage(session, "main",
#          selected = paste0("tab", i)
#        )
#      })
#    }
#  )

  values <- reactiveValues(lastSelected=NULL)

  lapply(
    X = c("Energy", "Waste", "GHG", "Water", "Map", "Projects", "Food", "About"),
    FUN = function(i){
      observeEvent(input[[paste0("openTab", i)]], {
        shinyjs::hide(values$lastSelected)
        shinyjs::toggle(paste0("tab", i), anim=T, animType="slide")
        values$lastSelected <- paste0("tab", i)
      })
    }
  )

}
