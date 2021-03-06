############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R

#Set waste goals
#wasteCAP2020 <- (3933386*.75)/(2000*12)
#wasteCAP2030 <- (3933386*.5)/(2000*12)
#wasteCAP2040 <- (3933386*.35)/(2000*12)
#wasteCAP2050 <- (3933386*.2)/(2000*12)


######## START Shiny Server ###################
server <- function(input, output, session) {

  callModule(highLinePlot, "energyUsageElec",
             dataTs=appData$energyTs[c("ElecKWH")],
             trends=T,
             plotTitle="<b>MSU Electricity Usage in Kilowatt Hours</b>",
             tsNames=c("Electricity"),
             ylab="Usage in KWH", colors=c("gold"),
             toolSuffix=" KWH", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highLinePlot, "energyUsageGas",
             dataTs=appData$energyTs[c("GasDKT")],
             trends=T,
             plotTitle="<b>MSU Natural Gas Usage in Million BTUs</b>",
             tsNames=c("Natural Gas"),
             ylab="Usage in MMBTU", colors=c("darkorange"),
             toolSuffix=" MMBTU", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highLinePlot, "energyExpend",
             dataTs=appData$energyTs[c("ElecExpend", "GasExpend")],
             trends=T,
             plotTitle="<b>MSU Electricity and Gas Expenditure</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Expenditure in Dollars",
             colors=c("gold", "darkorange"),
             toolSuffix=NULL, toolPrefix="$", aggregYearly=reactive(input$annual))


  callModule(highLinePlot, "wasteLine",
             dataTs=appData$wasteTs[c(2,1,3)],
             trends=T,
             plotTitle="<b>MSU Waste in Tons Per Month</b>",
             tsNames=c("Landfill", "Recycling", "Compost"),
             ylab="Waste in Tons", colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highAreaPlot, "wasteArea",
             dataTs=appData$wasteTs[c(2,1,3)],
             plotTitle="<b>Percent of MSU Waste Diverted</b>",
             tsNames=c("Landfill", "Recycling", "Compost"),
             ylab="Waste in Tons",
             colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highLinePlot, "waterUse",
             dataTs=appData$energyTs["WaterMCF"],
             trends=T,
             plotTitle="<b>MSU Water Usage in Million Cubic Feet per Month</b>",
             tsNames="Water",
             ylab="Usage in Million Cubic Feet (MCF)",
             colors="blue",
             toolSuffix=" MCF", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highLinePlot, "waterSewerExpend",
             dataTs=appData$energyTs["WaterSewerExpend"],
             trends=T,
             plotTitle="<b>Msu Water/Sewer Expenditure In Dollars</b>",
             tsNames="Water/Sewer",
             ylab="Expenditure in Dollars",
             colors="green",
             toolSuffix=NULL, toolPrefix="$", aggregYearly=reactive(input$annual))

  bldSelected <- callModule(buildingMap, "leafletMap",
             bldShape = appData$buildingShapes[which(appData$buildingShapes$BLGNUM %in% appData$bld$BldgNo),],
             lndScpData = appData$landscaping,
             leedImages = leedImages,
             mapIcons = mapIcons,
             leedBldData = appData$leed,
             projectData = appData$projectMap)

  observeEvent(bldSelected(), {
    if(length(bldSelected()) > 0){


  bldName <- reactive(simpleCap(select(filter(appData$bld, BldgNo == bldSelected()), BldgName)))


    callModule(highLinePlot, "bldEnergy",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["ElecKWH"],
               trends=T,
               plotTitle=paste0("<b>", bldName(), " Electricity", " Usage", "</b>"),
               tsNames=c("Electricity"),
               ylab="Usage in KWH",
               colors="gold",
               toolSuffix=" KWH", toolPrefix=NULL, aggregYearly=reactive(input$annual))

    callModule(highLinePlot, "bldGas",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["TotalGasDKT"],
               trends=T,
               plotTitle=paste0("<b>", bldName(), " Gas", " Usage", "</b>"),
               tsNames="Gas",
               ylab="Usage in MMBTU",
               colors="darkorange",
               toolSuffix=" MMBTU", toolPrefix=NULL, aggregYearly=reactive(input$annual))

    callModule(highLinePlot, "bldWater",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["WaterMCF"],
               trends=T,
               plotTitle=paste0("<b>", bldName(), " Water", " Usage", "</b>"),
               tsNames="Water",
               ylab="Usage in MCF",
               colors="blue",
               toolSuffix=" MCF", toolPrefix=NULL, aggregYearly=reactive(input$annual))

    callModule(highLinePlot, "bldSteam",
               dataTs=filter(appData$bld, BldgNo == bldSelected())$data[[1]]["SteamLBS"],
               trends=T,
               plotTitle=paste0("<b>", bldName(), " Steam", " Usage", "</b>"),
               tsNames="Steam",
               ylab="Usage in lbs",
               colors="darkgrey",
               toolSuffix=" lbs", toolPrefix=NULL, aggregYearly=reactive(input$annual))
    }
  })

  callModule(dataSource, "dataSources", appData)

  callModule(montanaMadeColumn, "montMadePurch", appData$food)

  callModule(montanaMadeBar, "montMadeTotal", appData$food)



  observeEvent(input$showBuildingGraphsButton, {
    shinyjs::hide("buildingGraphs", anim=T)
  })

  observeEvent(bldSelected(), {

    if(length(bldSelected()) == 0){
      shinyjs::hide("buildingGraphs", anim=T, animType="slide")
    } else {
      shinyjs::show("buildingGraphs", anim=T, animType="slide")
    }
  })


  lapply(
    X = c("Energy", "Waste", "GHG", "Water", "Map", "Projects", "Food", "About"),
    FUN = function(i){
      observeEvent(input[[paste0("openTab", i)]], {
        updateNavbarPage(session, "main",
          selected = paste0("tab", i)
        )
      })
    }
  )

#  values <- reactiveValues(lastSelected=NULL)
#
#  lapply(
#    X = c("Energy", "Waste", "GHG", "Water", "Map", "Projects", "Food", "About"),
#    FUN = function(i){
#      observeEvent(input[[paste0("openTab", i)]], {
#        shinyjs::hide(values$lastSelected)
#        shinyjs::toggle(paste0("tab", i), anim=T, animType="slide")
#        values$lastSelected <- paste0("tab", i)
#      })
#    }
#  )



}
