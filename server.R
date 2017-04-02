############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R
  source("modules.R")

options(shiny.port=5555)

#Set waste goals
wasteCAP2020 <- (3933386*.75)/(2000*12)
wasteCAP2030 <- (3933386*.5)/(2000*12)
wasteCAP2040 <- (3933386*.35)/(2000*12)
wasteCAP2050 <- (3933386*.2)/(2000*12)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

######## START Shiny Server ###################
shinyServer(function(input, output, session) {

  callModule(highLinePlot, "energyUsage",
             dataTs=appData$energyTs[,c("KWH.Units", "GAS.KWH")],
             trends=appData$energyTrends[,c("KWH.Units", "GAS.KWH")],
             plotTitle="<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Usage in KWH", colors=c("gold", "darkorange"),
             toolSuffix=" KWH", toolPrefix=NULL)

  callModule(highLinePlot, "energyExpend",
             dataTs=appData$energyTs[,c("ELEC", "GAS")],
             trends=NULL,
             plotTitle="<b>MSU Electricity and Gas Expenditure</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Expenditure in Dollars",
             colors=c("gold", "darkorange"),
             toolSuffix=NULL, toolPrefix="$")


  callModule(highLinePlot, "wasteLine",
             dataTs=appData$wasteTs,
             trends=appData$wasteTrends,
             plotTitle="<b>MSU Waste</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
             ylab="Waste in Tons", colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL)

  callModule(highAreaPlot, "wasteArea",
             dataTs=appData$wasteTs[,c(2,1,3)],
             plotTitle="<b>Percent of MSU Waste Diverted</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
             ylab="Waste in Tons",
             colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL)

  callModule(highLinePlot, "waterUse",
             dataTs=appData$energyTs[,"WATER.MCF"],
             trends=appData$energyTrends[,"WATER.MCF"],
             plotTitle="<b>MSU Water Usage in Million Cubic Feet per Month</b>",
             tsNames="Water",
             ylab="Usage in Million Cubic Feet (MCF)",
             colors="blue",
             toolSuffix=" MCF", toolPrefix=NULL)

  callModule(highLinePlot, "waterSewerExpend",
             dataTs=appData$energyTs[,"Water.Sewer"],
             trends=appData$energyTrends[,"Water.Sewer"],
             plotTitle="<b>Msu Water/Sewer Expenditure In Dollars</b>",
             tsNames="Water/Sewer",
             ylab="Expenditure in Dollars",
             colors="green",
             toolSuffix=NULL, toolPrefix="$")

  bldSelected <- callModule(buildingMap, "leafletMap",
             bldShape = appData$buildingShapes[which(appData$buildingShapes$BLGNUM %in% names(appData$bldTs)),],
             lndScpData = appData$landscaping,
             leedImages = leedImages,
             mapIcons = mapIcons,
             leedBldData = appData$leed,
             projectData = appData$projectMap)

  observeEvent(bldSelected(), {

    bldName <- if(length(bldSelected()) > 0){
      simpleCap(subset(appData$buildingUtilities, Bldg.No == bldSelected(), Building.Name)[1,1])
    } else {
      NULL
    }

    callModule(highLinePlot, "bldEnergy",
               dataTs=appData$bldTs[[bldSelected()]][,"KWH.QTY"],
               trends=NULL,
               plotTitle=paste0("<b>", bldName, " Electricity", " Usage", "</b>"),
               tsNames="Electricity",
               ylab="Usage in KWH",
               colors="gold",
               toolSuffix=" KWH", toolPrefix=NULL)

    callModule(highLinePlot, "bldGas",
               dataTs=appData$bldTs[[bldSelected()]][,"TOTAL.GAS..DKT"],
               trends=NULL,
               plotTitle=paste0("<b>", bldName, " Gas", " Usage", "</b>"),
               tsNames="Gas",
               ylab="Usage in DKT",
               colors="darkorange",
               toolSuffix=" DKT", toolPrefix=NULL)

    callModule(highLinePlot, "bldWater",
               dataTs=appData$bldTs[[bldSelected()]][,"WATER.MCF"],
               trends=NULL,
               plotTitle=paste0("<b>", bldName, " Water", " Usage", "</b>"),
               tsNames="Water",
               ylab="Usage in MCF",
               colors="blue",
               toolSuffix=" MCF", toolPrefix=NULL)

    callModule(highLinePlot, "bldSteam",
               dataTs=appData$bldTs[[bldSelected()]][,"STEAM.LBS"],
               trends=NULL,
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
    shinyjs::show("buildingGraphs", anim=T, animType="slide")
  })








#  observeEvent(input$showLeed, {
#    if(input$showLeed){
#      leafletProxy("map", session) %>% showGroup("LEED Buildings")
#    } else {
#      leafletProxy("map", session) %>% hideGroup("LEED Buildings")
#
#    }
#  })
#
#  observeEvent(input$showEdible, {
#    if(input$showEdible){
#      leafletProxy("map", session) %>% showGroup("Edible Landscaping")
#    } else {
#      leafletProxy("map", session) %>% hideGroup("Edible Landscaping")
#
#    }
#  })
#
#  observeEvent(input$showProject, {
#    if(input$showProject){
#      leafletProxy("map", session) %>% showGroup("Projects")
#    } else {
#      leafletProxy("map", session) %>% hideGroup("Projects")
#
#    }
#  })

  observeEvent(input$openTabEnergy, {
    updateNavbarPage(session, "main",
      selected = "tabEnergy"
    )
  })
  observeEvent(input$openTabWaste, {
    updateNavbarPage(session, "main",
      selected = "tabWaste"
    )
  })
  observeEvent(input$openTabGHG, {
    updateNavbarPage(session, "main",
      selected = "tabGHG"
    )
  })
  observeEvent(input$openTabWater, {
    updateNavbarPage(session, "main",
      selected = "tabWater"
    )
  })
  observeEvent(input$openTabMap, {
    updateNavbarPage(session, "main",
      selected = "tabMap"
    )
  })

  observeEvent(input$openTabAbout, {
    updateNavbarPage(session, "main",
      selected = "tabAbout"
    )
  })
  observeEvent(input$openTabProjects, {
    updateNavbarPage(session, "main",
                     selected = "tabProjects"
    )
  })
  observeEvent(input$openTabFood, {
    updateNavbarPage(session, "main",
                     selected = "tabFood"
    )
  })


})

#  ############ Waste Line ##################
#  output$MSUwaste  <- renderHighchart({
#
#    highchart(type="stock") %>%
#      hc_title(useHTML=T, text = "<b>MSU Waste</b>") %>%
#      hc_legend(enabled=T) %>%
#      hc_rangeSelector(inputEnabled=F) %>%
#      hc_yAxis(title = list(text = "Waste in Tons"),
#        plotLines = (list(
#          list(label = list(text = "2020 CAP Goal",style=list(color="purple")),
#            color = "purple",
#            width = 1.5,
#            dashStyle= "longdash",
#            value = wasteCAP2020,
#            floating= T),
#          list(label = list(text = "2030 CAP Goal",  style=list(color="purple")),
#              color = "purple",
#              width = 1.5,
#              dashStyle= "longdash",
#              value = wasteCAP2030),
#          list(label = list(text = "2040 CAP Goal", style=list(color="purple")),
#              color = "purple",
#              width = 1.5,
#
#              dashStyle= "longdash",
#              value = wasteCAP2040),
#          list(label = list(text = "2050 CAP Goal", style=list(color="purple")),
#              color = "purple",
#              width = 1.5,
#              dashStyle= "longdash",
#              value = wasteCAP2050))),
#       opposite= FALSE)%>%
#    hc_add_series_ts(name="Landfill", ts=appData$wasteTs[,"landfill"],
#      showInLegend=T, color= "black", type="line", visible=input$landfill) %>%
#    hc_add_series_ts(name="Recycle", ts=appData$wasteTs[,"recycle"],
#      color= "gold", type="line", visible=input$recycle) %>%
#    hc_add_series_ts(name="Compost", ts=appData$wasteTs[,"compost"],
#      color= "green", type="line", visible=input$compost) %>%
#
#    hc_add_series_ts(name="Landfill Trend", ts=wasteTrends[,"landfill"], color="brown",
#      visible = input$landfillTrendLine & input$landfill, type="line", showInLegend=F)%>%
#
#    hc_add_series_ts(name="Recycling Trend", ts=wasteTrends[,"recycle"], color="gold",
#      visible = input$recycleTrendLine & input$recycle, type="line", showInLegend=F)%>%
#
#    hc_add_series_ts(name="Compost Trend", ts=wasteTrends[,"compost"], color="green",
#      visible = input$compostTrendLine & input$compost, type="line", showInLegend=F)%>%
#
#    hc_tooltip(valueSuffix="tons")
#
#})

#  ############ Per Capita Waste Area ##################
#  output$PerCapitaWaste <- renderHighchart({
#
#    highchart() %>%
#      hc_title(useHTML=T, text = "<b>Per Capita Waste</b>") %>%
#      hc_legend(enabled=T, reversed=T) %>%
#      hc_xAxis(categories= perCapita$FY, title=list(text="Fiscal Year")) %>%
#      hc_yAxis(title=list(
#        text="Pounds Per Person (lbs)"), opposite=FALSE)%>%
#      #hc_plotOptions(column=list(stacking="normal")) %>%
#
#      hc_add_series(name="Compost", data=perCapita[,8], color= "orange",
#        type="column") %>%
#      hc_add_series(name="Recycle", data= perCapita[,6], color= "green",
#        type = "column") %>%
#      hc_add_series(name="Landfill", data=perCapita[,7], showInLegend=T,
#        color= "black", type="column") %>%
#      hc_tooltip(valueSuffix="lbs")
#  })
