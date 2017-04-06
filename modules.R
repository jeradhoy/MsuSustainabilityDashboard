#Modules
highLinePlotOutput <- function(id, plotWidth="100%", plotHeight="500px"){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), width = plotWidth, heigh = plotHeight)

}


highLinePlot <- function(input, output, session, dataTs, trends, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix, aggregYearly){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%

      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T, verticalAlign="bottom", align="center", layout="horizontal") %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = list(text = ylab), opposite= FALSE) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)


    if(!is.null(aggregYearly())){
      if(aggregYearly()){
        dataTs <- lapply(dataTs, function(x){aggregate(x, nfrequency=1)})
      }
    }

    for(i in 1:length(dataTs)){
      hchart <- hchart %>%
        hc_add_series(dataTs[[i]], color=colors[i], name=tsNames[i])
    }

    if(trends){
      for(i in 1:length(dataTs)){
          hchart <- hchart %>%
            hc_add_series(getTrendSeries(dataTs[[i]]), color=colors[i], name=paste(tsNames[i], "Trend"), showInLegend=T, visible=F)
      }
    }

    return(hchart)
  })

}

highAreaPlotOutput <- function(id){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), height = "500px")

}

highAreaPlot <- function(input, output, session, dataTs, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix, aggregYearly){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%
      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T) %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = ylab, opposite=FALSE) %>%
      hc_plotOptions(area=list(stacking="percent")) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)

    if(!is.null(aggregYearly())){
      if(aggregYearly()){
        dataTs <- lapply(dataTs, function(x){aggregate(x, nfrequency=1)})
      }
    }

      for(i in 1:length(dataTs)){
        hchart <- hchart %>%
          hc_add_series(dataTs[[i]], color=colors[i], name=tsNames[i], type="area")
      }

    return(hchart)
  })

}

buildingMapUI <- function(id, mapHeight="100%", mapWidth="100%"){

  ns <- NS(id)

  leafletOutput(ns("map"), height=mapHeight, width=mapWidth)

}

buildingMap <- function(input, output, session, bldShape, lndScpData, leedImages, mapIcons, leedBldData, projectData) {


  ########### Leed Map ################
  output$map <- renderLeaflet({

    leaflet() %>%
      setView(lat=45.6675, lng=-111.053, zoom=16) %>%
      addProviderTiles("Esri.WorldTopoMap", group="World Topo",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles("Esri.WorldImagery", group="World Imagery",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%


      addMarkers(data=leedBldData, group="LEED Buildings",
        ~Lon, ~Lat, icon=mapIcons["leed"],

        popup = ~paste0("<h3>", Building, " : ", LeedCert, "</h3>","<p>", Description ,"</p>",
          '<img src="', as.character(leedImages[LeedCert]), '" height="150" width="150">',
          '<a target="_blank" href="http://www.usgbc.org/leed"><p>Leed Certification Info</a>',
          " - ", '<a target="_blank" href="', ProjectLink, '">Project Info</p></a>')
      ) %>%

      addMarkers(data=lndScpData, group="Edible Landscaping",
                 ~Lon, ~Lat, icon=~mapIcons[Category],
          popup=~paste0("<h3>", Name , "</h3>",
                          "<p> Info </p>")) %>%

      addMarkers(data=projectData, group="Projects",
                 ~Lon, ~Lat, icon=~mapIcons[Category],
                 popup = ~paste0("<h3>", Name , "</h3>", "<p>", Description ,"</p>")) %>%

      addPolygons(data=bldShape, weight=2, layerId=bldShape$BLGNUM,
        highlightOptions = highlightOptions(weight=4,opacity=2, fillOpacity=0.5, bringToFront=T, sendToBack=T)) %>%

      addLayersControl(baseGroups=c("World Topo", "World Imagery"), overlayGroups = c("LEED Buildings", "Edible Landscaping"), position="topright", options = layersControlOptions(collapsed=F))
  })

  return(reactive({as.character(input$map_shape_click$id)}))

}

montanaMadeColumnOutput <- function(id){
  ns <- NS(id)
  highchartOutput(ns("montanaMade"), height = "auto", width = "auto")
}

montanaMadeColumn <- function(input, output, session, food){

  output$montanaMade <- renderHighchart({
    highchart()%>%
      hc_title(useHTML=T,
               text = "<b>Montana Made Food</b>")%>%
      #hc_add_series(name = "MTMade", data = Food$MTMade[1:7], type ="pie")%>%
      hc_add_series(name = "Total", data = food$MTMade[1:7], type = "column")%>%
      hc_plotOptions(
        series = list(
          colorByPoint =T
        )
      )%>%
      hc_legend(F)%>%
      hc_yAxis(
        title = list(text = "Dollars ($)"),
        endOnTick = T
      )%>%
      hc_xAxis(
        title = list( text = "Category"),
        categories = strsplit(food$Category, split = "\n")
      )%>%
      hc_tooltip(valuePrefix="$")
  })
}

montanaMadeBarOutput <- function(id){
  ns <- NS(id)
  highchartOutput(ns("montanaMade"), height = "auto", width = "auto")
}

montanaMadeBar <- function(input, output, session, food){

  output$montanaMade <- renderHighchart({
    highchart()%>%
      hc_title(useHTML=T,
               text = "<b>MSU Food Purchases</b>")%>%
      #hc_legend(enabled=T, verticalAlign="bottom", align="center", layout="horizontal")%>%
      hc_add_series(name="Total", data = food$NonMTMade[8], type = "bar", color = "grey")%>%
      hc_add_series(name = "MTMade", data = food$MTMade[8], type = "bar", color = "green")%>%
      hc_plotOptions(
        series = list(
          colorByPoint = F,
          stacking = "normal",
          showInLegend = T,
          dataLabels = list(
            enabled = F
          )
        )
      )%>%
      hc_legend(F)%>%
      hc_yAxis(
        title = list(text = "Dollars ($)"),
        endOnTick = T
      )%>%
      hc_tooltip(valuePrefix="$")
    })
}


dataSourceUI <- function(id){

  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(width=2,
        selectInput(ns("dataset"), "Choose a dataset:",
                    choices = c("Energy" = "energy", "Leed Buildings" = "leed", "Per Capita Waste" = "perCapita", "Waste" = "waste", "Landscaping" = "landscaping", "Projects" = "projectMap", "Building Data" = "buildingUtilities")),
        downloadButton(ns('downloadData'), 'Download')
      ),
      mainPanel(
        DT::dataTableOutput(ns('dataTable'))
      )
    )
  )
}

dataSource <- function(input, output, session, appData){

  output$dataTable <- DT::renderDataTable({
    DT::datatable(appData[[input$dataset]])
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$dataset, '.csv')
    },
    content = function(file) {
      write.csv(appData[[input$dataset]], file)
    }
  )
}

######## START Shiny Server ###################
server <- function(input, output, session) {

  callModule(highLinePlot, "energyUsage",
             dataTs=appData$energyTs[c("ElecKWH", "GasKWH")],
             trends=T,
             plotTitle="<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Usage in KWH", colors=c("gold", "darkorange"),
             toolSuffix=" KWH", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highLinePlot, "energyExpend",
             dataTs=appData$energyTs[c("ElecExpend", "GasExpend")],
             trends=T,
             plotTitle="<b>MSU Electricity and Gas Expenditure</b>",
             tsNames=c("Electricity", "Natural Gas"),
             ylab="Expenditure in Dollars",
             colors=c("gold", "darkorange"),
             toolSuffix=NULL, toolPrefix="$", aggregYearly=reactive(input$annual))


  callModule(highLinePlot, "wasteLine",
             dataTs=appData$wasteTs,
             trends=T,
             plotTitle="<b>MSU Waste</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
             ylab="Waste in Tons", colors=c("black", "gold", "green"),
             toolSuffix=" tons", toolPrefix=NULL, aggregYearly=reactive(input$annual))

  callModule(highAreaPlot, "wasteArea",
             dataTs=appData$wasteTs[c(2,1,3)],
             plotTitle="<b>Percent of MSU Waste Diverted</b>",
             tsNames=c("Recycling", "Landfill", "Compost"),
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
               ylab="Usage in DKT",
               colors="darkorange",
               toolSuffix=" DKT", toolPrefix=NULL, aggregYearly=reactive(input$annual))

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
