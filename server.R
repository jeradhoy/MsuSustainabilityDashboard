############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R
library(shiny)
library(highcharter)
library(devtools)
library(leaflet)
library(htmltools)

#Set waste goals
wasteCAP2020 <- (3933386*.75)/(2000*12)
wasteCAP2030 <- (3933386*.5)/(2000*12)
wasteCAP2040 <- (3933386*.35)/(2000*12)
wasteCAP2050 <- (3933386*.2)/(2000*12)

# Set thousands seperator in highcharts graph
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

energyTrends <- getTrendSeries(energyTs, startTs=c(2005, 1), freq=12)

wasteTrends <- getTrendSeries(wasteTs, startTs = c(2006, 1), freq=12)
######## START Shiny Server ###################
shinyServer(function(input, output, session) {

##########################
# DEBUG OUTPUTS; Remove before deployment
  #Radio button to decide input
  output$energyDebug<- renderPrint({
    paste(input$energyTrendLine)
  })
#########################


  ########### Energy Usage ################
  output$energyUsage <- renderHighchart({

    #If usage
    if(input$usageOrExpendRadio == "0"){
      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>") %>%
        hc_legend(enabled=T, verticalAlign="bottom", align="center", layout="horizontal") %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Usage in KWH"),
          opposite= FALSE) %>%

        hc_add_series_ts(name="Electricity", ts=energyTs[,"KWH.Units"],
          showInLegend=T, color="gold", visible=input$elec) %>%

        hc_add_series_ts(name="Electricity Trend",
          ts=energyTrends[,"KWH.Units"],
          showInLegend=F, color="gold", visible=input$elecTrendLine & input$elec, dashStyle="LongDash") %>%

        hc_add_series_ts(name="Gas", ts=energyTs[,"GAS.KWH"],
          color="darkorange", visible=input$gas) %>%

        hc_add_series_ts(name="Gas Trend",
          ts=energyTrends[,"GAS.KWH"],
          showInLegend=F, color="darkorange", visible=input$gasTrendLine & input$gas, dashStyle="LongDash") %>%

        hc_tooltip(valueSuffix=" KWH")

    } else {

      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Electricity and Gas Usage in Dollars</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Expenditure in Dollars"),
          opposite= FALSE) %>%
        hc_add_series_ts(name="Electricity", ts=energyTs[,"ELEC"],
          showInLegend=T, color="gold", visible=input$elec) %>%

        hc_add_series_ts(name="Electricity Trend", ts=energyTrends[,"ELEC"],
          showInLegend=F, color="gold", visible=input$elecTrendLine & input$elec) %>%
        #hc_add_series_ts(name="Electricity Target", ts=energyTarget[,1],
          #showInLegend=T, color="blue",dashStyle="dot", visible=F) %>%
        hc_add_series_ts(name="Gas", ts=energyTs[,"GAS"],
          color="darkorange", visible=input$gas) %>%
        hc_add_series_ts(name="Gas Trend", ts=energyTrends[,"GAS"],
          color="darkorange", visible=input$gasTrendLine & input$gas, type="line", showInLegend=F) %>%
        #hc_add_series_ts(name="Gas Target", ts=energyTarget[,2],
          #showInLegend=T, color="red", dashStyle="dot", visible=F) %>%
        hc_tooltip(valuePrefix="$")
    }
  })


  ############ Waste Line ##################
  output$MSUwaste  <- renderHighchart({

    highchart(type="stock") %>%
      hc_title(useHTML=T, text = "<b>MSU Waste</b>") %>%
      hc_legend(enabled=T) %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = list(text = "Waste in Tons"),
        plotLines = (list(
          list(label = list(text = "2020 CAP Goal",style=list(color="purple")),
            color = "purple",
            width = 1.5,
            dashStyle= "longdash",
            value = wasteCAP2020,
            floating= T),
          list(label = list(text = "2030 CAP Goal",  style=list(color="purple")),
              color = "purple",
              width = 1.5,
              dashStyle= "longdash",
              value = wasteCAP2030),
          list(label = list(text = "2040 CAP Goal", style=list(color="purple")),
              color = "purple",
              width = 1.5,

              dashStyle= "longdash",
              value = wasteCAP2040),
          list(label = list(text = "2050 CAP Goal", style=list(color="purple")),
              color = "purple",
              width = 1.5,
              dashStyle= "longdash",
              value = wasteCAP2050))),
       opposite= FALSE)%>%
    hc_add_series_ts(name="Landfill", ts=wasteTs[,"landfill"],
      showInLegend=T, color= "brown", type="line", visible=input$landfill) %>%
    hc_add_series_ts(name="Recycle", ts=wasteTs[,"recycle"],
      color= "gold", type="line", visible=input$recycle) %>%
    hc_add_series_ts(name="Compost", ts=wasteTs[,"compost"],
      color= "green", type="line", visible=input$compost) %>%

    hc_add_series_ts(name="Landfill Trend", ts=wasteTrends[,"landfill"], color="brown",
      visible = input$landfillTrendLine & input$landfill, type="line", showInLegend=F)%>%

    hc_add_series_ts(name="Recycling Trend", ts=wasteTrends[,"recycle"], color="gold",
      visible = input$recycleTrendLine & input$recycle, type="line", showInLegend=F)%>%

    hc_add_series_ts(name="Compost Trend", ts=wasteTrends[,"compost"], color="green",
      visible = input$compostTrendLine & input$compost, type="line", showInLegend=F)%>%

    hc_tooltip(valueSuffix="tons")

})

  ############ Waste Area ##################
  output$PercentWaste <- renderHighchart({

      highchart(type="stock") %>%
        hc_title(useHTML=T, text = "<b>Percent of MSU Waste Diverted</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(
          text = "% of Total Waste by Weight"), opposite=FALSE)%>%
        hc_plotOptions(area=list(stacking="percent")) %>%
        hc_add_series_ts(name="Landfill", ts=wasteTs[,2],
          showInLegend=T, color= "brown", type="area") %>%
        hc_add_series_ts(name="Recycle", ts=wasteTs[,1],
          color= "gold", type="area") %>%
        hc_add_series_ts(name="Compost", ts=wasteTs[,3],
          color= "green", type="area") %>%
        hc_tooltip(valueSuffix=" tons")
  })

  ############ Per Capita Waste Area ##################
  output$PerCapitaWaste <- renderHighchart({

    highchart() %>%
      hc_title(useHTML=T, text = "<b>Per Capita Waste</b>") %>%
      hc_legend(enabled=T, reversed=T) %>%
      hc_xAxis(categories= perCapita$FY, title=list(text="Fiscal Year")) %>%
      hc_yAxis(title=list(
        text="Pounds Per Person (lbs)"), opposite=FALSE)%>%
      #hc_plotOptions(column=list(stacking="normal")) %>%

      hc_add_series(name="Compost", data=perCapita[,8], color= "orange",
        type="column") %>%
      hc_add_series(name="Recycle", data= perCapita[,6], color= "green",
        type = "column") %>%
      hc_add_series(name="Landfill", data=perCapita[,7], showInLegend=T,
        color= "black", type="column") %>%
      hc_tooltip(valueSuffix="lbs")
  })

  ########### Water Charts ################
  output$waterSewer <- renderHighchart({


    if(input$waterUsage == "0"){
      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Water Usage in Million Cubic Feet per Month</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Usage in Million Cubic Feet (MCF)"),
          opposite=F) %>%
        hc_add_series_ts(name="Water", ts=energyTs[,"WATER.MCF"],
          showInLegend=T, color="blue", visible=T) %>%
        hc_add_series_ts(name="Water Trend", ts=energyTrends[,"WATER.MCF"], color="blue",
          visible = input$waterTrendLine, type="line", showInLegend=F)%>%
        hc_tooltip(valueSuffix=" MCF")

    } else {

      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Water/Sewer Expenditure in Dollars</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Expenditure in Dollars"), opposite=F) %>%
        hc_add_series_ts(name="Water/Sewer", ts=energyTs[,"Water.Sewer"],
          showInLegend=T, color="green", visible=T) %>%
        hc_add_series_ts(name="Water Trend", ts=energyTrends[,"Water.Sewer"], color="green",
          visible = input$waterTrendLine, type="line", showInLegend=F)%>%
        hc_tooltip(valuePrefix="$")
    }
  })

  mapIconSize <- 40
  mapIcons <- iconList(
    leed = makeIcon(iconUrl="assets/UWIcons/1l0-e0-e0-d-certification-icon.png", iconWidth=mapIconSize, iconHeight = mapIconSize, iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),
    solar = makeIcon(iconUrl="assets/UWIcons/1solar-panels-icon.png", iconWidth=mapIconSize, iconHeight = mapIconSize, iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),
    garden = makeIcon(iconUrl="assets/UWIcons/3garden-icon.png", iconWidth=mapIconSize, iconHeight = mapIconSize, iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),
    tree = makeIcon(iconUrl="assets/UWIcons/3brockman-tree-tour-icon.png", iconWidth=mapIconSize, iconHeight = mapIconSize, iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize),
    compost = makeIcon(iconUrl="assets/UWIcons/6on-site-composting-icon.png", iconWidth=mapIconSize, iconHeight = mapIconSize, iconAnchorX=mapIconSize/2, iconAnchorY = mapIconSize)
  )

  leedImages <- list(
    "LEED Gold" = "assets/UWIcons/leedGold.png",
    "LEED Silver" = "assets/UWIcons/leedSilver.jpg"
  )

  output$mapDebug <- renderPrint({
    input$map_shape_click
    #input$showBuildingGraphsButton %% 2
  })

  closeGraphs <- reactive({
    if(input$showBuildingGraphsButton %% 2 == 1){
      F
    }else {
      T
    }
  })


  output$showBuildingGraphs <- renderUI({
    if(!is.null(input$map_shape_click)){
      tagList(
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = F, bottom = 70, left = 70, right = "auto", top = "auto",
          width = "80%", height =400,
          column(width=6,
                 highchartOutput("buildingKwhChart", height = "100%")
          ),
          column(width=6,
                 highchartOutput("buildingWaterChart", height = "100%")
          ),
          actionButton("showBuildingGraphsButton", label="Close")
        )
      )
    }
  })

  ########### Food ##############
  output$montanaMade <- renderHighchart({


    highchart()%>%
      hc_title(useHTML=T,
               text = "<b>Made in Montana Food</b>")%>%
      hc_add_series(Food$MTMade,  hcaes(name = Food$X1, y = percent), name = "Percent")
  })

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
      addLayersControl(baseGroups=c("World Topo", "World Imagery"), position="bottomright") %>%
      addMarkers(data=leed, group="LEED Buildings",
        ~Lon, ~Lat, icon=mapIcons["leed"],
        popup = ~paste0("<h3>", Building, " : ", LeedCert, "</h3>","<p>", Description ,"</p>",
          '<img src="', as.character(leedImages[LeedCert]), '" height="150" width="150">',
          '<a target="_blank" href="http://www.usgbc.org/leed"><p>Leed Certification Info</a>',
          " - ", '<a target="_blank" href="', ProjectLink, '">Project Info</p></a>')
      ) %>%
    addMarkers(data=landscaping, group="Edible Landscaping",
               ~Lon, ~Lat, icon=~mapIcons[Category],
        popup=~paste0("<h3>", Name , "</h3>",
                        "<p> Info </p>")) %>%
    addMarkers(data=projectMap, group="Projects",
               ~Lon, ~Lat, icon=~mapIcons[Category],
               popup = ~paste0("<h3>", Name , "</h3>", "<p>", Description ,"</p>")) %>%

      addPolygons(data=buildingShapes, weight=2, layerId=buildingShapes$BLGNUM,
        highlightOptions = highlightOptions(weight=4,opacity=2, fillOpacity=0.5, bringToFront=T, sendToBack=T))

    #%>% addLayersControl(overlayGroups = c("LEED Buildings", "Edible Landscaping"))
  })


  output$buildingKwhChart <- renderHighchart({

    data <- subset(buildingUtilities, Bldg.No == input$map_shape_click$id)
    #data <- subset(buildingUtilities, Bldg.No == 535)
    KWH_TS <- with(data, ts(data=KWH.QTY, start=c(ACCTYR[1], ACCTMO[1]), frequency=12))

      hc <- highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = paste("<b>", data$Building.Name[1], "</b>")) %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Usage in KWH"),
          opposite=F) %>%
        hc_add_series_ts(name="Electricity", ts=KWH_TS,
          showInLegend=T, color="gold", visible=T) %>%
        hc_tooltip(valueSuffix=" KWH")

  })

  output$buildingWaterChart <- renderHighchart({

    data <- subset(buildingUtilities, Bldg.No == input$map_shape_click$id)
    #data <- subset(buildingUtilities, Bldg.No == 535)
    Water_TS <- with(data, ts(data=WATER.MCF, start=c(ACCTYR[1], ACCTMO[1]), frequency=12))

      hc <- highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = paste("<b>", data$Building.Name[1], "</b>")) %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Water Usage in MCF"),
          opposite=F) %>%
        hc_add_series_ts(name="Water", ts=Water_TS,
          showInLegend=T, color="blue", visible=T) %>%
        hc_tooltip(valueSuffix="MCF")

  })

  observeEvent(input$showLeed, {
    if(input$showLeed){
      leafletProxy("map", session) %>% showGroup("LEED Buildings")
    } else {
      leafletProxy("map", session) %>% hideGroup("LEED Buildings")

    }
  })

  observeEvent(input$showEdible, {
    if(input$showEdible){
      leafletProxy("map", session) %>% showGroup("Edible Landscaping")
    } else {
      leafletProxy("map", session) %>% hideGroup("Edible Landscaping")

    }
  })

  observeEvent(input$showProject, {
    if(input$showProject){
      leafletProxy("map", session) %>% showGroup("Projects")
    } else {
      leafletProxy("map", session) %>% hideGroup("Projects")

    }
  })

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


  output$dataTable <- DT::renderDataTable({
    DT::datatable(get(input$dataset))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(get(input$dataset), file)
    }
  )

  #checkbox button for map
  # function(input, output) {
  #
  #   # You can access the values of the widget (as a vector)
  #   # with input$checkGroup, e.g.
  #   output$value <- renderPrint({ input$checkGroup })
  #
  # }

#End of App
})
