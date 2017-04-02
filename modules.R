#Modules
highLinePlotOutput <- function(id, plotWidth="100%", plotHeight="500px"){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), width = plotWidth, heigh = plotHeight)

}


highLinePlot <- function(input, output, session, dataTs, trends, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%

      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T, verticalAlign="bottom", align="center", layout="horizontal") %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = list(text = ylab), opposite= FALSE) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)

    if(is.null(dim(dataTs))){

      print("MEOWW")
        hchart <- hchart %>%
          hc_add_series(dataTs, color=colors, name=tsNames)

    } else {

      for(i in 1:ncol(dataTs)){
        hchart <- hchart %>%
          hc_add_series(dataTs[, i], color=colors[i], name=tsNames[i])
      }

    }


    if(!is.null(trends)){
      if(is.null(ncol(trends))){
        hchart <- hchart %>%
          hc_add_series(trends, color=colors, name=paste(tsNames, "Trend"), showInLegend=T, visible=F)
      } else {
        for(i in 1:ncol(trends)){

          hchart <- hchart %>%
            hc_add_series(trends[, i], color=colors[i], name=paste(tsNames[i], "Trend"), showInLegend=T, visible=F)
        }

      }
    }

    return(hchart)
  })

}

highAreaPlotOutput <- function(id){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), height = "500px")

}

highAreaPlot <- function(input, output, session, dataTs, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%
      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T) %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = ylab, opposite=FALSE) %>%
      hc_plotOptions(area=list(stacking="percent")) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)

    for(i in 1:ncol(dataTs)){
      hchart <- hchart %>%
        hc_add_series(dataTs[, i], color=colors[i], name=tsNames[i], type="area")
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
      addLayersControl(baseGroups=c("World Topo", "World Imagery"), position="bottomright") %>%


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
        highlightOptions = highlightOptions(weight=4,opacity=2, fillOpacity=0.5, bringToFront=T, sendToBack=T))

    #%>% addLayersControl(overlayGroups = c("LEED Buildings", "Edible Landscaping"))
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
        categories = strsplit(food$X1, split = "\n")
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
      sidebarPanel(
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
