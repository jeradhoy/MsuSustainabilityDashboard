############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R
library(shiny)
library(rsconnect)
library(highcharter)
library(devtools)
library(googlesheets)
library(rgdal)
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

######## START Shiny Server ###################
shinyServer(function(input, output, session) {

##########################
# DEBUG OUTPUTS; Remove before deployment
  #Radio button to decide input
  output$energyDebug<- renderPrint({
    paste(input$openTab2)
  })
#########################


  ########### Energy Usage ################
  output$energyUsage <- renderHighchart({

    #If usage
    if(input$usageOrExpendRadio == "0"){
      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Usage in KWH"),
          opposite= FALSE) %>%
        hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,1],
          showInLegend=T, color="gold", visible=input$elec) %>%
        #hc_add_series_ts(name="Electricity Target", ts=energyTarget[,1],
          #showInLegend=T, color="blue",dashStyle="dot", visible=F) %>%
        hc_add_series_ts(name="Gas", ts=energyTimeSeries[,2],
          color="darkorange", visible=input$gas) %>%
        #hc_add_series_ts(name="Gas Target", ts=energyTarget[,2],
          #showInLegend=T, color="red", dashStyle="dot", visible=F) %>%
        hc_tooltip(valueSuffix=" KWH")

    } else {

      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Electricity and Gas Usage in Dollars</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Expenditure in Dollars"),
          opposite= FALSE) %>%
        hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,3],
          showInLegend=T, color="gold", visible=input$elec) %>%
        #hc_add_series_ts(name="Electricity Target", ts=energyTarget[,1],
          #showInLegend=T, color="blue",dashStyle="dot", visible=F) %>%
        hc_add_series_ts(name="Gas", ts=energyTimeSeries[,4],
          color="darkorange", visible=input$gas) %>%
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
    hc_add_series_ts(name="Landfill", ts=wastetimeseries[,2],
      showInLegend=T, color= "black", type="line") %>%
    hc_add_series_ts(name="Recycle", ts=wastetimeseries[,1],
      color= "green", type="line") %>%
    hc_add_series_ts(name="Compost", ts=wastetimeseries[,3],
      color= "orange", type="line") %>%
    hc_add_series_ts(name="Waste Fit", ts=wastefit, color="purple",
      type="line", showInLegend=F)%>%
    hc_tooltip(valueSuffix=" tons")
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
        hc_add_series_ts(name="Landfill", ts=wastetimeseries[,2],
          showInLegend=T, color= "black", type="area") %>%
        hc_add_series_ts(name="Recycle", ts=wastetimeseries[,1],
          color= "green", type="area") %>%
        hc_add_series_ts(name="Compost", ts=wastetimeseries[,3],
          color= "orange", type="area") %>%
        hc_tooltip(valueSuffix=" tons")
  })

  ############ Per Capita Waste Area ##################
  output$PerCapitaWaste <- renderHighchart({

    highchart() %>%
      hc_title(useHTML=T, text = "<b>Per Capita Waste</b>") %>%
      hc_legend(enabled=T, reversed=T) %>%
      hc_xAxis(categories= pcwaste$FY, title=list(text="Fiscal Year")) %>%
      hc_yAxis(title=list(
        text="Pounds Per Person (lbs)"), opposite=FALSE)%>%
      #hc_plotOptions(column=list(stacking="normal")) %>%
      hc_add_series(name="Compost", data=pcwaste[,8], color= "orange",
        type="column") %>%
      hc_add_series(name="Recycle", data= pcwaste[,6], color= "green",
        type = "column") %>%
      hc_add_series(name="Landfill", data=pcwaste[,7], showInLegend=T,
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
        hc_add_series_ts(name="Water", ts=waterSewerTimeSeries[,1],
          showInLegend=T, color="blue", visible=T) %>%
        hc_tooltip(valueSuffix=" MCF")

    } else {

      highchart(type="stock") %>%
        hc_title(useHTML=T,
          text = "<b>MSU Water/Sewer Expenditure in Dollars</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "Expenditure in Dollars"), opposite=F) %>%
        hc_add_series_ts(name="Water/Sewer", ts=waterSewerTimeSeries[,2],
          showInLegend=T, color="green", visible=T) %>%
        hc_tooltip(valuePrefix="$")
    }
  })

  ########### Leed Map ################
  output$leedBuildingMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data=leedBuildings,
        ~Lon, ~Lat,
        popup = ~paste0("<h3>", Building, " : ", LeedCert, "</h3>",
          "<p> Info </p>",
          '<a href="http://www.usgbc.org/leed"><p>Leed Certification Info</a>',
          " - ", '<a href="', ProjectLink, '">Project Info</p></a>')
      )
  })

  rv <- reactiveValues(tabOpen=0)

  observeEvent(input$openTab1, {
    rv$tabOpen <- 1
  })
  observeEvent(input$openTab2, {
    rv$tabOpen <- 2
  })
  observeEvent(input$openTab3, {
    rv$tabOpen <- 3
  })
  observeEvent(input$openTab4, {
    rv$tabOpen <- 4
  })
  observeEvent(input$openTab5, {
    rv$tabOpen <- 5
  })

  observeEvent(rv$tabOpen, {
    updateNavbarPage(session, "main",
      selected = paste0("tab", rv$tabOpen)
    )
  })

#End of App
})
