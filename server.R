############ server.R #############
##All options and processing that are cheap and can be done each time app launches goes here, otherwise put in global.R
library(shiny)
library(rsconnect)
library(highcharter)
library(devtools)
library(googlesheets)
library(rgdal)
library(leaflet)

# get highchart options
hcopts <- getOption("highcharter.options")
# explore
hcopts
# override
hcopts$lang$thousandsSep <- ","
# update
options(highcharter.options = hcopts)

wasteCAP2020 <- (3933386*.75)/(2000*12)
wasteCAP2030 <- (3933386*.5)/(2000*12)
wasteCAP2040 <- (3933386*.35)/(2000*12)
wasteCAP2050 <- (3933386*.2)/(2000*12)

buildings <- readOGR(dsn = "./data/Buildings", layer = "msuBuildings", verbose = F)


######## START Shiny Server ###################
shinyServer(function(input, output) {

  #Radio button to decide input
  output$energyDebug<- renderPrint({paste(as.logical(as.numeric(input$usageOrExpendRadio)), "F")})

	  


  ########### Energy Charts ################
  output$energyUsage <- renderHighchart({
    

	highchart(type="stock") %>%
	  hc_title(useHTML=T, text = "<b>MSU Electricity and Gas Usage in Kilowatt Hours</b>") %>%
	  hc_legend(enabled=T) %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = list(text = "Usage in KWH"),
               opposite= FALSE) %>% 
      hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,1+2*as.numeric(input$usageOrExpendRadio)], showInLegend=T, color="gold", visible=input$elec) %>%
      #hc_add_series_ts(name="Electricity Trend", ts=energyTrends[,1], showInLegend=F, color="blue") %>%
      #hc_add_series_ts(name="Electricity Trend", ts=energyTrends[,1], showInLegend=T, color="blue", visible=F) %>%
      #hc_add_series_ts(name="Electricity Target", ts=energyTarget[,1], showInLegend=T, color="blue",dashStyle="dot", visible=F) %>%
      hc_add_series_ts(name="Gas", ts=energyTimeSeries[,2+2*as.numeric(input$usageOrExpendRadio)], color="darkorange", visible=input$gas) %>%
      #hc_add_series_ts(name="Gas Trend", ts=energyTrends[,2], showInLegend=T, color="red", visible=F) %>%
      #hc_add_series_ts(name="Gas Target", ts=energyTarget[,2], showInLegend=T, color="red", dashStyle="dot", visible=F) %>%
      hc_tooltip(valueSuffix=" KWH") 
  })
 
  ### Output the energyExpend highchart, to be called in the ui.R
#  output$energyExpend <- renderHighchart({
#    
#    highchart(type="stock") %>%
#      hc_title(useHTML=T, text = "<b>MSU Expenditure on Electricity and Gas</b>") %>%
#      hc_legend(enabled=T) %>%
#      hc_rangeSelector(inputEnabled=F) %>%
#      hc_yAxis(title = list(text = "Expenditures in Dollars"),
#               opposite= FALSE) %>% 
#      hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,1], showInLegend=T, color="blue") %>%
#      #hc_add_series_ts(name="Electricity Trend", ts=energyTrends[,5], showInLegend=F, color="blue") %>%
#      #hc_add_series_ts(name="Gas", ts=energyTimeSeries[,4], color="red") %>%
#      hc_tooltip(valuePrefix="$") %>%
#	  hc_exporting(enabled=T)
#
#    
#  })
#
#  
#  output$PercentEnergy <- renderHighchart({
#    
#      highchart(type="stock") %>%
#        hc_title(useHTML=T, text = "<b>Percent of Total MSU Energy Use</b>") %>%
#        hc_legend(enabled=T) %>%
#        hc_rangeSelector(inputEnabled=F) %>%
#        hc_yAxis(title = list(text = "% of Total Energy"),
#                 opposite=FALSE)%>% 
#        hc_plotOptions(area=list(stacking="percent")) %>%
#        hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,1], color= "yellow", type="area") %>%
#        hc_add_series_ts(name="Natural Gas", ts=energyTimeSeries[,2], color= "red", type="area") %>%
#        hc_tooltip(valueSuffix=" KWH")   
#    
#  })
#
#
#
#  output$PerCapitaEnergy <- renderHighchart({
#    
#    highchart() %>%
#      hc_title(useHTML=T, text = "<b>Annual Per Capita Energy Use</b>") %>%
#      hc_legend(enabled=T, reversed=T) %>%
#      #hc_xAxis(categories= pcwaste$FY, title=list(text="Fiscal Year")) %>%
#      hc_yAxis(title=list(text="Energy Use Per Capita (KWH)"),
#               opposite=FALSE)%>%
#      #hc_plotOptions(column=list(stacking="normal")) %>%
#      hc_add_series(name="Electricity", data=pcEnergy[,1], color= "blue", type="column") %>%
#      hc_add_series(name="Natural Gas", data= pcEnergy[,2], color= "red", type = "column") %>%
#      hc_tooltip(valueSuffix=" KWH")
#  })
#
#  output$PerCapitaEnergyExpend <- renderHighchart({
#    
#    highchart() %>%
#      hc_title(useHTML=T, text = "<b>Annual Per Capita Energy Expenditure</b>") %>%
#      hc_legend(enabled=T, reversed=T) %>%
#      #hc_xAxis(categories= pcwaste$FY, title=list(text="Fiscal Year")) %>%
#      hc_yAxis(title=list(text="Energy Expediture Per Capita (dollars)"),
#               opposite=FALSE)%>%
#      #hc_plotOptions(column=list(stacking="normal")) %>%
#      hc_add_series(name="Electricity", data=pcEnergy[,3], color= "blue", type="column") %>%
#      hc_add_series(name="Natural Gas", data= pcEnergy[,4], color= "red", type = "column") %>%
#      hc_tooltip(valuePrefix="$")
#  })

  ############ Waste Charts ##################
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
    hc_add_series_ts(name="Landfill", ts=wastetimeseries[,2], showInLegend=T, color= "black", type="line") %>%
    hc_add_series_ts(name="Recycle", ts=wastetimeseries[,1], color= "green", type="line") %>%
    hc_add_series_ts(name="Compost", ts=wastetimeseries[,3], color= "orange", type="line") %>%
    hc_add_series_ts(name="Waste Fit", ts=wastefit, color="purple", type="line", showInLegend=F)%>%
    hc_tooltip(valueSuffix=" tons") 
})
    
  output$PercentWaste <- renderHighchart({
      
      highchart(type="stock") %>%
        hc_title(useHTML=T, text = "<b>Percent of MSU Waste Diverted</b>") %>%
        hc_legend(enabled=T) %>%
        hc_rangeSelector(inputEnabled=F) %>%
        hc_yAxis(title = list(text = "% of Total Waste by Weight"),
                 opposite=FALSE)%>% 
        hc_plotOptions(area=list(stacking="percent")) %>%
        hc_add_series_ts(name="Landfill", ts=wastetimeseries[,2], showInLegend=T, color= "black", type="area") %>%
        hc_add_series_ts(name="Recycle", ts=wastetimeseries[,1], color= "green", type="area") %>%
        hc_add_series_ts(name="Compost", ts=wastetimeseries[,3], color= "orange", type="area") %>%
        hc_tooltip(valueSuffix=" tons")   
  })
  
  output$PerCapitaWaste <- renderHighchart({
    
    highchart() %>%
      hc_title(useHTML=T, text = "<b>Per Capita Waste</b>") %>%
      hc_legend(enabled=T, reversed=T) %>%
      hc_xAxis(categories= pcwaste$FY, title=list(text="Fiscal Year")) %>%
      hc_yAxis(title=list(text="Pounds Per Person (lbs)"),
               opposite=FALSE)%>%
      #hc_plotOptions(column=list(stacking="normal")) %>%
      hc_add_series(name="Compost", data=pcwaste[,8], color= "orange", type="column") %>%
      hc_add_series(name="Recycle", data= pcwaste[,6], color= "green", type = "column") %>%
      hc_add_series(name="Landfill", data=pcwaste[,7], showInLegend=T, color= "black", type="column") %>%
      hc_tooltip(valueSuffix="lbs")
  })


  output$leedBuildingMap <- renderLeaflet({
    leaflet(data=leedBuildings) %>%
      addProviderTiles("Esri.WorldImagery",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(~Lon, ~Lat)
  })
  
  output$buildingMap <- renderLeaflet({
    leaflet(data=buildings) %>%
      #addProviderTiles("Esri.WorldImagery",
        #options = providerTileOptions(noWrap = TRUE)
      #) %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = "blue")
  })

  
})