library(shiny)
library(rsconnect)
library(dygraphs)
library(highcharter)

# get
hcopts <- getOption("highcharter.options")
# explore
hcopts
# override
hcopts$lang$thousandsSep <- ","
# update
options(highcharter.options = hcopts)

energyData <- read.csv("./Data/msuUtilites2010-2015.csv")
#names(energyData)
#strsplit(names(energyData), ".", fixed=T)
energyTimeSeries <- ts(energyData[,-c(1,2)], frequency=12, start=c(2010, 1))
#head(energyTimeSeries)
#colnames(energyTimeSeries)
#hchart(energyTimeSeries[,1]) %>%
#	  hc_title(text = "MSU Energy Usage in Kilowatts per Month")

#energyTimeSeries[,5]
#
##### Energy Expendetures
#highchart(type="stock") %>%
#	hc_title(text = "MSU Expenditure on Electricity, Gas, and Water/Sewer") %>%
#	hc_legend(enabled=T) %>%
#	hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,5], showInLegend=T) %>%
#	hc_add_series_ts(name="Gas", ts=energyTimeSeries[,6]) %>%
#	hc_add_series_ts(name="Water/Sewer", ts=energyTimeSeries[,7]) %>%
#	hc_tooltip(valuePrefix="$")
#
##### Electricity and Natural Gas Usage
#highchart(type="stock") %>%
#	hc_title(text = "MSU Electricity and Natural Gas Usage") %>%
#	hc_legend(enabled=T) %>%
#	hc_yAxis(
#		list(
#			 title = list(text= "Electricity (KWH)"),
#			 align = "left",
#			 showFirstLabel=F,
#			 showLastLabel=F,
#			 labels = list(format = "{value} KWH"),
#			 opposite=T
#			 ),
#		list(
#			 title = list(text= "Natural Gas (DKT)"),
#			 align = "right",
#			 showFirstLabel=F,
#			 showLastLabel=F,
#			 labels = list(format = "{value} DKT")
#			 )
#		) %>%
#	hc_add_series_ts(name="Natural Gas", ts=energyTimeSeries[,3], yAxis=1) %>%
#	hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,2])  %>%
#	hc_rangeSelector()
#
#hchart(energyTimeSeries[,5:7]) %>%
#	  hc_title(text = "MSU Energy Usage in Kilowatts per Month") %>%
#	hc_legend(align = "left", verticalAlign = "top", layout = "vertical", x = 0, y = 100)
#



shinyServer(function(input, output) {

  output$energyExpend <- renderHighchart({

	highchart(type="stock") %>%
		hc_title(text = "MSU Expenditure on Electricity, Gas, and Water/Sewer") %>%
		hc_legend(enabled=T) %>%
		hc_rangeSelector(inputEnabled=F) %>%
		hc_add_series_ts(name="Electricity", ts=energyTimeSeries[,5], showInLegend=T) %>%
		hc_add_series_ts(name="Gas", ts=energyTimeSeries[,6]) %>%
		hc_add_series_ts(name="Water/Sewer", ts=energyTimeSeries[,7]) %>%
		hc_tooltip(valuePrefix="$")

  })

})

