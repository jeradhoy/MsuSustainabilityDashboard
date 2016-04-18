library(shiny)
library(rsconnect)
library(dygraphs)
library(highcharter)
library(stats)



# Define UI for dataset viewer application
shinyUI(navbarPage("MSU SNow",
   tabPanel("Sustainability Dashboard",

		mainPanel(
			tabsetPanel(
			  tabPanel("Energy", icon=icon("bolt"),

				   tags$p("Energy use from electricity and natural gas usage."),
				   highchartOutput("energyExpend")

				   ), 

			  tabPanel("Waste", icon=icon("trash"),
				   tags$p(""),
				   highchartOutput("MSUwaste"),
				   tags$p(""),
				   highchartOutput("PerCapitaWaste")
					   
					   ), 
			  tabPanel("Climate", icon=icon("sun-o"),
					   HTML("Climate Action plan")), 
			  tabPanel("Water", icon=icon("tint")),
			  tabPanel("Food", icon=icon("cutlery"))
			)
		)
	),
	tabPanel("About")
))
