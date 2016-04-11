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
				   #plotOutput("distPlot"),
			   #sliderInput("years", "Select Years",min=1950, max=2016, value=c(1980, 2016), sep=""),
				   #dygraphOutput("dyg1"),
				   highchartOutput("energyExpend")
				   ), 
			  tabPanel("Waste", icon=icon("trash")), 
			  tabPanel("Climate", icon=icon("sun-o"),
					   HTML("Climate Action plan")), 
			  tabPanel("Water", icon=icon("tint")),
			  tabPanel("Food", icon=icon("cutlery"))
			  #tabPanel("Solar",HTML("MEOW")),
			  #tabPanel("Building", HTML("MEOW")),
			  #tabPanel("Food", HTML("MEOW")),
			  #tabPanel("Curriculum", HTML("MEOW")),
			  #tabPanel("Engagement", HTML("MEOW"))
			)
		)
	),
	tabPanel("About")
))
