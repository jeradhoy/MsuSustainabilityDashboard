library(shiny)
library(rsconnect)
library(dygraphs)
library(highcharter)
library(stats)

# Define UI for dataset viewer application
shinyUI(fluidPage(title="MSU SNow",
						mainPanel(width=8,
						  tabsetPanel(
							tabPanel("Energy", icon=icon("bolt"),
									 
									 tags$p("Energy use from electricity and natural gas usage."),
									 highchartOutput("energyExpend", height="500px"),
									 highchartOutput("energyUsage", height="500px"),
									 highchartOutput("PercentEnergy", height="500px")
									 
							), 
							
							tabPanel("Waste", icon=icon("trash"),
									 tags$p("In 2009, Montana State University published its first Climate Action Plan (CAP).
											 This document outlines goals and objectives for reducing MSU's climate impact. 
											 Baseline data for 2009 found that solid waste accounted for 3% of MSU's net 
											emissions totaling 2,132 MT of carbon dioxide equivalents per year.  The Waste 
											Reduction goals are as follows: reduce the total weight of waste to 25% of 2009 
											levels by 2020, 50% by 2030, 65% by 2040, and 80% by 2050.

											 In order to meet these goals, MSU started a recycling program in 2009, started
											recycling E-waste in 2012, and is currently developing a composting program. 
											 It will require envolvement from the whole student body and falculty to reach 
											these ambitious goals."),
											
									 highchartOutput("MSUwaste", height="500px"),
									 tags$p(""),
									 highchartOutput("PercentWaste", height="500px"),
									 tags$p(""),
									 highchartOutput("PerCapitaWaste", height="500px")
							), 
							
							tabPanel("Climate", icon=icon("sun-o"),
									 HTML("Climate Action plan")), 
							tabPanel("Water", icon=icon("tint")),
							tabPanel("Food", icon=icon("cutlery"))
						  )
						)
			   )
)

