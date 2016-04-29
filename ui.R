library(shiny)
library(rsconnect)
library(dygraphs)
library(highcharter)
library(stats)

# Define UI for dataset viewer application
shinyUI(navbarPage(title="MSU Sustainability Dashboard",
							tabPanel("Energy", icon=icon("bolt"),
									 
									 tags$p("For electricity and natural gas usage, Montana State's Climate Action Plan aims to hold these parameters constant with ideally a negative growth trend but a maximum of 0.25% growth per year. To put this in perspective, in 2009 electricity consumption was growing at a rate of 1.6% and natural gas was growing at a rate of 1.3%."),
									 tags$p("In 2009, purchased electricity for 27% MSU’s net emissions and was responsible for 20,564 MT of CO2 equivalents. Combusting fossil fuels such as gas and coal accounted for an additional 27% of emissions and was responsible for 21,099 MT of CO2 equivalents. The average Montanan in 2013 caused about 31.3 MT of energy-related CO2 emissions, while the national average was 16.7 MT of CO2 equivalent."),
									 highchartOutput("energyUsage", height="500px"),
									 highchartOutput("energyExpend", height="500px"),
									 highchartOutput("PerCapitaEnergy", height="500px"),
									 highchartOutput("PerCapitaEnergyExpend", height="500px")
									 #highchartOutput("PercentEnergy", height="500px")
									 
							), 
							
							tabPanel("Waste", icon=icon("trash"),
									 tags$p("In 2009, Montana State University published its first Climate Action Plan (CAP).
											 This document outlines goals and objectives for reducing MSU's climate impact. 
											 Baseline data for 2009 found that solid waste accounted for 3% of MSU's net 
											emissions totaling 2,132 MT of carbon dioxide equivalents per year."), 
											tags$p("The Waste 
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

