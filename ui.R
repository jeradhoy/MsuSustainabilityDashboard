library(shiny)
library(rsconnect)
library(highcharter)
library(stats)
library(leaflet)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(navbarPage(title="MSU Sustainability Dashboard",
  fluid=T, selected="Green Building & Landscaping",
  inverse=T, #For dark top
  collapsible = T,
  theme=shinytheme("cerulean"),
  includeCSS("styles.css"),

  #Start first tab, Info
  tabPanel("Info", icon=icon("info"),
    "This web app was developed in collaboration with Sustainability Now, the MSU Office of Sustainability, and MSU Facilities Services"
  ),

  tabPanel("Energy", icon=icon("bolt"),
    tags$h1("Energy"),
    tags$p("For electricity and natural gas usage, MSU's Climate Action Plan aims to hold these parameters constant with ideally a negative growth trend but a maximum of ",
      tags$b("0.25% growth per year"),
      ". To put this in perspective, in 2009 electricity consumption was growing at a rate of 1.6% and natural gas was growing at a rate of 1.3%."),
    tags$p("In 2009, purchased electricity accounted for 27% MSU’s net emissions and was responsible for 20,564 MT of CO2 equivalents. Combusting fossil fuels such as gas and coal accounted for an additional 27% of emissions and was responsible for 21,099 MT of CO2 equivalents. The average Montanan in 2013 caused about 31.3 MT of energy-related CO2 emissions, while the national average was 16.7 MT of CO2 equivalent."),

    sidebarLayout(
      sidebarPanel(

        #tags$h3("Options"),
        #Checkbox Group Input: Choose Electriciy or naturalgas or both
        checkboxInput("elec", label = "Electricity", value = TRUE),
        checkboxInput("gas", label = "Natural Gas", value = TRUE),

        #Radio Button: Choose Usage or expenditure
        radioButtons("usageOrExpendRadio", label = h5("Usage or Expenditure"),
        choices = list("Usage" = 0, "Expenditure" = 1),
        selected = 0),

        #Radio Button: Choose Total or Per capita
        radioButtons("totalOrPercapitaRadio", label = h5("Total or Per Capita"),
        choices = list("Total" = 1, "Per Capita" = 2),
        selected = 1)
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Line Plot",

            highchartOutput("energyUsage", height="500px", width="100%"),
            verbatimTextOutput("energyDebug")

          )
        )
      )
    )
  ),


  tabPanel("Waste", icon=icon("trash"),

  tags$p("In 2009, Montana State University published its first Climate Action Plan (CAP).
  This document outlines goals and objectives for reducing MSU's climate impact.
  Baseline data for 2009 found that solid waste accounted for 3% of MSU's net
  emissions totaling 2,132 MT of carbon dioxide equivalents per year."),

  tags$p("The Waste Reduction goals are as follows: reduce the total weight of waste to ",
      tags$b("25% of 2009 levels by 2020, 50% by 2030, 65% by 2040, and 80% by 2050."),
    "In order to meet these goals, MSU started a recycling program in 2009, started recycling E-waste in 2012, and is currently developing a composting program.
  It will require envolvement from the whole student body and falculty to reach these ambitious goals."),
    sidebarLayout(
      sidebarpanel(
        #tags$h3("options"),
        #Checkbox Group Input: Choose Electriciy or naturalgas or both
        checkboxInput("landfill", label = "Landfill", value = TRUE),
        checkboxInput("recycle", label = "Recycling", value = TRUE),
        checkboxInput("compost", label = "Compost", value = TRUE),

        #Radio Button: Choose Total or Per capita
        radioButtons("totalOrPercapitaRadioWaste",
          label = h5("Total or Per Capita"),
          choices = list("Total" = 1, "Per Capita" = 2),
          selected = 1)
    ),

      mainPanel(
        tabsetPanel(
          tabPanel("Area Plot",
          highchartOutput("PercentWaste", height="500px")),
          tabPanel("Line Plot",
          highchartOutput("MSUwaste", height="500px"))
        )
      )
    )
  ),

  tabPanel("Climate", icon=icon("sun-o"),
    HTML("Climate Action plan")
  ),

  tabPanel("Water", icon=icon("tint"),
    sidebarLayout(
      sidebarPanel(

        #Radio Button: Choose Usage or expenditure
        radioButtons("waterUsage", label = h5("Usage or Expenditure"),
          choices = list("Usage" = 0, "Expenditure" = 1),
          selected = 0),

        #Radio Button: Choose Total or Per capita
        radioButtons("waterPerCap", label = h5("Total or Per Capita"),
          choices = list("Total" = 1, "Per Capita" = 2),
          selected = 1)
      ),

      mainPanel(

        tabsetPanel(
          tabPanel("Line Plot",

            highchartOutput("waterSewer", height="500px", width="100%")
            #verbatimTextOutput("energyDebug")

        )
        )
      )
  )
  ),

 #tabPanel("Food", icon=icon("cutlery")),

  tabPanel("Green Building & Landscaping", icon=icon("home"),
    div(class="outerMap",
      tags$style(type = "text/css",
        "div.outerMap {position: fixed; top: 47px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      leafletOutput("leedBuildingMap", width="100%", height="100%")
    )
  ),
    navbarMenu("About",
        tabPanel("About"),
        tabPanel("Data Sources"),
        tabPanel("Contribute!")
    )
))
