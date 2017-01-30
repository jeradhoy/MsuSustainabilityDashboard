library(shiny)
library(highcharter)
library(leaflet)
library(shinythemes)

 ######## Home ########
# Define UI for dataset viewer application
shinyUI(navbarPage(id="main", #title="MSU Sustainability Dashboard",
  fluid=T,
  position="static-top",
  selected="tabMap",
  inverse=T, #For dark top
  collapsible = T,
  theme=shinytheme("cerulean"),
  tags$head(
    tags$link(rel= "stylesheet", type="text/css", href = "styles.css")
  ),

  shinyjs::useShinyjs(),

  #Start first tab, Info
  tabPanel(title = "Home", value="tabHome",
    tags$div(align="center",
      tags$h1("Montana State University Sustainability Dashboard"),
      tags$p("click on an icon to explore data, or ",
        actionLink(inputId="openTabAbout", label="learn about the Sustustainability Dashboard")),
      fluidRow(
        column(4,
          actionLink("openTabEnergy", HTML('<h2>Energy</h2><i class="fa fa-lightbulb-o fa-5x"></i>'))),
        column(4,
          actionLink("openTabWaste", HTML('<h2>Waste</h2><i class="fa fa-trash fa-5x"></i>'))),
        column(4,
          actionLink("openTabGHG", HTML('<h2>Greenhouse Gases</h2><i class="fa fa-globe fa-5x"></i>'))
      )),
      fluidRow(
        column(4,
          actionLink("openTabWater", HTML('<h2>Water</h2><i class="fa fa-tint fa-5x"></i>'))),
        column(4,
          actionLink("openTabMap", HTML('<h2>Buildings & Landscaping</h2><i class="fa fa-home fa-5x"></i>'))),
        column(4,
          actionLink("openTabFood", HTML('<h2>Food</h2><i class="fa fa-apple fa-5x"></i>'))
      )),
     fluidRow(
       column(4,
          actionLink("openTabProjects", HTML('<h2>Projects</h2><i class="fa fa-gears fa-5x"></i>'))
      ))
     )
    ),

  ########### Energy ################
  tabPanel(title = "Energy", value="tabEnergy", icon=icon("lightbulb-o"),
    tags$h1("Energy"),
    tags$p("For electricity and natural gas usage, MSU's Climate Action Plan aims to hold these parameters constant with ideally a negative growth trend but a maximum of ",
      tags$b("0.25% growth per year"),
      ". To put this in perspective, in 2009 electricity consumption was growing at a rate of 1.6% and natural gas was growing at a rate of 1.3%."),
    tags$p("In 2009, purchased electricity accounted for 27% MSU’s net emissions and was responsible for 20,564 MT of CO2 equivalents. Combusting fossil fuels such as gas and coal accounted for an additional 27% of emissions and was responsible for 21,099 MT of CO2 equivalents. The average Montanan in 2013 caused about 31.3 MT of energy-related CO2 emissions, while the national average was 16.7 MT of CO2 equivalent."),


    sidebarLayout(
      sidebarPanel(

        tags$h5("Select Data"),
        #Checkbox Group Input: Choose Electriciy or naturalgas or both
        checkboxInput("elec", label = "Electricity", value = TRUE),
        checkboxInput("gas", label = "Natural Gas", value = TRUE),

        #Radio Button: Choose Usage or expenditure
        radioButtons("usageOrExpendRadio", label = h5("Usage or Expenditure"),
          choices = list("Usage" = 0, "Expenditure" = 1),
          selected = 0),

        #Radio Button: Choose Total or Per capita
        #radioButtons("totalOrPercapitaRadio", label = h5("Total or Per Capita"),
        #  choices = list("Total" = 1, "Per Capita" = 2),
        #  selected = 1),
        tags$h5("Trend Lines"),
        checkboxInput("elecTrendLine", label="Electricity", value=FALSE),
        checkboxInput("gasTrendLine", label="Gas", value=FALSE)
        #radioButtons("energyTimeStep", label = h5("Total or Per Capita"),
          #choices = list("Monthly" = 1, "Annual" = 2),
          #selected = 1)
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Line Plot",

            highchartOutput("energyUsage", height = "500px")
            #, verbatimTextOutput("energyDebug")

          )
        )
      )
    )
  ),


  ########### Waste ################
  tabPanel(title = "Waste", value="tabWaste", icon=icon("trash"),

    tags$h1("Waste"),
    tags$p("In 2009, Montana State University published its first Climate Action Plan (CAP).
    This document outlines goals and objectives for reducing MSU's climate impact.
    Baseline data for 2009 found that solid waste accounted for 3% of MSU's net
    emissions totaling 2,132 MT of carbon dioxide equivalents per year."),

    tags$p("The Waste Reduction goals are as follows: reduce the total weight of waste to ",
      tags$b("25% of 2009 levels by 2020, 50% by 2030, 65% by 2040, and 80% by 2050."),
    "In order to meet these goals, MSU started a recycling program in 2009, started recycling E-waste in 2012, and is currently developing a composting program.
  It will require envolvement from the whole student body and falculty to reach these ambitious goals."),
    sidebarLayout(
      sidebarPanel(
        tags$h5("Select Data"),
        #Checkbox Group Input: Choose Electriciy or naturalgas or both
        checkboxInput("landfill", label = "Landfill", value = TRUE),
        checkboxInput("recycle", label = "Recycle", value = TRUE),
        checkboxInput("compost", label = "Compost", value = TRUE),

        #Radio Button: Choose Total or Per capita
        #radioButtons("totalOrPercapitaRadioWaste",
        #  label = h5("Total or Per Capita"),
        #  choices = list("Total" = 1, "Per Capita" = 2),
        #  selected = 1),
        tags$h5("Trend Line"),
        checkboxInput("landfillTrendLine", label="Landfill", value=FALSE),
        checkboxInput("recycleTrendLine", label="Recycle", value=FALSE),
        checkboxInput("compostTrendLine", label="Compost", value=FALSE)
    ),

      mainPanel(
        tabsetPanel(
          tabPanel("Area Plot",
            highchartOutput("PercentWaste", height = "500px")),
          tabPanel("Line Plot",
            highchartOutput("MSUwaste", height = "500px"))
        )
      )
    )
  ),

  ########### GHG ################
  tabPanel(title = "GHG", value="tabGHG", icon=icon("globe"),
    tags$h1("Greenhouse Gas Emissions"),
    HTML("Climate Action plan")
  ),

  ########### Water ################
  tabPanel(title = "Water", value="tabWater", icon=icon("tint"),
    tags$h1("Water"),
    sidebarLayout(
      sidebarPanel(

        #Radio Button: Choose Usage or expenditure
        radioButtons("waterUsage", label = h5("Usage or Expenditure"),
          choices = list("Usage" = 0, "Expenditure" = 1),
          selected = 0),

        #Radio Button: Choose Total or Per capita
        #radioButtons("waterPerCap", label = h5("Total or Per Capita"),
        #  choices = list("Total" = 1, "Per Capita" = 2),
        #  selected = 1),
        tags$h5("Trend Line"),
        checkboxInput("waterTrendLine", label="Water", value=FALSE)
      ),

      mainPanel(

        tabsetPanel(
          tabPanel("Line Plot",

            highchartOutput("waterSewer", height = "500px")
            #verbatimTextOutput("energyDebug")

          )
        )
      )
  )
  ),

 #tabPanel(title = "Food", value="tabFood", icon=icon("cutlery")),

  ########### Leed ################
  tabPanel(title = "Buildings & Landscaping", value="tabMap", icon=icon("home"),
    div(class="outer",
      #{tags$style(type = "text/css",
       # "div.outerMap {position: fixed; top: 48px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = T, top =70, left = "auto", right =20, bottom = "auto",
        width =250, height = "auto",
        h2("Buildings and Landscaping"),
        h3("Show Layers:"),
          checkboxInput("showLeed", label=tags$div(tags$b("LEED Buildings"), tags$img(src="assets/UWIcons/1l0-e0-e0-d-certification-icon.png")), value=T),
          checkboxInput("showEdible", label=tags$div(tags$b("Landscaping"), tags$img(src="assets/UWIcons/3brockman-tree-tour-icon.png", tags$img(src="assets/UWIcons/3garden-icon.png"))), value=T),
        checkboxInput("showProject", label=tags$div(tags$b("Projects"), tags$img(src="assets/UWIcons/6on-site-composting-icon.png"), tags$img(src="assets/UWIcons/1solar-panels-icon.png")), value=T),

        verbatimTextOutput("mapDebug")
      ),
      shinyjs::hidden(
      absolutePanel(
        id = "buildingGraphs", class = "panel panel-default", fixed = TRUE,
        draggable = F, bottom = 70, left = 70, right = "auto", top = "auto",
        width = "40%", height = "auto",
        fluidRow(
          column(12, align="center",
               highchartOutput("buildingKwhChart", height = "100%")
          )
        ),

        fluidRow(
          column(12, align="center",
               highchartOutput("buildingWaterChart", height = "100%")
          )
        ),
        fluidRow(
          column(12, align="right",
            actionButton("showBuildingGraphsButton", label="Close")
          )
        )
      )
    )
    )
  ),


 ########## Food #################
 tabPanel(title = "Food", value = "tabFood", icon = icon("apple"),
          tags$h1("Food")
 ),

 ########### Projects ################
 tabPanel(title = "Projects", value="tabProjects", icon=icon("gears"),
    tags$h1("Projects"),
    highchartOutput("montanaMade", height = "500px")
 ),

  ########### About ################
    navbarMenu("About",
        tabPanel("About", value="tabAbout",
          tags$h3("This web app was developed in collaboration with Sustainability Now, the MSU Office of Sustainability, and MSU Facilities Services")
          ),
        tabPanel("Data Sources",
          h2("Data Sources"),
          sidebarLayout(
            sidebarPanel(
              selectInput("dataset", "Choose a dataset:",
                choices = c("Energy" = "energy", "Leed Buildings" = "leed", "Per Capita Waste" = "perCapita", "Waste" = "waste", "Landscaping" = "landscaping", "Projects" = "projectMap", "Building Data" = "buildingUtilities")),
              downloadButton('downloadData', 'Download')
            ),
            mainPanel(
              DT::dataTableOutput('dataTable')
            )
          )
        )
    )
))
