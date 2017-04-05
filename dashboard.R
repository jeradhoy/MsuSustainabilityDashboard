library(shinydashboard)
source("global.r")
source("modules.R")
source("server.R")

uiDash <- dashboardPage(
  dashboardHeader(title = "MSU Sustainability"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard")),
      menuItem("Map", tabName="map", icon=icon("map")),
      menuItem("Data", tabName="data", icon=icon("database")),
      checkboxInput("showTrends", label = "Show Trendlines", value = F),
      checkboxInput("showPredict", label = "Predict Future Values", value = F)


    )
  ),

  dashboardBody(
    tabItems(

      tabItem(tabName="dashboard",
        fluidRow(
          tabBox(title="Energy",
            tabPanel("Usage",
              highLinePlotOutput("energyUsage")
            ),
            tabPanel("Expenditure",
              highLinePlotOutput("energyExpend")
            )
          ),
          tabBox(title="Area",
            tabPanel("Line",
              highLinePlotOutput("wasteArea")
            ),
            tabPanel("Expenditure",
              highLinePlotOutput("wasteLine")
            )
          )
        ),
        fluidRow(
          tabBox(title="Water", width=12,
            tabPanel("Usage",
              highLinePlotOutput("waterUse")
            ),
            tabPanel("Expenditure",
              highLinePlotOutput("waterSewerExpend")
            )
          )

        )
      ),

      tabItem(tabName="map",
        fluidRow(
            box(width=8, solidHeader=T,
              buildingMapUI("leafletMap", mapHeight = 500)
            ),
            box(width=4,
              selectInput("selectBld", label = h3("Select Building"),
                          choices = appData$bld$BldgName,
                          selectize = T)
            )
        ),
        fluidRow(
          tabBox(width=9,
            tabPanel("Energy",
                     highLinePlotOutput("bldEnergy", plotHeight="100%", plotWidth="95%")
            ),
            tabPanel("Gas",
                     highLinePlotOutput("bldGas", plotHeight="100%", plotWidth="95%")
            ),
            tabPanel("Water",
                     highLinePlotOutput("bldWater", plotHeight="100%", plotWidth="95%")
            ),
            tabPanel("Steam",
                     highLinePlotOutput("bldSteam", plotHeight="100%", plotWidth="95%")
            )
          )
        )
      ),

      tabItem(tabName="data",
        dataSourceUI("dataSourceTable")
      )

    )
  )
)

shinyApp(ui, server)
