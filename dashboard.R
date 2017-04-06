library(shinydashboard)
source("global.r")
source("modules.R")
source("server.R")

dataChange <- lapply(appData$energyTs, function(x){
  tail(diff(aggregate(x, nfrequency=1)), 1)
  })

uiDash <- dashboardPage(
  dashboardHeader(title = "MSU Sustainability"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard")),
      menuItem("Map", tabName="map", icon=icon("map")),
      menuItem("Data", tabName="data", icon=icon("database")),
      checkboxInput("annual", label = "Aggregate to Annual Values", value = F)

    )
  ),

  dashboardBody(
    tabItems(

      tabItem(tabName="dashboard",
        fluidRow(
          infoBox("Electricity", paste(formatC(round(dataChange$ElecKWH), format="d", big.mark=","), "KWH"), "Change from 2014-2015", icon=icon("lightbulb-o"), color=ifelse(dataChange$ElecKWH > 0, "red", "green"), fill=T),
          infoBox("Natural Gas", paste(formatC(round(dataChange$GasKWH), format="d", big.mark=","), "KWH"), "Change from 2014-2015", icon=icon("cloud"), color=ifelse(dataChange$GasKWH > 0, "red", "green"), fill=T),
          infoBox("Water", paste(formatC(round(dataChange$WaterMCF), format="d", big.mark=","), "MCF"), "Change from 2014-2015", icon=icon("tint"), color=ifelse(dataChange$WaterMCF > 0, "red", "green"), fill=T)
        ),
        fluidRow(
          tabBox(title="Energy",
            tabPanel("Usage",
              highLinePlotOutput("energyUsage")
            ),
            tabPanel("Expenditure",
              highLinePlotOutput("energyExpend")
            )
          ),
          tabBox(title="Waste",
            tabPanel("Area",
              highLinePlotOutput("wasteArea")
            ),
            tabPanel("Line",
              highLinePlotOutput("wasteLine")
            )
          )
        ),
        fluidRow(
          tabBox(title="Water", width=8,
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

      tabItem(title="Data Sources", tabName="data",

        dataSourceUI("dataSources")
      )

    )
  )
)

shinyApp(uiDash, server)
