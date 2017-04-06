uiPublic <- navbarPage(id="main", #title="MSU Sustainability Dashboard",
  fluid=T,
  position="static-top",
  selected="tabHome",
  inverse=T, #For dark top
  collapsible = T,
  theme=shinytheme("cerulean"),

  tags$head(
    tags$link(rel= "stylesheet", type="text/css", href = "styles.css")
  ),

  shinyjs::useShinyjs(),

  tags$div(style="position: absolute; z-index:1000",
    absolutePanel(id = "shareLink", fixed = TRUE,
      draggable = F, top = "auto", left = 10, right = "auto", bottom = 10,
      width = "auto", height = "auto",
      bookmarkButton(label="Share Link")
    )
  ),

  ######## Home ########
  #Start first tab, Info
  tabPanel(title = "Home", value="tabHome",
    tags$div(align="center",
      tags$h1("Montana State University Sustainability Dashboard"),
      tags$p("click on an icon to explore data, or ",
        actionLink(inputId="openTabAbout", label="learn about the Sustustainability Dashboard")),
      fluidRow(
        column(3,
          actionLink("openTabEnergy", HTML('<h2>Energy</h2><i class="fa fa-lightbulb-o home-icon"></i>'))),
        column(3,
          actionLink("openTabWaste", HTML('<h2>Waste</h2><i class="fa fa-trash home-icon"></i>'))),
        column(3,
          actionLink("openTabGHG", HTML('<h2>Greenhouse Gases</h2><i class="fa fa-globe home-icon"></i>'))),
        column(3,
          actionLink("openTabWater", HTML('<h2>Water</h2><i class="fa fa-tint home-icon"></i>')))
      ),
      fluidRow(
        column(4,
          actionLink("openTabMap", HTML('<h2>Buildings & Landscaping</h2><i class="fa fa-home home-icon"></i>'))),
        column(4,
          actionLink("openTabFood", HTML('<h2>Food</h2><i class="fa fa-cutlery home-icon"></i>'))),
       column(4,
          actionLink("openTabProjects", HTML('<h2>Projects</h2><i class="fa fa-gears home-icon"></i>'))
        ))
      )),

  ########### Energy ################
  tabPanel(title = "Energy", value="tabEnergy", icon=icon("lightbulb-o"),
    fluidRow(column(10, offset=1,
      tags$h1("Energy"),
      tags$p("For electricity and natural gas usage, MSU's ",
      tags$a(href="http://www.montana.edu/sustainability/projectsandinitiatives/climateactionplan.html", "Climate Action Plan"),
      " aims to hold these parameters constant with ideally a negative growth trend but a maximum of ",
        tags$b("0.25% growth per year"),
        ". To put this in perspective, in 2009 electricity consumption was growing at a rate of 1.6% and natural gas was growing at a rate of 1.3%."),
      tags$p("In 2009, purchased electricity accounted for 27% MSU’s net emissions and was responsible for 20,564 MT of CO2 equivalents. Combusting fossil fuels such as gas and coal accounted for an additional 27% of emissions and was responsible for 21,099 MT of CO2 equivalents. The average Montanan in 2013 caused about 31.3 MT of energy-related CO2 emissions, while the national average was 16.7 MT of CO2 equivalent."),

      tabsetPanel(
        tabPanel("Usage",
                 highLinePlotOutput("energyUsage")
        ),
        tabPanel("Expenditure",
                 highLinePlotOutput("energyExpend")
        )
      )
    )
    )
  ),


  ########### Waste ################
  tabPanel(title = "Waste", value="tabWaste", icon=icon("trash"),

    fluidRow(column(10, offset=1,
    tags$h1("Waste"),
    tags$p("In 2009, Montana State University published its first ",
    tags$a(href="http://www.montana.edu/sustainability/projectsandinitiatives/climateactionplan.html", "Climate Action Plan"),
    " (CAP).
    This document outlines goals and objectives for reducing MSU's climate impact.
    Baseline data for 2009 found that solid waste accounted for 3% of MSU's net
    emissions totaling 2,132 MT of carbon dioxide equivalents per year."),

    tags$p("The Waste Reduction goals are as follows: reduce the total weight of waste to ",
      tags$b("25% of 2009 levels by 2020, 50% by 2030, 65% by 2040, and 80% by 2050."),
    "In order to meet these goals, MSU started a recycling program in 2009, started recycling E-waste in 2012, and is currently developing a composting program.
  It will require involvement from the whole student body and falculty to reach these ambitious goals."),

        tabsetPanel(
          tabPanel("Area Plot",
            highLinePlotOutput("wasteArea")
          ),
          tabPanel("Line Plot",
            highLinePlotOutput("wasteLine")
          )
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
    fluidRow(column(10, offset=1,
    tags$h1("Water"),
        tabsetPanel(
          tabPanel("Water Use",
            highAreaPlotOutput("waterUse")
          ),
          tabPanel("Expenditure",
            highLinePlotOutput("waterSewerExpend")
          )
        )
    )
    )
  ),


  ########### Leed ################
  tabPanel(title = "Buildings & Landscaping", value="tabMap", icon=icon("home"),
    div(class="outer",
      buildingMapUI("leafletMap"),

      shinyjs::hidden(

      absolutePanel(
        id = "buildingGraphs", class = "panel panel-default", fixed = TRUE,
        draggable = F, top = 70, left = 20, right = "auto", bottom = "auto",
        width = "40%", height = "auto",

        tabsetPanel(
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
        ),
        fluidRow(align="center",
            actionButton("showBuildingGraphsButton", label="Close")
        )
      )
      )
    )
  ),

  ########## Food #################
  tabPanel(title = "Food", value = "tabFood", icon = icon("cutlery"),
    fluidRow(column(10, offset=1,
    tags$h1("Food"),

    tabsetPanel(
      tabPanel(title = "Purchases",
        montanaMadeColumnOutput("montMadePurch")
      ),
      tabPanel(title = "Total",
        montanaMadeBarOutput("montMadeTotal")
      )
    )
    )
    )
  ),

 ########### Projects ################
 tabPanel(title = "Projects", value="tabProjects", icon=icon("gears"),
    tags$h1("Projects")
 ),

  ########### About ################
  navbarMenu("About",
      tabPanel("About", value="tabAbout",
        tags$h3("This web app was developed in collaboration with Sustainability Now, the MSU Office of Sustainability, and MSU Facilities Services")
        ),
      tabPanel("Data Sources",
        fluidRow(column(10, offset=1,
          h2("Data Sources"),
          dataSourceUI("dataSources")
        ))
      ))
)
