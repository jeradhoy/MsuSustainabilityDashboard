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
head(energyTimeSeries)
colnames(energyTimeSeries)
hchart(energyTimeSeries[,1]) %>%
		  hc <- title(text = "MSU Energy Usage in Kilowatts per Month")

energyTimeSeries[,5]


rainfall <- c(49.9, 71.5, 106.4, 129.2, 144, 176,
              135.6, 148.5, 216.4, 194.1, 95.6, 54.4)

temperature <- c(7, 6.9, 9.5, 14.5, 18.2, 21.5,
                 25.2, 26.5, 23.3, 18.3, 13.9, 9.6)

col1 <- hc_get_colors()[3]
col2 <- hc_get_colors()[2]

highchart(type="stock") %>% 
  hc_title(text = "Tokyo Climate") %>% 
  hc_legend(enabled = FALSE) %>% 
  hc_xAxis(categories = month.abb) %>% 
  hc_yAxis(
    list(
      title = list(text = "Temperature"),
      align = "left",
      showFirstLabel = FALSE,
      showLastLabel = FALSE,
      labels = list(format = "{value} &#176;C", useHTML = TRUE)
    ),
    list(
      title = list(text = "Rainfall"),
      align = "right",
      showFirstLabel = FALSE,
      showLastLabel = FALSE,
      labels = list(format = "{value} mm"),
      opposite = TRUE
    )
  ) %>% 
  hc_tooltip(formatter = JS("function(){
                             if('Sunshine' == this.series.name){
                             return  '<b>' + this.point.name + ': </b>' + this.y
                             } else {
                             unts = this.series.name == 'Rainfall' ? 'mm' : '&#176;C';
                             return (this.x + ': ' + this.y + ' ' + unts)
                             }}"),
             useHTML = TRUE) %>% 
  hc_add_series(name = "Rainfall",
                data = rainfall, yAxis = 1) %>% 
  hc_add_series(name = "Temperature",
                data = temperature) %>% 
  hc_add_series(name = "Sunshine", type = "pie",
                data = list(list(y = 2020, name = "Sunshine hours",
                                 sliced = TRUE, color = col1),
                            list(y = 6740, name = "Non sunshine hours (including night)",
                                 color = col2,
                                 dataLabels = list(enabled = FALSE))),
                center = c('20%', 45),
                size = 80)
