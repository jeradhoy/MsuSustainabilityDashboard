#Modules
highLinePlotOutput <- function(id){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), height = "500px")

}


highLinePlot <- function(input, output, session, dataTs, trends, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%

      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T, verticalAlign="bottom", align="center", layout="horizontal") %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = list(text = ylab), opposite= FALSE) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)

    for(i in 1:ncol(dataTs)){
      hchart <- hchart %>%
        hc_add_series(dataTs[, i], color=colors[i], name=tsNames[i])
    }

    if(!is.null(trends)){
      for(i in 1:ncol(trends)){
        hchart <- hchart %>%
          hc_add_series(trends[, i], color=colors[i], name=paste(tsNames[i], "Trend"), showInLegend=T, visible=F)
      }
    }

    return(hchart)
  })

}

highAreaPlotOutput <- function(id){

  ns <- NS(id)

  highchartOutput(ns("highPlot"), height = "500px")

}

highAreaPlot <- function(input, output, session, dataTs, plotTitle, tsNames, ylab, colors, toolSuffix, toolPrefix){

  output$highPlot <- renderHighchart({
    hchart <- highchart(type="stock") %>%
      hc_title(useHTML=T, text = plotTitle) %>%
      hc_legend(enabled=T) %>%
      hc_rangeSelector(inputEnabled=F) %>%
      hc_yAxis(title = ylab, opposite=FALSE) %>%
      hc_plotOptions(area=list(stacking="percent")) %>%
      hc_tooltip(valueSuffix=toolSuffix, valuePrefix=toolPrefix)

    for(i in 1:ncol(dataTs)){
      hchart <- hchart %>%
        hc_add_series(dataTs[, i], color=colors[i], name=tsNames[i], type="area")
    }

    return(hchart)
  })

}
