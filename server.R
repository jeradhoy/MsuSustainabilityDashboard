library(shiny)
library(rsconnect)
library(dygraphs)
library(highcharter)

randomData <- data.frame(seq(to=2016, length=150), runif(150, 100, 150),
		runif(150, 50, 80),
		runif(150, 70, 110),
		runif(150, 110, 130))

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
	
	plot(input$years[1]:input$years[2], randomData[input$years[1]:input$years[2]-1949, 2], type="l", col="blue")
	lines(input$years[1]:input$years[2], randomData[input$years[1]:input$years[2]-1949, 5], type="l", col="red")

  })

  output$dyg1 <- renderDygraph({
	  dygraph(randomData, main="Electricity and Natural Gas Usage") %>% dyRangeSelector()
  })

  output$high1 <- renderHighchart({

	  rownames(randomData) <- randomData[,1]
	  randomData <- as.ts(randomData[,-1], start=1867)

	#highchart(type = "stock") %>% 
	hchart(randomData) %>%
	  hc_title(text = "Monthly Deaths from Lung Diseases in the UK") %>% 
	  hc_subtitle(text = "Deaths from bronchitis, emphysema and asthma")
	  #hc_add_series_ts(fdeaths, name = "Female") %>%
	  #hc_add_series_ts(mdeaths, name = "Male")
	  })

})
