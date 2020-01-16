library(shiny)
library(shinydashboard)


shinyServer(function(input, output) {
    
    observeEvent(input$graf_normal,{
        output$graf_normal <-  renderPlot({
            curve(dnorm(x, mean = input$media_normal, sd = input$sd_normal), input$min_max_normal[1], input$min_max_normal[2], xlab = "", ylab = "")

        })
    })
    
    
    
    observeEvent(input$graf_gamma, {
        output$graf_gamma <- renderPlot({
            curve(dgamma(x, shape = input$shape_gamma, rate = input$rate_gamma), input$min_max_gamma[1], input$min_max_gamma[2], xlab = "", ylab = "")
        })
    })
    
    

})
