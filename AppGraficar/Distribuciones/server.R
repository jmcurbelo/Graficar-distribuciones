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
    
    
    
    observeEvent(input$graf_exp, {
        output$graf_exp <- renderPlot({
            curve(dexp(x, rate = input$rate_exp), input$min_max_exp[1], input$min_max_exp[2], xlab = "", ylab = "")
        })
    })
    
    
    
    observeEvent(input$graf_cauchy, {
        output$graf_cauchy <- renderPlot({
            curve(dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy), input$min_max_cauchy[1], input$min_max_cauchy[2], xlab = "", ylab = "")
        })
    })
    
    
    
    observeEvent(input$graf_t, {
        output$graf_t <- renderPlot({
            curve(dt(x, df = input$df_t, ncp = input$ncp_t), input$min_max_t[1], input$min_max_t[2], xlab = "", ylab = "")
        })
    })
    
    
    
    observeEvent(input$graf_chisq, {
        output$graf_chisq <- renderPlot({
            curve(dchisq(x, df = input$df_chisq, ncp = input$ncp_chisq), input$min_max_chisq[1], input$min_max_chisq[2], xlab = "", ylab = "")
        })
    })
    
    

})
