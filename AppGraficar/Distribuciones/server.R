library(shiny)
library(shinydashboard)
library(ggplot2)


shinyServer(function(input, output) {
    
    observeEvent(input$graf_normal,{
        output$graf_normal <-  renderPlot({
            # curve(dnorm(x, mean = input$media_normal, sd = input$sd_normal), input$min_max_normal[1], input$min_max_normal[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_normal[1], input$min_max_normal[2])), aes(x))+
                stat_function(fun = dnorm, n=101, args = list(mean = input$media_normal, sd = input$sd_normal), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)

        })
    })
    
    
    
    observeEvent(input$graf_gamma, {
        output$graf_gamma <- renderPlot({
            # curve(dgamma(x, shape = input$shape_gamma, rate = input$rate_gamma), input$min_max_gamma[1], input$min_max_gamma[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_gamma[1], input$min_max_gamma[2])), aes(x))+
                stat_function(fun = dgamma, n=101, args = list(shape = input$shape_gamma, rate = input$rate_gamma), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_exp, {
        output$graf_exp <- renderPlot({
            # curve(dexp(x, rate = input$rate_exp), input$min_max_exp[1], input$min_max_exp[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_exp[1], input$min_max_exp[2])), aes(x))+
                stat_function(fun = dexp, n=101, args = list(rate = input$rate_exp), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_cauchy, {
        output$graf_cauchy <- renderPlot({
            # curve(dcauchy(x, location = input$location_cauchy, scale = input$scale_cauchy), input$min_max_cauchy[1], input$min_max_cauchy[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_cauchy[1], input$min_max_cauchy[2])), aes(x))+
                stat_function(fun = dcauchy, n=101, args = list(location = input$location_cauchy, scale = input$scale_cauchy), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_t, {
        output$graf_t <- renderPlot({
            # curve(dt(x, df = input$df_t, ncp = input$ncp_t), input$min_max_t[1], input$min_max_t[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_t[1], input$min_max_t[2])), aes(x))+
                stat_function(fun = dt, n=101, args = list(df = input$df_t, ncp = input$ncp_t), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_chisq, {
        output$graf_chisq <- renderPlot({
            # curve(dchisq(x, df = input$df_chisq, ncp = input$ncp_chisq), input$min_max_chisq[1], input$min_max_chisq[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_chisq[1], input$min_max_chisq[2])), aes(x))+
                stat_function(fun = dchisq, n=101, args = list(df = input$df_chisq, ncp = input$ncp_chisq), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_f, {
        output$graf_f <- renderPlot({
            # curve(df(x, df1 = input$df1_f, df2 = input$df2_f, ncp = input$ncp_f), input$min_max_f[1], input$min_max_f[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_f[1], input$min_max_f[2])), aes(x))+
                stat_function(fun = df, n=101, args = list(df1 = input$df1_f, df2 = input$df2_f, ncp = input$ncp_f), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_u, {
        output$graf_u <- renderPlot({
            # curve(dunif(x, min = input$min_max_u[1], max = input$min_max_u[2]), input$min_max_u[1], input$min_max_u[2],xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_u[1], input$min_max_u[2])), aes(x))+
                stat_function(fun = dunif, n=101, args = list(min = input$min_max_u[1], max = input$min_max_u[2]), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_logis, {
        output$graf_logis <- renderPlot({
            # curve(dlogis(x, location = input$location_logis, scale = input$scale_logis), input$min_max_logis[1], input$min_max_logis[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_logis[1], input$min_max_logis[2])), aes(x))+
                stat_function(fun = dlogis, n=101, args = list(location = input$location_logis, scale = input$scale_logis), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_lognormal, {
        output$graf_lognormal <- renderPlot({
            # curve(dlnorm(x, meanlog = input$meanlog, sdlog = input$sdlog), input$min_max_lognormal[1], input$min_max_lognormal[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_lognormal[1], input$min_max_lognormal[2])), aes(x))+
                stat_function(fun = dlnorm, n=101, args = list(meanlog = input$meanlog, sdlog = input$sdlog), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_beta, {
        output$graf_beta <- renderPlot({
            # curve(dbeta(x, shape1 = input$shape1_beta, shape2 = input$shape2_beta, ncp = input$ncp_beta), input$min_max_beta[1], input$min_max_beta[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_beta[1], input$min_max_beta[2])), aes(x))+
                stat_function(fun = dbeta, n=101, args = list(shape1 = input$shape1_beta, shape2 = input$shape2_beta, ncp = input$ncp_beta), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_pois, {
        output$graf_pois <- renderPlot({
            # curve(dpois(x, lambda = input$lambda_pois), input$min_max_pois[1], input$min_max_pois[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_pois[1], input$min_max_pois[2])), aes(x))+
                stat_function(fun = dpois, n=101, args = list(lambda = input$lambda_pois), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    
    
    observeEvent(input$graf_binomial, {
        output$graf_binomial <- renderPlot({
            # curve(dbinom(x, size = input$size_binomial, prob = input$prob_binomial), input$min_max_binomial[1], input$min_max_binomial[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_binomial[1], input$min_max_binomial[2])), aes(x))+
                stat_function(fun = dbinom, n=101, args = list(size = input$size_binomial, prob = input$prob_binomial), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)
        })
    })
    
    

})
