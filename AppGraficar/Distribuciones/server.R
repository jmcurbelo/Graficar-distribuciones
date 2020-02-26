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
                scale_y_continuous(breaks = NULL)+
                geom_vline(aes(xintercept = input$media_normal, color="media"), size=1)+
                scale_color_manual(name="", values = c(media="black"))+
                theme(legend.position = c(0.95, 0.95))

        })
    })
    
    
    
    observeEvent(input$graf_gamma, {
        output$graf_gamma <- renderPlot({
            # curve(dgamma(x, shape = input$shape_gamma, rate = input$rate_gamma), input$min_max_gamma[1], input$min_max_gamma[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_gamma[1], input$min_max_gamma[2])), aes(x))+
                stat_function(fun = dgamma, n=101, args = list(shape = input$shape_gamma, rate = input$rate_gamma), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)+
                geom_vline(aes(xintercept = input$shape_gamma*(1/input$rate_gamma), color = "media"), size = 1)+
                scale_color_manual(name="",values = c(media="black"))+
                theme(legend.position = c(0.95,0.95))
                
                
        })
    })
    
    
    
    observeEvent(input$graf_exp, {
        output$graf_exp <- renderPlot({
            # curve(dexp(x, rate = input$rate_exp), input$min_max_exp[1], input$min_max_exp[2], xlab = "", ylab = "")
            ggplot(data = data.frame(x = c(input$min_max_exp[1], input$min_max_exp[2])), aes(x))+
                stat_function(fun = dexp, n=101, args = list(rate = input$rate_exp), colour = "blue", lwd = 1)+
                ylab("")+
                scale_y_continuous(breaks = NULL)+
                geom_vline(aes(xintercept=1/input$rate_exp, color="media"), size=1)+
                scale_color_manual(name="", values=c(media="black"))+
                theme(legend.position=c(0.95,0.95))
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
    
    
    output$formula_normal <- renderUI({
        withMathJax("Función de densidad normal: $$f(x)=\\frac{1}{\\sqrt{2\\pi}\\sigma}e^{-\\frac{(x-\\mu)^{2}}{2\\sigma^{2}}}\\,\\,\\,para\\,\\,\\,-\\infty<\\mu<\\infty;\\,\\sigma>0$$")
    })
    
    
    
    output$formula_gamma <- renderUI({
        withMathJax("Función de densidad gamma: $$f(x)=\\frac{\\lambda^{r}}{\\Gamma(r)}x^{r-1}e^{-\\lambda x}\\,\\,\\,para\\,\\,\\,\\lambda>0;\\,r>0;\\,x>0$$")
    })
    
    
    
    output$formula_exponencial <- renderUI({
        withMathJax("Función de densidad exponencial: $$f(x)=\\lambda e^{-\\lambda x}\\,\\,\\,para\\,\\,\\,\\lambda>0;\\,x>0$$")
    })
    
    
    
    output$formula_cauchy <- renderUI({
        withMathJax("Función de densidad cuachy: $$f(x)=\\frac{1}{\\pi B\\left\\{ 1+\\left[\\frac{x-\\alpha}{\\beta}\\right]^{2}\\right\\} }\\,\\,\\,para\\,\\,\\,-\\infty<\\alpha<\\infty;\\,\\beta>0$$")
    })
    
    
    
    output$formula_t <- renderUI({
        withMathJax("Función de densidad t: $$f(x)=\\frac{\\Gamma\\left[\\frac{k+1}{2}\\right]}{\\Gamma\\left(\\frac{k}{2}\\right)}\\frac{1}{\\sqrt{k\\pi}}\\frac{1}{\\left(1+\\frac{x^{2}}{k}\\right)^{\\frac{k+1}{2}}}\\,\\,\\,para\\,\\,\\,k>0$$")
    })
    
    
    
    output$formula_chisq <- renderUI({
        withMathJax("Función de densidad Chi Cuadrada: $$f(x)=\\frac{1}{\\Gamma\\left(\\frac{k}{2}\\right)}\\left(\\frac{1}{2}\\right)^{\\frac{k}{2}}x^{\\frac{k}{2}-1}e^{-\\frac{1}{2}x}\\,\\,\\,para\\,\\,\\,x>0;\\,k=1,2,...$$")
    })
    
    
    
    output$formula_f <- renderUI({
        withMathJax("Función de densidad F de Fisher: $$f(x)=\\frac{\\Gamma\\left[\\frac{m+n}{2}\\right]}{\\Gamma\\left(\\frac{m}{2}\\right)\\Gamma\\left(\\frac{n}{2}\\right)}\\left(\\frac{m}{n}\\right)^{\\frac{m}{2}}\\,\\,\\,para\\,\\,\\,m,n=1,2,3,...$$")
    })
    
    
    
    output$formula_uniforme <- renderUI({
        withMathJax("Función de densidad uniforme: $$f(x)=\\frac{1}{b-a}\\,\\,\\,para\\,\\,\\,-\\infty<a<b<\\infty;\\,a\\leq x\\leq b$$")
    })
    
    
    
    output$formula_logistica <- renderUI({
        withMathJax("Función de densidad logistica: $$F(x)=\\left[1+e^{-\\frac{(x-\\alpha)}{\\beta}}\\right]^{-1}\\,\\,\\,para\\,\\,\\,-\\infty<\\alpha<\\infty;\\,\\beta>0$$")
    })
    
    
    
    output$formula_lognormal <- renderUI({
        withMathJax("Función de densidad lognormal: $$f(x)=\\frac{1}{x\\sqrt{2\\pi}\\sigma}e^{-\\frac{(log_{e}x-\\mu)^{2}}{2\\sigma^{2}}\\,\\,\\,para\\,\\,\\,-\\infty<\\mu<\\infty;\\,\\sigma>0;\\,x>0}$$")
    })
    
    
    
    output$formula_beta <- renderUI({
        withMathJax("Función de densidad beta: $$f(x)=\\frac{1}{B(a,b)}x^{a-1}(1-x)^{b-1}\\,\\,\\,para\\,\\,\\,a>0;\\,b>0;\\,0<x<1$$")
    })
    
    
    
    output$formula_poisson <- renderUI({
        withMathJax("Función de densidad Poisson: $$f(x)=\\frac{e^{-\\lambda}\\lambda^{x}}{x!}\\,\\,\\,para\\,\\,\\,\\lambda>0;\\,x=0,1,2,...$$")
    })
    
    
    
    output$formula_binomial <- renderUI({
        withMathJax("Función de densidad binomial: $$f(x)=\\binom{n}{x}p^{x}q^{n-x}\\,\\,\\,para\\,\\,\\,0\\leq p\\leq1;\\,n=1,2,3...;\\,q=1-p;\\,x=0,1,...,n$$")
    })
    
    

})
