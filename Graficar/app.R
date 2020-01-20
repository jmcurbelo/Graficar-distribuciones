

library(shiny)
library(shinydashboard)
library(ggplot2)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Graficar Distribuciones"),
                    dashboardSidebar(
                        
                        sidebarMenu(
                            menuItem("Normal", tabName = "normal"),
                            menuItem("Gamma", tabName = "gamma"),
                            menuItem("Exponencial", tabName = "exponencial"),
                            menuItem("Cauchy", tabName = "cauchy"),
                            menuItem("T Student", tabName = "t"),
                            menuItem("Chi Cuadrada", tabName = "chisq"),
                            menuItem("F-Fisher", tabName = "f"),
                            menuItem("Uniforme", tabName = "uniforme"),
                            menuItem("Logística", tabName = "logistica"),
                            menuItem("Log-Normal", tabName = "lognormal"),
                            menuItem("Beta", tabName = "beta"),
                            menuItem("Poisson", tabName = "poisson"),
                            menuItem("Binomial", tabName = "binomial")
                        )
                        
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "normal",
                                    fluidRow(
                                        h3("Seleccione los hiperparámetros"),
                                        column(6,
                                               sliderInput(inputId = "media_normal", label = "Media", min = -50, max = 50, value = 0)
                                               
                                        ),
                                        column(6,
                                               sliderInput(inputId = "sd_normal", label = "SD", min = 0, max = 20, value = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_normal", label = "Rango para graficar", min = -50, max = 50, value = c(-5,5)),
                                               
                                        ),
                                        
                                        column(4,
                                               actionButton(inputId = "graf_normal", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_normal")
                                    )
                                    
                                    
                            ),
                            tabItem(tabName = "gamma",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "shape_gamma", label = "Shape", min = 0, max = 50, value = 1)
                                               
                                        ),
                                        column(6,
                                               sliderInput(inputId = "rate_gamma", label = "Rate", min = 0, max = 50, value = 1)
                                               
                                        )
                                        
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_gamma", label = "Rango para graficar", min = 0, max = 50, value = c(-5,5))
                                               
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_gamma", label = "Graficar")
                                        )
                                        
                                    ),
                                    fluidRow(
                                        plotOutput("graf_gamma")
                                    )
                                    
                            ),
                            tabItem(tabName = "exponencial",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "rate_exp", label = "Scale", min = 0, max = 50, value = 1)
                                               
                                        ),
                                        column(6,
                                               
                                               sliderInput(inputId = "min_max_exp", label = "Rango para graficar", min = 0, max = 30, value = c(0,5), step = 0.25)
                                               
                                        )
                                    ),
                                    fluidRow(
                                        actionButton(inputId = "graf_exp", label = "Graficar")
                                    ),
                                    fluidRow(
                                        plotOutput("graf_exp")
                                    )
                            ),
                            tabItem(tabName = "cauchy",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "location_cauchy", label = "Location", min = -10, max = 10, value = 0)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "scale_cauchy", label = "Scale", min = 0.1, max = 10, value = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_cauchy", label = "Rango para graficar", min = -30, max = 30, value = c(-5,5), step = 0.5)
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_cauchy", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_cauchy")
                                    )
                            ),
                            tabItem(tabName = "t",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "df_t", label = "DF", min = 1, max = 50, value = 30)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "ncp_t", label = "NCP", min = -20, max = 20, value = 0)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_t", label = "Rango para graficar", min = -50, max = 50, value = c(-5,5))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_t", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_t")
                                    )
                                    
                                    
                            ),
                            tabItem(tabName = "chisq",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "df_chisq", label = "DF", min = 1, max = 50, value = 2)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "ncp_chisq", label = "NCP", min = 0, max = 50, value = 0)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_chisq", label = "Rango para graficar", min = 0, max = 50, value = c(-5,5))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_chisq", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_chisq")
                                    )
                                    
                            ),
                            tabItem(tabName = "f",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "df1_f", label = "DF1", min = 1, max = 50, value = 1)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "df2_f", label = "DF2", min = 1, max = 50, value = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "ncp_f", label = "NCP", min = 0, max = 50, value = 0)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "min_max_f", label = "Rango para graficar", min = 0, max = 50, value = c(0,10))
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               actionButton(inputId = "graf_f", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_f")
                                    )
                            ),
                            tabItem(tabName = "uniforme",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_u", label = "Valores mínimos y máximos", min = -50, max = 50, value = c(0,1))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_u", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_u")
                                    )
                            ),
                            tabItem(tabName = "logistica",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "location_logis", label = "Location", min = -10, max = 10, value = 0)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "scale_logis", label = "Scale", min = 1, max = 20, value = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_logis", label = "Rango para graficar", min = -50, max = 50, value = c(-5,5))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_logis", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_logis")
                                    )
                            ),
                            tabItem(tabName = "lognormal",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "meanlog", label = "meanlog", min = 0.0001, max = 50, value = 1)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "sdlog", label = "sdlog", min = 0.0001, max = 50, value = 1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_lognormal", label = "Rango para graficar", min = 0, max = 100, value = c(0,5))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_lognormal", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_lognormal")
                                    )
                            ),
                            tabItem(tabName = "beta",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "shape1_beta", label = "Shape 1", min = 0.0001, max = 20, value = 0.1)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "shape2_beta", label = "Shape 2", min = 0.0001, max = 20, value = 0.1)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "ncp_beta", label = "NCP", min = 0, 5, value = 0)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "min_max_beta", label = "Rango para graficar", min = 0, max = 1, value = c(0.1, 0.9), step = 0.01)
                                        )
                                    ),
                                    fluidRow(
                                        actionButton(inputId = "graf_beta", label = "Graficar")
                                        
                                    ),
                                    fluidRow(
                                        plotOutput("graf_beta")
                                    )
                            ),
                            tabItem(tabName = "poisson",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "lambda_pois", label = "Lambda", min = 0, max = 20, value = 1)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "min_max_pois", label = "Rango para graficar", min = -5, max = 100, value = c(-1,1))
                                        )
                                    ),
                                    fluidRow(
                                        actionButton(inputId = "graf_pois", label = "Graficar")
                                    ),
                                    fluidRow(
                                        plotOutput("graf_pois")
                                    )
                            ),
                            tabItem(tabName = "binomial",
                                    h3("Seleccione los hiperparámetros"),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "size_binomial", label = "size", min = 1, max = 50, value = 5)
                                        ),
                                        column(6,
                                               sliderInput(inputId = "prob_binomial", label = "Prob", min = 0, max = 1, value = 0.5)
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               sliderInput(inputId = "min_max_binomial", label = "Rango para graficar",min = -50, max = 50, value = c(-5,5))
                                        ),
                                        column(6,
                                               actionButton(inputId = "graf_binomial", label = "Graficar")
                                        )
                                    ),
                                    fluidRow(
                                        plotOutput("graf_binomial")
                                    )
                            )
                            
                        )
                    )
)




server <- shinyServer(function(input, output) {
    
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


# Run the application 
shinyApp(ui = ui, server = server)
