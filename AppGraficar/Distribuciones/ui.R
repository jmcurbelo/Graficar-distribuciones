library(shinydashboard)
library(shiny)
library(ggplot2)


dashboardPage(skin = "blue",
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
                              withMathJax(),
                              tabsetPanel(type = "tabs",
                                          
                                          tabPanel("Función de densidad",
                                                   h3("aqui va funcion de densidad"),
                                                   uiOutput("formula_normal")
                                                   ),
                                          tabPanel("Gráfica",
                                                   fluidRow(
                                                       h3("Seleccione los Parámetros"),
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
                                                   )
                                  
                              )

                              
                              
                             ),
                      tabItem(tabName = "gamma",
                              tabsetPanel(type = "tabs",
                                          tabPanel("Función de densidad",
                                                   h3("funcion de densidad")
                                                   ),
                                          tabPanel("Gráfica",
                                                   
                                                   h3("Seleccione los Parámetros"),
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
                                                   
                                                   )
                                  
                              )

                              
                              ),
                      tabItem(tabName = "exponencial",
                              h3("Seleccione los Parámetros"),
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                      sliderInput(inputId = "location_cauchy", label = "Localización", min = -10, max = 10, value = 0)
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                         sliderInput(inputId = "df_t", label = "GL", min = 1, max = 50, value = 30)
                                         ),
                                  column(6,
                                         sliderInput(inputId = "ncp_t", label = "PNC", min = -20, max = 20, value = 0)
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                         sliderInput(inputId = "df_chisq", label = "GL", min = 1, max = 50, value = 2)
                                         ),
                                  column(6,
                                         sliderInput(inputId = "ncp_chisq", label = "PNC", min = 0, max = 50, value = 0)
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                      sliderInput(inputId = "df1_f", label = "GL1", min = 1, max = 50, value = 1)
                                  ),
                                  column(6,
                                      sliderInput(inputId = "df2_f", label = "GL2", min = 1, max = 50, value = 1)
                                  )
                              ),
                              fluidRow(
                                  column(6,
                                      sliderInput(inputId = "ncp_f", label = "PNC", min = 0, max = 50, value = 0)
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
                              h3("Seleccione los Parámetros"),
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                         sliderInput(inputId = "location_logis", label = "Localización", min = -10, max = 10, value = 0)
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
                              h3("Seleccione los Parámetros"),
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
                              h3("Seleccione los Parámetros"),
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
                                         sliderInput(inputId = "ncp_beta", label = "PNC", min = 0, 5, value = 0)
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
                              h3("Seleccione los Parámetros"),
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
                              h3("Seleccione los Parámetros"),
                              fluidRow(
                                  column(6,
                                         sliderInput(inputId = "size_binomial", label = "Tamaño", min = 1, max = 50, value = 5)
                                         ),
                                  column(6,
                                         sliderInput(inputId = "prob_binomial", label = "Probabilidad", min = 0, max = 1, value = 0.5)
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