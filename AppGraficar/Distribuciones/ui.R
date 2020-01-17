library(shinydashboard)
library(shiny)


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
                              h3("F-Fisher")),
                      tabItem(tabName = "uniforme",
                              h3("uniforme")),
                      tabItem(tabName = "logistica",
                              h3("logistica")),
                      tabItem(tabName = "lognormal",
                              h3("Lognormal")),
                      tabItem(tabName = "beta",
                              h3("Beta")),
                      tabItem(tabName = "poisson",
                              h3("Poisson")),
                      tabItem(tabName = "binomial",
                              h3("Binomial"))
                      
                  )
              )
              )