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
                      menuItem("uniforme", tabName = "uniforme"),
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
                              h3("exponencial")),
                      tabItem(tabName = "cauchy",
                              h3("Cauchy")),
                      tabItem(tabName = "t",
                              h3("t student")),
                      tabItem(tabName = "chisq",
                              h3("Chi cuadrada")),
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