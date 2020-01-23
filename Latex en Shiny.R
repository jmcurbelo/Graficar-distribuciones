ui <- fluidPage(
    withMathJax(),
    tabPanel(
        title = "Diagnostics", 
        h4(textOutput("diagTitle")),
        uiOutput("formula")
    )
)

server <- function(input, output, session){
    output$formula <- renderUI({
        # my_calculated_value <- 5
        withMathJax("Función de densidad normal: $$f(x)=\\frac{a}{b}\\pi$$")
    })
}

shinyApp(ui, server)
