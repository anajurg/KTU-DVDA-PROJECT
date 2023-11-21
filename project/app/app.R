library(shiny)
ui <- fluidPage(
  titlePanel("Hello world 3"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
    )
  )
)
server <- function(input, output) {

}
shinyApp(ui = ui, server = server)
