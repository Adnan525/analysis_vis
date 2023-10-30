library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  uiOutput("titlePanelUI"),
  sidebarLayout(
    sidebarPanel(
      textInput("title_text", "Enter New Title:")
    ),
    mainPanel(
    )
  )
)

server <- function(input, output) {
  output$titlePanelUI <- renderUI({
    if(nchar(input$title_text) > 0){
      titlePanel(input$title_text)
    }
    else{
      titlePanel("Topic 2 Q8")
    }
  })
}

shinyApp(ui, server)
