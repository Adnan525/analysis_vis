library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)

# data
data("iris")

ui <- fluidPage(
  #uiOutput("titlePanelUI"),
  titlePanel("Topic 2 Q8"),
  sidebarLayout(
    sidebarPanel(
      # dynamic title
      # textInput("title_text", "Enter New Title:"),
      
      # select species input
      selectInput("select_species", "Select Species", c(unique(iris$Species))),
      
      # slider input
      sliderInput("sepal_length", "Sepal Length", min(iris$Sepal.Length), max(iris$Sepal.Length), value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))),
      sliderInput("sepal_width", "Sepal Width", min(iris$Sepal.Width), max(iris$Sepal.Width), value = c(min(iris$Sepal.Width), max(iris$Sepal.Width))),
      sliderInput("petal_length", "Petal Length", min(iris$Petal.Length), max(iris$Petal.Length), value = c(min(iris$Petal.Length), max(iris$Petal.Length))),
      sliderInput("petal_width", "Petal Width", min(iris$Petal.Width), max(iris$Petal.Width), value = c(min(iris$Petal.Width), max(iris$Petal.Width))),
      
      # dynamic overall plot title
      textInput("overall_title", "Enter Overall Title", "Iris Histogram", placeholder = NULL)
    ),
    mainPanel(
      uiOutput("overall_title"),
      plotOutput("iris_plot"))
  )
)

server <- function(input, output) {
  # dynamic title
  # output$titlePanelUI <- renderUI({
  #   if(nchar(input$title_text) > 0){
  #     titlePanel(input$title_text)
  #   }
  #   else{
  #     titlePanel("Topic 2 Q8")
  #   }
  # })
  # =================================
  
  # dynamic overall title
  output$overall_title <-renderUI(titlePanel(input$overall_title))
  
  # slider filtering
  sep_length_filter <- reactive({
    iris %>% 
      filter(Species == input$select_species) %>% 
      filter(Sepal.Length >= input$sepal_length[1] & Sepal.Length <= input$sepal_length[2])
  })
  sep_width_filter <- reactive({
    iris %>% 
      filter(Species == input$select_species) %>% 
      filter(Sepal.Width >= input$sepal_width[1] & Sepal.Width <= input$sepal_width[2])
  })
  pet_length_filter <- reactive({
    iris %>% 
      filter(Species == input$select_species) %>% 
      filter(Petal.Length >= input$petal_length[1] & Petal.Length <= input$petal_length[2])
  })
  pet_width_filter <- reactive({
    iris %>% 
      filter(Species == input$select_species) %>% 
      filter(Petal.Width >= input$petal_width[1] & Petal.Width <= input$petal_width[2])
  })
  
  # histogram plots
  output$iris_plot <- renderPlot({
    p1 <- ggplot(
      sep_length_filter(), aes(x = Sepal.Length)) +
      geom_histogram(fill = "cyan", color="black", binwidth = 1) +
      labs(y = paste("count =", nrow(sep_length_filter()))) +
      labs(x = "Sepal Length") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    p2 <- ggplot(
      sep_width_filter(), aes(x = Sepal.Width)) +
      geom_histogram(fill = "magenta", color="black", binwidth = 1) +
      labs(y = paste("count =", nrow(sep_width_filter()))) +
      labs(x = "Sepal Width") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    p3 <- ggplot(
      pet_length_filter(), aes(x = Petal.Length)) +
      geom_histogram(fill = "yellow", color="black", binwidth = 1) +
      labs(y = paste("count =", nrow(pet_length_filter()))) +
      labs(x = "Petal Length") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    p4 <- ggplot(
      pet_width_filter(), aes(x = Petal.Width)) +
      geom_histogram(fill = "black", color="black", binwidth = 1) +
      labs(y = paste("count =", nrow(pet_width_filter()))) +
      labs(x = "Petal Width") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    
    # arrange
    plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2)
    
  })
}

shinyApp(ui, server)
