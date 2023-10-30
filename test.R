library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)

# data
data("iris")

generate_plot <- function(df, x, species){
  color <- ifelse(species == "setosa", "red", ifelse(species == "versicolor", "green", "blue"))
  
  ggplot(
    df, aes(x = {{x}})) +
    geom_histogram(binwidth = 1, fill = color) +
    labs(x = paste("Total count =", nrow(df))) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+ 
    theme(legend.position = "none")
}

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
    
    # just for legend
    p0 <- ggplot(iris, aes(x = Sepal.Length, fill = Species))+
      geom_histogram(binwidth = 1)+
      scale_fill_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")) +
      theme_minimal()
    
    # actual plot
    p1 <- generate_plot(sep_length_filter(), Sepal.Length, input$select_species)
    p2 <- generate_plot(sep_width_filter(), Sepal.Width, input$select_species)
    p3 <- generate_plot(pet_length_filter(), Petal.Length, input$select_species)
    p4 <- generate_plot(pet_width_filter(), Petal.Width, input$select_species)
    
    
    # legend
    legend <- get_legend(p0)
    
    # arrange
    # prow <- plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2)
    # plot_grid(prow, legend)

    plot_grid(NULL, legend, p1, p2, p3, p4, ncol = 2)
    
    
    
  })
}

shinyApp(ui, server)
