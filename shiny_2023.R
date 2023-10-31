library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

df <- read.csv("dataset_shinyapp.csv")
df_long <- pivot_longer(df, cols = -Experiment_ID, names_to = "Category", values_to = "Value")

ui <- fluidPage(
  titlePanel("Topic 2 Q4"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("categories", "Categories", choices = unique(df_long$Category), selected = unique(df_long$Category)),
      # slider number samples
      sliderInput("experiment", "Experiment ID",
                  min(df_long$Experiment_ID), max(df_long$Experiment_ID),
                  value = c(min(df_long$Experiment_ID), max(df_long$Experiment_ID))),
      
      # slider input
      sliderInput("high_hand", "Range HIGH_HAND", min(df$HIGH_HAND), max(df$HIGH_HAND), value = c(min(df$HIGH_HAND), max(df$HIGH_HAND))),
      sliderInput("high_leg", "Range HIGH_LEG", min(df$HIGH_LEG), max(df$HIGH_LEG), value = c(min(df$HIGH_LEG), max(df$HIGH_LEG))),
      sliderInput("low_hand", "Range LOW_HAND", min(df$LOW_HAND), max(df$LOW_HAND), value = c(min(df$LOW_HAND), max(df$LOW_HAND))),
      sliderInput("low_leg", "Range LOW_LEG", min(df$LOW_LEG), max(df$LOW_LEG), value = c(min(df$LOW_LEG), max(df$LOW_LEG))),
    ),
    mainPanel(
      plotOutput("boxplot"),
      uiOutput("stat"),
      uiOutput("summary")
    ),
  )
)
server <- function(input, output) {
  # category checkbox
  filtered_data <- reactive({
    df_long%>%
      filter(Category %in% input$categories)
  })
  # experiment id filter
  experiment_filter <- reactive({
    filtered_data() %>% 
      filter(Experiment_ID >= input$experiment[1] & Experiment_ID <= input$experiment[2])
  })
  #slider range filter
  high_hand_filter <- reactive({
    experiment_filter() %>%
      filter(Category == "HIGH_HAND") %>% 
      filter(Value >= input$high_hand[1] & Value <= input$high_hand[2])
  })
  high_leg_filter <- reactive({
    experiment_filter() %>%
      filter(Category == "HIGH_LEG") %>% 
      filter(Value >= input$high_leg[1] & Value <= input$high_leg[2])
  })
  low_hand_filter <- reactive({
    experiment_filter() %>%
      filter(Category == "LOW_HAND") %>% 
      filter(Value >= input$low_hand[1] & Value <= input$low_hand[2])
  })
  low_leg_filter <- reactive({
    experiment_filter() %>%
      filter(Category == "LOW_LEG") %>% 
      filter(Value >= input$low_leg[1] & Value <= input$low_leg[2])
  })
  all_filtered_data <- reactive({
    bind_rows(
      high_hand_filter(),
      high_leg_filter(),
      low_hand_filter(),
      low_leg_filter()
    )
  })
  # histogram plots
  output$boxplot <- renderPlot({
    ggplot(all_filtered_data(), aes(x = Category, y = Value, fill = Category)) +
      geom_boxplot() +
      labs(
        title = "Plot for Final Assessment",
        x = "Category",
        y = "Value"
      ) +
      theme_classic()
  })
  
  category_summary <- reactive({
    df %>% 
      filter(Experiment_ID >= input$experiment[1] & Experiment_ID <= input$experiment[2])
  })
  
  # stat
  output$stat <- renderText(paste("Statistics", "<br>", collapse = "\n"))
  # generate output
  column_summary <- reactive({
    if (length(input$categories) == 0) {
      return(NULL)
    }
    summaries <- lapply(input$categories, function(column_name) {
      data <- category_summary()
      mean_val <- mean(data[[column_name]])
      sd_val <- sd(data[[column_name]])
      paste("<br>",
            column_name,
            "<br>",
            "Mean:", round(mean_val, 2), 
            "<br>",
            "SD:", round(sd_val, 2),
            "<br>")
    })
    
    paste(summaries)
  })
  # Render the summary as a string
  output$summary <- renderText({
    if (is.null(column_summary())) {
      return("No columns selected.")
    } else {
      paste(column_summary())
    }
  })
}

shinyApp(ui, server)