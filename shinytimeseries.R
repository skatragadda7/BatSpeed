library(tidyverse)
library(shiny)

baseball_series <- read.csv("fangraphs-leaderboards.csv") %>%
  select(Season, Name, wOBA, wRC.) %>%
  rename(`wRC+` = wRC.)

# Precompute global ranges for each metric (over all players/seasons)
woba_range <- range(baseball_series$wOBA, na.rm = TRUE)
wrc_range  <- range(baseball_series$`wRC+`, na.rm = TRUE)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel( 
      selectInput(inputId = "Player1", 
                  label = "Player 1?",
                  choices = unique(baseball_series$Name),
                  selected = "Aaron Judge"
      ),
      selectInput(inputId = "Player2",
                  label = "Player 2?",
                  choices = c("None", unique(baseball_series$Name))),
      radioButtons(inputId = "Response",
                   label = "Which Performance Metric?",
                   choices = c("wOBA", "wRC+"),
                   selected = "wOBA"),
      checkboxInput(inputId = "ShowPoints",
                    label   = "Show points on lines?",
                    value   = TRUE)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    # which players to show?
    players <- if (input$Player2 == "None") {
      input$Player1
    } else {
      c(input$Player1, input$Player2)
    }
    
    # filter seasons + players
    plot_data <- baseball_series %>%
      filter(
        Season >= 2017,
        Season <= 2025,
        Name %in% players
      )
    
    metric <- input$Response  # "wOBA" or "wRC+"
    
    # base plot
    p <- ggplot(plot_data,
                aes(x = Season,
                    y = .data[[metric]],
                    color = Name,
                    group = Name)) +
      geom_line(linewidth = 1) +
      { if (input$ShowPoints) geom_point(size = 2) else NULL } +
      labs(
        x = "Season",
        y = metric,
        color = "Player"
      ) +
      theme_classic() +
      # fixed x-axis for all views
      scale_x_continuous(breaks = 2017:2025,
                         limits = c(2017, 2025))
    
    # add fixed y-axis depending on metric
    if (metric == "wOBA") {
      p <- p + scale_y_continuous(limits = woba_range)
    } else {
      p <- p + scale_y_continuous(limits = wrc_range)
    }
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
