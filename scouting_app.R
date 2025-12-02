library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(readr)

# R -e "shiny::runApp('scouting_app.R')"
# Load the real dataset
data_path <- "RK_wrc_models_complete/onlyCompleteData.csv"

if (file.exists(data_path)) {
    raw_data <- read_csv(data_path, show_col_types = FALSE)
} else {
    stop(paste("Data file not found at:", data_path))
}

# Filter for 2025 Cohort and Select Metrics
mlb_data <- raw_data %>%
    filter(year == 2025) %>%
    select(
        Player = player.name,
        wRC_plus = WRC.,
        Swing_Speed = avg_swing_speed,
        Exit_Velo = exit_velocity_avg,
        Barrel_Pct = barrel_batted_rate,
        Hard_Hit_Pct = hard_hit_percent,
        Whiff_Pct = whiff_percent
    ) %>%
    drop_na()

# Build the regression model using 2024 data (training set)
train_data <- raw_data %>%
    filter(year == 2024) %>%
    select(
        WRC., isolated_power, exit_velocity_avg, hard_hit_percent,
        flyballs_percent, sweet_spot_percent, avg_swing_speed
    ) %>%
    drop_na()

LinReg.mod14 <- lm(
    WRC. ~ isolated_power
        + exit_velocity_avg + I(exit_velocity_avg^2)
        + hard_hit_percent
        + flyballs_percent
        + sweet_spot_percent + I(sweet_spot_percent^2)
        + isolated_power * exit_velocity_avg
        + isolated_power * avg_swing_speed
        + hard_hit_percent * avg_swing_speed,
    data = train_data
)

# 2. UI Definition --------------------------------------------------------

ui <- page_sidebar(
    theme = bs_theme(
        bootswatch = "cyborg",
        primary = "#007bff",
        base_font = font_google("Inter"),
        heading_font = font_google("Oswald")
    ),
    title = "2025 MLB Scout Dashboard (Advanced Metrics)",
    sidebar = sidebar(
        title = "Novel Player Projections",
        width = 350,
        helpText("Enter statistics for your player."),
        h5("Core Metrics", class = "text-primary"),
        numericInput("input_wrc", "wRC+ (Your Projection)", value = 115, min = 0, max = 250, step = 1),
        numericInput("input_swing", "Avg Swing Speed (mph)", value = 72.5, min = 60, max = 85, step = 0.1),
        numericInput("input_ev", "Avg Exit Velocity (mph)", value = 89.5, min = 80, max = 100, step = 0.1),
        numericInput("input_barrel", "Barrel %", value = 8.5, min = 0, max = 30, step = 0.1),
        numericInput("input_hardhit", "Hard Hit %", value = 40.0, min = 0, max = 70, step = 0.1),
        numericInput("input_whiff", "Whiff % (Lower is Better)", value = 24.5, min = 0, max = 50, step = 0.1),
        hr(),
        h5("Model Inputs", class = "text-info"),
        helpText("Additional metrics for wRC+ prediction model:"),
        numericInput("input_iso", "Isolated Power (ISO)", value = 0.180, min = 0, max = 0.500, step = 0.001),
        numericInput("input_flyball", "Flyball %", value = 30.0, min = 0, max = 60, step = 0.1),
        numericInput("input_sweetspot", "Sweet Spot %", value = 35.0, min = 0, max = 50, step = 0.1),
        actionButton("update_btn", "Update Analysis", class = "btn-primary w-100")
    ),
    layout_columns(
        col_widths = c(12, 12, 6, 6),

        # Row 1: Model Prediction
        card(
            card_header("wRC+ Model Prediction vs Your Projection"),
            uiOutput("model_comparison")
        ),

        # Row 2: Rankings
        card(
            card_header("Percentile Rankings (2025 Cohort)"),
            uiOutput("ranking_cards")
        ),

        # Row 3: Visuals
        card(
            card_header("Metric Distribution"),
            layout_sidebar(
                sidebar = sidebar(
                    selectInput("dist_metric", "Select Metric",
                        choices = c(
                            "wRC_plus", "Swing_Speed", "Exit_Velo",
                            "Barrel_Pct", "Hard_Hit_Pct", "Whiff_Pct"
                        ),
                        selected = "wRC_plus"
                    ),
                    width = "200px",
                    open = "closed"
                ),
                plotOutput("dist_plot", height = "400px")
            )
        ),
        card(
            card_header("Holistic Profile (Radar Chart)"),
            plotOutput("radar_plot", height = "400px")
        )
    )
)

# 3. Server Logic ---------------------------------------------------------

server <- function(input, output, session) {
    # Reactive Novel Player Data
    novel_player <- eventReactive(input$update_btn,
        {
            tibble(
                wRC_plus = input$input_wrc,
                Swing_Speed = input$input_swing,
                Exit_Velo = input$input_ev,
                Barrel_Pct = input$input_barrel,
                Hard_Hit_Pct = input$input_hardhit,
                Whiff_Pct = input$input_whiff,
                ISO = input$input_iso,
                Flyball_Pct = input$input_flyball,
                Sweet_Spot_Pct = input$input_sweetspot
            )
        },
        ignoreNULL = FALSE
    )

    # Model Prediction
    model_prediction <- reactive({
        player <- novel_player()

        # Create data frame for prediction
        pred_data <- data.frame(
            isolated_power = player$ISO,
            exit_velocity_avg = player$Exit_Velo,
            hard_hit_percent = player$Hard_Hit_Pct,
            flyballs_percent = player$Flyball_Pct,
            sweet_spot_percent = player$Sweet_Spot_Pct,
            avg_swing_speed = player$Swing_Speed
        )

        # Predict
        predicted_wrc <- predict(LinReg.mod14, newdata = pred_data)
        return(round(predicted_wrc, 1))
    })

    # Output: Model Comparison
    output$model_comparison <- renderUI({
        player_input <- novel_player()$wRC_plus
        model_pred <- model_prediction()
        difference <- player_input - model_pred

        # Determine if projection is realistic
        interpretation <- if (abs(difference) <= 10) {
            list(color = "success", msg = "Projection aligns with model")
        } else if (difference > 10) {
            list(color = "warning", msg = "Projection is optimistic")
        } else {
            list(color = "info", msg = "Projection is conservative")
        }

        layout_column_wrap(
            width = 1 / 3,
            value_box(
                title = "Your Projection",
                value = player_input,
                showcase = bsicons::bs_icon("person-raised-hand"),
                theme = "primary",
                p("Scout's wRC+ estimate", class = "fs-6")
            ),
            value_box(
                title = "Model Prediction",
                value = model_pred,
                showcase = bsicons::bs_icon("calculator"),
                theme = "secondary",
                p("Based on swing metrics", class = "fs-6")
            ),
            value_box(
                title = "Difference",
                value = ifelse(difference > 0, paste0("+", round(difference, 2)), round(difference, 2)),
                showcase = bsicons::bs_icon("graph-up-arrow"),
                theme = interpretation$color,
                p(interpretation$msg, class = "fs-6")
            )
        )
    })

    # Ranking Engine
    rankings <- reactive({
        cohort <- mlb_data
        player <- novel_player()

        metrics <- c("wRC_plus", "Swing_Speed", "Exit_Velo", "Barrel_Pct", "Hard_Hit_Pct", "Whiff_Pct")

        sapply(metrics, function(m) {
            fn <- ecdf(cohort[[m]])
            p <- fn(player[[m]])

            # Invert ranking for Whiff_Pct (Lower is better)
            if (m == "Whiff_Pct") {
                p <- 1 - p
            }
            return(p)
        })
    })


    # Output: Ranking Cards
    output$ranking_cards <- renderUI({
        ranks <- rankings()
        player <- novel_player()

        # Create horizontal layout with fixed widths
        div(
            style = "display: flex; flex-direction: row; gap: 10px; overflow-x: auto;",
            lapply(names(ranks), function(m) {
                pct <- ranks[[m]]
                val <- player[[m]]

                # Color coding
                color <- if (pct >= 0.9) "success" else if (pct >= 0.5) "primary" else "danger"
                text_rank <- paste0("Top ", round((1 - pct) * 100), "%")
                if (pct >= 0.99) text_rank <- "Top 1%"

                # Icon selection
                icon_name <- switch(m,
                    "wRC_plus" = "graph-up-arrow",
                    "Swing_Speed" = "speedometer",
                    "Exit_Velo" = "lightning-charge",
                    "Whiff_Pct" = "wind",
                    "bar-chart"
                )

                div(
                    style = "flex: 1; min-width: 150px;",
                    value_box(
                        title = m,
                        value = val,
                        showcase = bsicons::bs_icon(icon_name),
                        theme = color,
                        p(text_rank, class = "fs-6")
                    )
                )
            })
        )
    })


    # Output: Distribution Plot
    output$dist_plot <- renderPlot({
        cohort <- mlb_data
        player <- novel_player()
        metric <- input$dist_metric

        player_val <- player[[metric]]

        # Determine if lower is better for label placement or color logic (optional)
        is_inverse <- metric == "Whiff_Pct"

        p <- ggplot(cohort, aes(x = .data[[metric]])) +
            geom_density(fill = "#007bff", alpha = 0.3, color = NA) +
            geom_vline(xintercept = player_val, color = "black", linetype = "dashed", size = 1) +
            annotate("text",
                x = player_val, y = 0, label = " Player",
                vjust = -1, hjust = -0.1, color = "black", angle = 90
            ) +
            theme_minimal(base_size = 14) +
            theme(
                text = element_text(color = "white"),
                axis.text = element_text(color = "gray80"),
                panel.grid.major = element_line(color = "gray30"),
                panel.grid.minor = element_blank()
            ) +
            labs(
                title = paste("Distribution of", metric),
                x = metric, y = "Density"
            )

        if (is_inverse) {
            p <- p + scale_x_reverse() # Optional: flip axis for Whiff% so right is "better"?
            # Actually, standard density plots usually keep x-axis numeric increasing.
            # Let's keep it standard but maybe note "Lower is better" in title?
            p <- p + labs(subtitle = "(Lower is Better)")
        }

        p
    })


    # Output: Radar Chart
    output$radar_plot <- renderPlot({
        cohort <- mlb_data
        player <- novel_player()

        metrics <- c("wRC_plus", "Swing_Speed", "Exit_Velo", "Barrel_Pct", "Hard_Hit_Pct", "Whiff_Pct")

        # Calculate Min/Max for scaling
        # For Whiff_Pct, we want to invert the scale so that a LOWER value maps to 1 (outer edge)
        # and HIGHER value maps to 0 (center).

        radar_data <- data.frame(metric = metrics)

        # Compute scaled values
        radar_data$Player_Score <- sapply(metrics, function(m) {
            val <- player[[m]]
            min_v <- min(cohort[[m]], na.rm = TRUE)
            max_v <- max(cohort[[m]], na.rm = TRUE)

            if (m == "Whiff_Pct") {
                # Invert: (Max - Val) / (Max - Min)
                score <- (max_v - val) / (max_v - min_v)
            } else {
                # Normal: (Val - Min) / (Max - Min)
                score <- (val - min_v) / (max_v - min_v)
            }
            return(pmin(1, pmax(0, score)))
        })

        radar_data$Cohort_Score <- sapply(metrics, function(m) {
            val <- mean(cohort[[m]], na.rm = TRUE)
            min_v <- min(cohort[[m]], na.rm = TRUE)
            max_v <- max(cohort[[m]], na.rm = TRUE)

            if (m == "Whiff_Pct") {
                score <- (max_v - val) / (max_v - min_v)
            } else {
                score <- (val - min_v) / (max_v - min_v)
            }
            return(pmin(1, pmax(0, score)))
        })

        # Reshape
        radar_long <- radar_data %>%
            pivot_longer(cols = c("Player_Score", "Cohort_Score"), names_to = "Group", values_to = "Score") %>%
            mutate(Group = ifelse(Group == "Player_Score", "Player", "Cohort Avg"))

        # Plot
        ggplot(radar_long, aes(x = metric, y = Score, group = Group, color = Group, fill = Group)) +
            geom_polygon(alpha = 0.4, size = 1) +
            geom_point(size = 3) +
            coord_polar() +
            scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = NULL) +
            scale_fill_manual(values = c("Player" = "#0d6efd", "Cohort Avg" = "#6c757d")) +
            scale_color_manual(values = c("Player" = "#0d6efd", "Cohort Avg" = "#6c757d")) +
            theme_minimal(base_size = 14) +
            theme(
                text = element_text(color = "white"),
                axis.text.x = element_text(size = 12, color = "white", face = "bold"),
                axis.text.y = element_blank(),
                panel.grid.major = element_line(color = "gray40", linetype = "dotted"),
                legend.position = "bottom",
                legend.text = element_text(color = "black")
            ) +
            labs(x = NULL, y = NULL)
    })
}

shinyApp(ui = ui, server = server)
