library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fluid(
  title = "T-test calculator",
  
  layout_columns(
    col_widths = c(4, 8),
    
    # Left column - Input parameters
    card(
      card_header("Input parameters"),
      card_body(
        numericInput("tscore", "T-statistic", value = 2.0, step = 0.01),
        numericInput("df", "Degrees of freedom", value = 20, min = 1, step = 1),
        radioButtons("test_type", "Test type",
                    choices = list("Two-tailed" = "two",
                                  "One-tailed (upper)" = "upper",
                                  "One-tailed (lower)" = "lower"),
                    selected = "two"),
        hr(),
        helpText("This app calculates p-values for t-tests based on the t-distribution with specified degrees of freedom.")
      )
    ),
    
    # Right column - Graphical representation
    card(
      card_header("Graphical representation"),
      card_body(
        plotOutput("density_plot", height = "300px")
      )
    )
  ),
  
  # Results at the bottom
  card(
    card_header("T-test results"),
    card_body(
      verbatimTextOutput("pvalue_result")
    )
  )
)

server <- function(input, output, session) {
  
  # Calculate p-value based on test type
  p_value <- reactive({
    t <- input$tscore
    df <- input$df
    test <- input$test_type
    
    if (test == "two") {
      p <- 2 * pt(-abs(t), df = df)
      result <- paste0("Two-tailed p-value: ", round(p, 4))
    } else if (test == "upper") {
      p <- pt(t, df = df, lower.tail = FALSE)
      result <- paste0("Upper-tailed p-value: ", round(p, 4))
    } else if (test == "lower") {
      p <- pt(t, df = df, lower.tail = TRUE)
      result <- paste0("Lower-tailed p-value: ", round(p, 4))
    }
    
    return(list(p = p, result = result, test = test))
  })
  
  # Display p-value
  output$pvalue_result <- renderText({
    p_value()$result
  })
  
  # Create density plot
  output$density_plot <- renderPlot({
    t <- input$tscore
    df <- input$df
    test <- p_value()$test
    
    # Generate x values for t-distribution
    x <- seq(-4, 4, length.out = 1000)
    y <- dt(x, df = df)
    df_data <- data.frame(x = x, y = y)
    
    # Base plot
    p <- ggplot(df_data, aes(x = x, y = y)) +
      geom_line() +
      labs(x = "T-statistic", y = "Density", 
           title = paste("T-distribution (df =", df, ")")) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
    
    # Add shaded area based on test type, using #3F6BB6 as the color
    if (test == "two") {
      # Two-tailed test: shade both tails
      if (t > 0) {
        p <- p + 
          geom_area(data = subset(df_data, x >= t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_area(data = subset(df_data, x <= -t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_vline(xintercept = t, color = "#3F6BB6") +
          geom_vline(xintercept = -t, color = "#3F6BB6")
      } else {
        p <- p + 
          geom_area(data = subset(df_data, x <= t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_area(data = subset(df_data, x >= -t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_vline(xintercept = t, color = "#3F6BB6") +
          geom_vline(xintercept = -t, color = "#3F6BB6")
      }
    } else if (test == "upper") {
      # Upper-tailed test: shade area above t
      p <- p + 
        geom_area(data = subset(df_data, x >= t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
        geom_vline(xintercept = t, color = "#3F6BB6")
    } else if (test == "lower") {
      # Lower-tailed test: shade area below t
      p <- p + 
        geom_area(data = subset(df_data, x <= t), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
        geom_vline(xintercept = t, color = "#3F6BB6")
    }
    
    return(p)
  })
}

shinyApp(ui, server)
