library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fluid(
  title = "Example of lower, upper, two-tailed tests",
  
  layout_columns(
    col_widths = c(4, 8),
    
    # Left column - Input parameters
    card(
      card_header("Input parameters"),
      card_body(
        numericInput("zscore", "Z-score", value = 1.96, step = 0.01),
        radioButtons("test_type", "Test Type",
                    choices = list("Two-tailed" = "two",
                                  "One-tailed (upper)" = "upper",
                                  "One-tailed (lower)" = "lower"),
                    selected = "two"),
        hr(),
        helpText("This app demonstrates p-values in the normal distribution, for use in Z-testing (see Example 4).")
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
    card_header("Result"),
    card_body(
      verbatimTextOutput("pvalue_result")
    )
  )
)

server <- function(input, output, session) {
  
  # Calculate p-value based on test type
  p_value <- reactive({
    z <- input$zscore
    test <- input$test_type
    
    if (test == "two") {
      p <- 2 * pnorm(-abs(z))
      result <- paste0("Two-tailed p-value: ", round(p, 6))
    } else if (test == "upper") {
      p <- pnorm(z, lower.tail = FALSE)
      result <- paste0("Upper-tailed p-value: ", round(p, 6))
    } else if (test == "lower") {
      p <- pnorm(z, lower.tail = TRUE)
      result <- paste0("Lower-tailed p-value: ", round(p, 6))
    }
    
    return(list(p = p, result = result, test = test))
  })
  
  # Display p-value
  output$pvalue_result <- renderText({
    p_value()$result
  })
  
  # Create density plot
  output$density_plot <- renderPlot({
    z <- input$zscore
    test <- p_value()$test
    
    # Generate x values for normal distribution
    x <- seq(-4, 4, length.out = 1000)
    y <- dnorm(x)
    df <- data.frame(x = x, y = y)
    
    # Base plot
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      labs(x = "Z-score", y = "Density") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
    
    # Add shaded area based on test type, using #3F6BB6 as the color
    if (test == "two") {
      # Two-tailed test: shade both tails
      if (z > 0) {
        p <- p + 
          geom_area(data = subset(df, x >= z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_area(data = subset(df, x <= -z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_vline(xintercept = z, color = "#3F6BB6") +
          geom_vline(xintercept = -z, color = "#3F6BB6")
      } else {
        p <- p + 
          geom_area(data = subset(df, x <= z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_area(data = subset(df, x >= -z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
          geom_vline(xintercept = z, color = "#3F6BB6") +
          geom_vline(xintercept = -z, color = "#3F6BB6")
      }
    } else if (test == "upper") {
      # Upper-tailed test: shade area above z
      p <- p + 
        geom_area(data = subset(df, x >= z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
        geom_vline(xintercept = z, color = "#3F6BB6")
    } else if (test == "lower") {
      # Lower-tailed test: shade area below z
      p <- p + 
        geom_area(data = subset(df, x <= z), aes(x = x, y = y), fill = "#3F6BB6", alpha = 0.5) +
        geom_vline(xintercept = z, color = "#3F6BB6")
    }
    
    return(p)
  })
}

shinyApp(ui, server)
