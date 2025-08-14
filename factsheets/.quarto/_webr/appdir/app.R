library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fluid(
  title = "Beta distribution calculator",
  
  layout_columns(
    col_widths = c(4, 8),
    
    # Left column - Inputs
    card(
      card_header("Parameters"),
      card_body(
        numericInput("shape1", "Shape parameter α:", value = 2, min = 0.01, step = 0.1),
        numericInput("shape2", "Shape parameter β:", value = 2, min = 0.01, step = 0.1),
        hr(),
        radioButtons("prob_type", "Probability to calculate:",
                    choices = list("P(X ≤ x)" = "less", 
                                  "P(X ≥ x)" = "greater", 
                                  "P(x ≤ X ≤ y)" = "between"),
                    selected = "less"),
        conditionalPanel(
          condition = "input.prob_type == 'less'",
          sliderInput("x_less", "x value:", min = 0, max = 1, value = 0.5, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.prob_type == 'greater'",
          sliderInput("x_greater", "x value:", min = 0, max = 1, value = 0.5, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.prob_type == 'between'",
          sliderInput("x_lower", "Lower bound (x):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("x_upper", "Upper bound (y):", min = 0, max = 1, value = 0.75, step = 0.01)
        )
      )
    ),
    
    # Right column - Plot
    card(
      card_header("Beta distribution plot"),
      card_body(
        uiOutput("plot_title"),
        plotOutput("distPlot", height = "300px")
      )
    )
  ),
  
  # Bottom row - Results
  card(
    card_header("Results"),
    card_body(
      textOutput("explanation")
    )
  )
)

server <- function(input, output, session) {
  
  # Ensure that x_upper is always greater than or equal to x_lower
  observe({
    if (input$x_upper < input$x_lower) {
      updateSliderInput(session, "x_upper", value = input$x_lower)
    }
  })
  
  # Display the plot title with distribution parameters
  output$plot_title <- renderUI({
    title <- sprintf("Beta(α = %.2f, β = %.2f)", input$shape1, input$shape2)
    tags$h4(title, style = "text-align: center; margin-bottom: 15px;")
  })
  
  # Calculate the probability based on user selection
  probability <- reactive({
    if (input$prob_type == "less") {
      prob <- pbeta(input$x_less, shape1 = input$shape1, shape2 = input$shape2)
      explanation <- sprintf("P(X ≤ %.2f) = %.6f or %.4f%%", 
                           input$x_less, prob, prob * 100)
      return(list(prob = prob, explanation = explanation, type = "less", x = input$x_less))
      
    } else if (input$prob_type == "greater") {
      prob <- 1 - pbeta(input$x_greater, shape1 = input$shape1, shape2 = input$shape2)
      explanation <- sprintf("P(X ≥ %.2f) = %.6f or %.4f%%", 
                           input$x_greater, prob, prob * 100)
      return(list(prob = prob, explanation = explanation, type = "greater", x = input$x_greater))
      
    } else if (input$prob_type == "between") {
      if (input$x_lower == input$x_upper) {
        # For continuous distributions, P(X = a) = 0
        prob <- 0
      } else {
        upper_prob <- pbeta(input$x_upper, shape1 = input$shape1, shape2 = input$shape2)
        lower_prob <- pbeta(input$x_lower, shape1 = input$shape1, shape2 = input$shape2)
        prob <- upper_prob - lower_prob
      }
      explanation <- sprintf("P(%.2f ≤ X ≤ %.2f) = %.6f or %.4f%%", 
                           input$x_lower, input$x_upper, prob, prob * 100)
      return(list(prob = prob, explanation = explanation, type = "between", 
                 lower = input$x_lower, upper = input$x_upper))
    }
  })
  
  # Display an explanation of the calculation
  output$explanation <- renderText({
    res <- probability()
    return(res$explanation)
  })
  
  # Generate the beta distribution plot
  output$distPlot <- renderPlot({
    # Get parameters
    shape1 <- input$shape1
    shape2 <- input$shape2
    
    # Create data frame for plotting
    # Beta distribution is defined on the interval [0, 1]
    x_values <- seq(0, 1, length.out = 500)
    density_values <- dbeta(x_values, shape1 = shape1, shape2 = shape2)
    plot_df <- data.frame(x = x_values, density = density_values)
    
    # Create base plot
    p <- ggplot(plot_df, aes(x = x, y = density)) +
      geom_line(size = 1, color = "darkgray") +
      labs(x = "X", y = "probability density function") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      xlim(0, 1) +
      # Adjust y-limit based on maximum density to handle tall peaks
      ylim(0, max(density_values) * 1.05)
    
    # Add shaded area based on selected probability type
    res <- probability()
    
    if (res$type == "less") {
      # Create data for the filled area
      fill_x <- seq(0, res$x, length.out = 200)
      fill_y <- dbeta(fill_x, shape1 = shape1, shape2 = shape2)
      fill_df <- data.frame(x = fill_x, density = fill_y)
      
      p <- p + geom_area(data = fill_df, aes(x = x, y = density), 
                        fill = "#3F6BB6", alpha = 0.6)
      
    } else if (res$type == "greater") {
      # Create data for the filled area
      fill_x <- seq(res$x, 1, length.out = 200)
      fill_y <- dbeta(fill_x, shape1 = shape1, shape2 = shape2)
      fill_df <- data.frame(x = fill_x, density = fill_y)
      
      p <- p + geom_area(data = fill_df, aes(x = x, y = density), 
                        fill = "#3F6BB6", alpha = 0.6)
      
    } else if (res$type == "between") {
      # Create data for the filled area
      fill_x <- seq(res$lower, res$upper, length.out = 200)
      fill_y <- dbeta(fill_x, shape1 = shape1, shape2 = shape2)
      fill_df <- data.frame(x = fill_x, density = fill_y)
      
      p <- p + geom_area(data = fill_df, aes(x = x, y = density), 
                        fill = "#3F6BB6", alpha = 0.6)
    }
    
    return(p)
  })
}

shinyApp(ui = ui, server = server)
