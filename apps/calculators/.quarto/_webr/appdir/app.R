library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

ui <- page_sidebar(
  title = "Confidence interval calculator",
  
  # Add custom CSS to hide numeric input arrows
  tags$head(
    tags$style(HTML("
      input[type=number]::-webkit-inner-spin-button,
      input[type=number]::-webkit-outer-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      input[type=number] {
        -moz-appearance: textfield;
      }
    "))
  ),
  
  sidebar = sidebar(
    sliderInput("alpha",
                "significance level (α):",
                min = 0.01,
                max = 0.20,
                value = 0.05,
                step = 0.01),
    
    hr(),
    
    numericInput("x_bar",
                 "sample mean (x̄)",
                 value = 75,
                 min = 50,
                 max = 100,
                 step = 0.1),
    
    numericInput("sigma",
                 "sample standard deviation (s):",
                 value = 10,
                 min = 1,
                 max = 20,
                 step = 0.1),
    
    numericInput("n",
                 "sample size (n):",
                 value = 100,
                 min = 10,
                 max = 500,
                 step = 1)
  ),
  
  card(
    card_header("Normal distribution and confidence interval"),
    plotlyOutput("normal_plot", height = "600px")
  ),
  
  card(
    card_header("Confidence interval summary"),
    div(
      style = "font-size: 16px; padding: 10px;",
      uiOutput("ci_summary")
    ),
    height = "250px"
  )
)

server <- function(input, output, session) {
  
  # Reactive calculations
  confidence_level <- reactive({
    (1 - input$alpha) * 100
  })
  
  alpha_half <- reactive({
    input$alpha / 2
  })
  
  z_critical <- reactive({
    qnorm(1 - alpha_half())
  })
  
  standard_error <- reactive({
    input$sigma / sqrt(input$n)
  })
  
  margin_of_error <- reactive({
    z_critical() * standard_error()
  })
  
  ci_lower <- reactive({
    input$x_bar - margin_of_error()
  })
  
  ci_upper <- reactive({
    input$x_bar + margin_of_error()
  })
  
  # Main plot
  output$normal_plot <- renderPlotly({
    
    # Create data for the normal distribution
    x_seq <- seq(-4, 4, length.out = 1000)
    y_seq <- dnorm(x_seq)
    
    # Create the base plot
    p <- ggplot(data.frame(x = x_seq, y = y_seq), aes(x = x, y = y)) +
      geom_line(linewidth = 1.2, color = "#3f68b6") +
      
      # Shade the rejection regions
      geom_area(data = data.frame(x = x_seq[x_seq <= -z_critical()], 
                                  y = y_seq[x_seq <= -z_critical()]),
                aes(x = x, y = y), fill = "#db4315", alpha = 0.3) +
      geom_area(data = data.frame(x = x_seq[x_seq >= z_critical()], 
                                  y = y_seq[x_seq >= z_critical()]),
                aes(x = x, y = y), fill = "#db4315", alpha = 0.3) +
      
      # Shade the confidence region
      geom_area(data = data.frame(x = x_seq[x_seq >= -z_critical() & x_seq <= z_critical()], 
                                  y = y_seq[x_seq >= -z_critical() & x_seq <= z_critical()]),
                aes(x = x, y = y), fill = "#3f68b6", alpha = 0.2) +
      
      # Add vertical lines for critical values
      geom_vline(xintercept = c(-z_critical(), z_critical()), 
                 linetype = "dashed", color = "#db4315", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
      
      # Add labels
      annotate("text", x = -z_critical(), y = 0.1, 
               label = paste("-Z =", round(-z_critical(), 3)), 
               hjust = 1.1, color = "#db4315", size = 4) +
      annotate("text", x = z_critical(), y = 0.1, 
               label = paste("Z =", round(z_critical(), 3)), 
               hjust = -0.1, color = "#db4315", size = 4) +
      annotate("text", x = 0, y = 0.45, 
               label = paste(confidence_level(), "% confidence"), 
               hjust = 0.5, color = "#3f68b6", size = 5, fontface = "bold") +
      annotate("text", x = -3, y = 0.05, 
               label = paste("α/2 =", round(alpha_half(), 4)), 
               hjust = 0.5, color = "#db4315", size = 4) +
      annotate("text", x = 3, y = 0.05, 
               label = paste("α/2 =", round(alpha_half(), 4)), 
               hjust = 0.5, color = "#db4315", size = 4) +
      
      labs(
        title = "Standard normal distribution for confidence interval",
        x = "Z-score",
        y = "Probability density",
        subtitle = paste("Confidence interval for μ: [", round(ci_lower(), 2), "g,", round(ci_upper(), 2), "g]")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      xlim(-4, 4) +
      ylim(0, 0.5)
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
  # Confidence interval summary
  output$ci_summary <- renderUI({
    tagList(
      p(strong("Confidence level:"), paste0(sprintf("%.1f", confidence_level()), "%")),
      p(strong("Confidence interval:"), 
        paste0("[", sprintf("%.3f", ci_lower()), " g , ", sprintf("%.3f", ci_upper()), " g]")),
      p(strong("Margin of error (ME):"), paste0(sprintf("%.3f", margin_of_error()), " g")),
      p(strong("Standard error (SE):"), paste0(sprintf("%.3f", standard_error()), " g"))
    )
  })
}

shinyApp(ui = ui, server = server)
