library(shiny)
library(bslib)

# Hard-coded hex color for the button (blue color requested)
BUTTON_COLOR <- "#3F6BB6"

ui <- page_fluid(
  title = "Coin Flipper",
  
  card(
    card_header("Coin Flipper"),
    card_body(
      # Use a row layout for landscape orientation
      layout_columns(
        col_widths = c(4, 4, 4), # Equal width columns
        
        # Left column: Description and stats
        card(
          card_body(
            div(
              style = "display: flex; flex-direction: column; height: 100%; justify-content: center;",
              p("Click the button to flip a coin.", style = "font-size: 18px;"),
              br(),
              div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                h5("Statistics:"),
                div(
                  style = "display: flex; justify-content: space-between;",
                  div("Total flips:"),
                  textOutput("totalFlips", inline = TRUE)
                ),
                div(
                  style = "display: flex; justify-content: space-between;",
                  div("Heads:"),
                  textOutput("headsCount", inline = TRUE)
                ),
                div(
                  style = "display: flex; justify-content: space-between;",
                  div("Tails:"),
                  textOutput("tailsCount", inline = TRUE)
                )
              )
            )
          )
        ),
        
        # Middle column: Coin display
        card(
          card_body(
            div(
              style = "height: 100%; display: flex; align-items: center; justify-content: center;",
              div(
                id = "coinDisplay",
                style = "font-size: 40px; line-height: 1; width: 150px; height: 150px; 
                       margin: 0 auto; border: 2px solid #ccc; border-radius: 50%;
                       display: flex; align-items: center; justify-content: center;
                       background-color: #f0f0f0;",
                textOutput("coinResult")
              )
            )
          )
        ),
        
        # Right column: Button
        card(
          card_body(
            div(
              style = "display: flex; flex-direction: column; height: 100%; 
                     align-items: center; justify-content: center; gap: 20px;",
              actionButton("flipButton", "Flip Coin", class = "btn-lg", 
                          style = paste0("background-color: ", BUTTON_COLOR, "; color: white;")),
              actionButton("resetButton", "Reset Stats", class = "btn-sm")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Create reactive values to store the current state
  flips <- reactiveValues(
    current = sample(c("HEADS", "TAILS"), 1),
    total = 0,
    heads = 0,
    tails = 0,
    flipping = FALSE,
    timer = NULL  # Track the timer
  )
  
  # Initialize the display
  output$coinResult <- renderText({
    if(flips$flipping) {
      return("")  # Show blank state during animation
    } else {
      return(flips$current)
    }
  })
  
  # Update the statistics displays
  output$totalFlips <- renderText({
    flips$total
  })
  
  output$headsCount <- renderText({
    paste0(flips$heads, " (", round(ifelse(flips$total > 0, flips$heads/flips$total*100, 0), 1), "%)")
  })
  
  output$tailsCount <- renderText({
    paste0(flips$tails, " (", round(ifelse(flips$total > 0, flips$tails/flips$total*100, 0), 1), "%)")
  })
  
  # Handle the coin flip
  observeEvent(input$flipButton, {
    # Set the flipping state to true to show blank state
    flips$flipping <- TRUE
    
    # Create a separate reactive timer that will complete the flip after delay
    # This fixes the delay issue in the previous version
    flips$timer <- reactiveTimer(500)
    
    # This observer will fire when the timer triggers
    observeEvent(flips$timer(), {
      # Determine the result
      result <- sample(c("HEADS", "TAILS"), 1)
      
      # Update the state
      flips$current <- result
      flips$total <- flips$total + 1
      
      # Update the appropriate counter
      if(result == "HEADS") {
        flips$heads <- flips$heads + 1
      } else {
        flips$tails <- flips$tails + 1
      }
      
      # End the flipping state
      flips$flipping <- FALSE
    }, once = TRUE) # This ensures it only fires once per button click
  })
  
  # Reset button to clear statistics
  observeEvent(input$resetButton, {
    flips$total <- 0
    flips$heads <- 0
    flips$tails <- 0
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
