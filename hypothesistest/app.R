#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  titlePanel("☂️ Should You Bring an Umbrella?"),
  br(),
  uiOutput("question_ui"),
  br(),
  fluidRow(
    column(3, actionButton("yes_btn", "✅ Yes", width = "100%")),
    column(3, actionButton("no_btn", "❌ No", width = "100%")),
    column(3, actionButton("back_btn", "⬅ Back", width = "100%")),
    column(3, actionButton("reset_btn", "🔄 Start Over", width = "100%"))
  )
)

server <- function(input, output, session) {
  
  # Define the decision tree structure
  decision_tree <- list(
    q1 = list(
      question = "Is it cloudy?",
      yes = "q2",
      no = "out1"
    ),
    q2 = list(
      question = "Is rain in the forecast?",
      yes = "out2",
      no = "q3"
    ),
    q3 = list(
      question = "Will you be outside for more than 30 minutes?",
      yes = "out2",
      no = "out3"
    ),
    out1 = "☀️ No umbrella needed. Enjoy your day!",
    out2 = "🌧️ Yes, bring an umbrella!",
    out3 = "🌤️ No need to bring an umbrella unless you're worried."
  )
  
  # Use a character vector instead of a list for history
  rv <- reactiveValues(
    current_node = "q1",
    history = character()
  )
  
  observeEvent(input$yes_btn, {
    node <- rv$current_node
    if (is.list(decision_tree[[node]])) {
      rv$history <- c(rv$history, node)
      rv$current_node <- decision_tree[[node]]$yes
    }
  })
  
  observeEvent(input$no_btn, {
    node <- rv$current_node
    if (is.list(decision_tree[[node]])) {
      rv$history <- c(rv$history, node)
      rv$current_node <- decision_tree[[node]]$no
    }
  })
  
  observeEvent(input$back_btn, {
    if (length(rv$history) > 0) {
      rv$current_node <- tail(rv$history, 1)
      rv$history <- head(rv$history, -1)
    }
  })
  
  observeEvent(input$reset_btn, {
    rv$current_node <- "q1"
    rv$history <- character()
  })
  
  output$question_ui <- renderUI({
    node <- rv$current_node
    
    if (!is.list(decision_tree[[node]])) {
      return(
        tagList(
          h3("🌈 Outcome:"),
          p(decision_tree[[node]])
        )
      )
    } else {
      return(
        tagList(
          h3(decision_tree[[node]]$question)
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
