library(shiny)
library(bslib)

# Define the decision tree structure (removed don't know options)
decision_tree <- list(
  question = "Are you comparing means in your hypothesis test μ?",
  yes = list(
    question = "Do you have one sample or two? Click yes for one, no for two.",
    yes = list(
      question = "Brilliant, one sample. Do you know the population standard deviation σ?",
      yes = list(
        result = "You should use a Z-test."
      ),
      no = list(
        question = "Ok, so you don't know σ. Is your sample size n > 30?",
          yes = list(
            result = "You should use a Z-test."
          ),
          no = list(
            result = "You should use a t-test."
          )
        )
      ),
    no = list(
      question = "Ok, so you have two samples. Are they independent or are they paired? Click yes for independent, no for paired.",
      yes = list(
        question = "Ok, your two samples are independent. Do you know the population standard deviation σ for both samples?",
      yes = list(
        result = "You should use a Z-test."
      ),
      no = list(
        question = "Ok, so you don't know σ for both samples. Is your sample size n > 30?",
          yes = list(
            result = "You should use a two-sample Z-test."
          ),
          no = list(
            result = "You should use a two-sample t-test."
          )
        )
      ),
      no = list(
        result = "You should use a paired t-test."
      )
    )
  ),
  no = list(
    question = "Are you testing for variance?",
    yes = list(
      result = "You should use an F-test for variance."
      ),
    no = list(
      question = "Are you testing for goodness of fit?",
      yes = list(
        result = "You should use a chi-squared test for goodness of fit."
      ),
      no = list(
        question = "Are you testing for independence?",
          yes = list(
            result = "You should use a chi-squared test for independence."
          ),
          no = list(
            result = "Unfortunately this is not covered in the interactive figure; please start again."
          )
        )
      )
    )
  )

# Define button color hex codes
button_colors <- list(
  yes = "#3F6BB6",        # Green
  no = "#DB4315",         # Red
  back = "#9E9E9E",       # Gray
  start_over = "#FFCB00", # syellow
  result_bg = "#C0D6FF",  # Light green background for results
  result_border = "#3F6BB6", # Slightly darker green border for results
  result_text = "#000000"  # Dark green text for results
)


ui <- page_fluid(
  # Add some custom CSS to style buttons with hex colors
  tags$head(
    tags$style(HTML(
      paste0(
        ".yes-btn { background-color: ", button_colors$yes, "; border-color: ", button_colors$yes, "; color: white; }",
        ".no-btn { background-color: ", button_colors$no, "; border-color: ", button_colors$no, "; color: white; }",
        ".back-btn { background-color: ", button_colors$back, "; border-color: ", button_colors$back, "; color: white; }",
        ".start-over-btn { background-color: ", button_colors$start_over, "; border-color: ", button_colors$start_over, "; color: white; }",
        ".result-box { background-color: ", button_colors$result_bg, "; border-color: ", button_colors$result_border, "; color: ", button_colors$result_text, "; border: 1px solid; padding: 15px; border-radius: 5px; }"
      )
    ))
  ),
  card(
    card_header(
      h2("Hypothesis testing interactive flowchart", class = "text-center")
    ),
    card_body(
      uiOutput("question_ui"),
      uiOutput("result_ui"),
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton("back_btn", "Back", icon = icon("arrow-left"), class = "back-btn"),
        actionButton("start_over_btn", "Start Over", icon = icon("refresh"), class = "start-over-btn")
      )
    )
  )
)

server <- function(input, output, session) {
  # Keep track of the path through the decision tree
  path_history <- reactiveVal(list())
  
  # Current node in the decision tree
  current_node <- reactiveVal(decision_tree)
  
  # Update the UI based on the current node
  output$question_ui <- renderUI({
    node <- current_node()
    if (is.null(node) || !hasName(node, "question")) {
      return(NULL)
    }
    
    div(
      h4(node$question, class = "mb-4"),
      div(
        class = "d-flex justify-content-center gap-2",
        actionButton("yes_btn", "Yes", class = "yes-btn"),
        actionButton("no_btn", "No", class = "no-btn")
      )
    )
  })
  
  output$result_ui <- renderUI({
    node <- current_node()
    if (is.null(node) || !hasName(node, "result")) {
      return(NULL)
    }
    
    div(
      class = "result-box", # Using custom class instead of alert-success
      h4("Recommendation:", class = "mb-3"),
      p(node$result)
    )
  })
  
  # Navigate to the next node based on user choice
  observeEvent(input$yes_btn, {
    node <- current_node()
    if (!is.null(node$yes)) {
      path_history(c(path_history(), list(node)))
      current_node(node$yes)
    }
  })
  
  observeEvent(input$no_btn, {
    node <- current_node()
    if (!is.null(node$no)) {
      path_history(c(path_history(), list(node)))
      current_node(node$no)
    }
  })
  
  # Go back to the previous question
  observeEvent(input$back_btn, {
    history <- path_history()
    if (length(history) > 0) {
      # Set current node to the last node in history
      current_node(history[[length(history)]])
      # Remove the last node from history
      path_history(history[-length(history)])
    }
  })
  
  # Start over - reset to the root of the decision tree
  observeEvent(input$start_over_btn, {
    current_node(decision_tree)
    path_history(list())
  })
}

shinyApp(ui = ui, server = server)
