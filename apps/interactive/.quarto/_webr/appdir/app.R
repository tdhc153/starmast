library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(bg = "#ffffff", fg = "#000000"),
  titlePanel("T-test selection flowchart"),
  card(
    card_header("Interactive flowchart: Which t-test should I use?"),
    card_body(uiOutput("flowchart"))
  )
)

server <- function(input, output, session) {
  current_step <- reactiveVal(1)
  
  observeEvent(input$reset, current_step(1))
  observeEvent(input$step1_one, current_step(2))
  observeEvent(input$step1_two, current_step(3))
  observeEvent(input$step2_yes, current_step(4))
  observeEvent(input$step2_no, current_step(5))
  observeEvent(input$step3_paired, current_step(6))
  observeEvent(input$step3_independent, current_step(7))
  observeEvent(input$step7_yes, current_step(8))
  observeEvent(input$step7_no, current_step(9))
  
  observeEvent(input$back, {
    step <- current_step()
    new_step <- switch(as.character(step),
      "2" = 1, "3" = 1,
      "4" = 2, "5" = 2,
      "6" = 3, "7" = 3,
      "8" = 7, "9" = 7,
      1
    )
    current_step(new_step)
  })
  
  output$flowchart <- renderUI({
    step <- current_step()
    
    make_button <- function(id, label, color) {
      actionButton(id, label, style = paste0(
        "background-color:", color, "; color:white; border:none; padding:12px 24px; ",
        "margin:6px; font-size:16px; border-radius:6px; font-weight:600;"
      ))
    }
    
    make_panel <- function(bg, text_color, content, buttons) {
      tagList(
        div(style = "text-align:center; padding:20px;",
          div(style = paste0("background-color:", bg, "; color:", text_color, 
                            "; padding:20px; border-radius:10px; margin:20px auto; max-width:500px;"),
            content
          ),
          div(style = "margin-top:30px;", buttons)
        )
      )
    }
    
    result_panel <- function(title, items) {
      make_panel("#db4315", "white",
        tagList(
          h2(title),
          hr(style = "border-color:white;"),
          h4("Use this test when:"),
          tags$ul(style = "text-align:left; display:inline-block;", lapply(items, tags$li))
        ),
        tagList(make_button("back", "← Back", "#666666"), make_button("reset", "Start over", "#666666"))
      )
    }
    
    nav_buttons <- if(step > 1) {
      tagList(br(), br(), make_button("back", "← Back", "#666666"), make_button("reset", "Start over", "#666666"))
    } else {
      NULL
    }
    
    switch(as.character(step),
      "1" = make_panel("#ffffff", "#000000",
        tagList(h3("How many samples do you have?"),
                p("Are you comparing one sample to a known value or two samples?")),
        tagList(make_button("step1_one", "One sample", "#3f68b6"),
                make_button("step1_two", "Two samples", "#3f68b6"))
      ),
      "2" = make_panel("#ffffff", "#000000",
        tagList(h3("One-tailed or two-tailed?"),
                p("Do you have a directional hypothesis or are you testing for any difference?")),
        tagList(make_button("step2_yes", "One-tailed", "#3f68b6"),
                make_button("step2_no", "Two-tailed", "#3f68b6"),
                nav_buttons)
      ),
      "3" = make_panel("#ffffff", "#000000",
        tagList(h3("Are the samples paired or independent?"),
                p("Paired: same subjects measured twice (before/after)"),
                p("Independent: different subjects in each group")),
        tagList(make_button("step3_paired", "Paired samples", "#3f68b6"),
                make_button("step3_independent", "Independent samples", "#3f68b6"),
                nav_buttons)
      ),
      "4" = result_panel("✓ One-sample Student's t-test (one-tailed)",
        c("Comparing one sample mean to a known population value",
          "You have a directional hypothesis",
          "Example: Is the average height greater than 170 cm?")
      ),
      "5" = result_panel("✓ One-sample Student's t-test (two-tailed)",
        c("Comparing one sample mean to a known population value",
          "Testing for any difference",
          "Example: Is the average height different from 170 cm?")
      ),
      "6" = make_panel("#db4315", "white",
        tagList(
          h2("✓ Paired-samples Student's t-test"),
          hr(style = "border-color:white;"),
          tags$ul(style = "text-align:left; display:inline-block;",
            tags$li("Same subjects measured twice"),
            tags$li("Matched pairs of observations"),
            tags$li("Example: blood pressure before and after treatment")
          ),
          p(style = "margin-top:15px; font-style:italic;",
            "Can be one-tailed or two-tailed depending on the hypothesis.")
        ),
        tagList(make_button("back", "← Back", "#666666"), make_button("reset", "Start over", "#666666"))
      ),
      "7" = make_panel("white", "black",
        tagList(h3("Do the groups have equal variances?"),
                p("This can be tested using Levene's test or an F-test.")),
        tagList(make_button("step7_yes", "Yes (equal variances)", "#3f68b6"),
                make_button("step7_no", "No (unequal variances)", "#3f68b6"),
                br(), br(),
                make_button("back", "← Back", "#666666"),
                make_button("reset", "Start over", "#666666"))
      ),
      "8" = result_panel("✓ Independent samples Student's t-test",
        c("Two independent groups",
          "Equal variances assumed",
          "Example: comparing scores between two similar classes")
      ),
      "9" = result_panel("✓ Welch's t-test",
        c("Two independent groups",
          "Unequal variances assumed",
          "Example: comparing groups with different variability")
      )
    )
  })
}

shinyApp(ui, server)
