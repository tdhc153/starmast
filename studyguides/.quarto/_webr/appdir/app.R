library(shiny)
library(bslib)
library(DT)

# Generate truth table
generate_truth_table <- function(vars) {
  expand.grid(rep(list(c(TRUE, FALSE)), length(vars))) |>
    setNames(vars)
}

# Convert logic symbols to R syntax (UPDATED for brackets)
convert_logic <- function(expr) {
  expr <- gsub("¬", "!", expr)
  expr <- gsub("∧", "&", expr)
  expr <- gsub("∨", "|", expr)

  # implication (supports brackets now)
  expr <- gsub("(.*)→(.*)", "(!\\1 | \\2)", expr)

  # biconditional (supports brackets now)
  expr <- gsub("(.*)↔(.*)", "(\\1 == \\2)", expr)

  expr
}

# Safe evaluation
evaluate_expr <- function(expr, data) {
  expr <- convert_logic(expr)

  tryCatch({
    eval(parse(text = expr), envir = data)
  }, error = function(e) {
    rep(NA, nrow(data))
  })
}

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      .T { color:#3f68b6; font-weight:bold; font-size:18px; }
      .F { color:#db4315; font-weight:bold; font-size:18px; }

      table {
        border-collapse: collapse;
        margin-top: 20px;
        font-family: sans-serif;
        min-width: 300px;
      }

      th {
        background-color: #f5f5f5;
        padding: 10px 16px;
        text-align: center;
        font-weight: 600;
        border-bottom: 2px solid #ddd;
      }

      td {
        padding: 10px 16px;
        text-align: center;
        border-bottom: 1px solid #eee;
      }

      tr:nth-child(even) {
        background-color: #fafafa;
      }

      tr:hover {
        background-color: #f1f7ff;
      }

      button {
        margin: 2px;
      }
    "))
  ),

  titlePanel("Truth Table Builder"),

  sidebarLayout(
    sidebarPanel(
      textInput("vars", "Variables:", "p q"),
      textInput("expr", "Expression:", ""),

      h4("Variables"),
      uiOutput("var_buttons"),

      h4("Connectives"),
      div(
        actionButton("not", "¬"),
        actionButton("and", "∧"),
        actionButton("or", "∨"),
        actionButton("imp", "→"),
        actionButton("iff", "↔")
      ),

      h4("Brackets"),
      div(
        actionButton("lpar", "("),
        actionButton("rpar", ")")
      ),

      br(),
      actionButton("clear", "Clear")
    ),

    mainPanel(
      htmlOutput("table")
    )
  )
)

server <- function(input, output, session) {

  # Reactive variable list
  vars <- reactive({
    v <- strsplit(input$vars, "\\s+")[[1]]
    v[v != ""]
  })

  # Variable buttons
  output$var_buttons <- renderUI({
    lapply(vars(), function(v) {
      actionButton(paste0("var_", v), v)
    })
  })

  # Handle variable button clicks
  observe({
    for (v in vars()) {
      local({
        var <- v
        observeEvent(input[[paste0("var_", var)]], {
          updateTextInput(session, "expr",
                          value = paste0(input$expr, var))
        }, ignoreInit = TRUE)
      })
    }
  })

  # Connectives
  observeEvent(input$not, {
    updateTextInput(session, "expr", value = paste0(input$expr, "¬"))
  })

  observeEvent(input$and, {
    updateTextInput(session, "expr", value = paste0(input$expr, " ∧ "))
  })

  observeEvent(input$or, {
    updateTextInput(session, "expr", value = paste0(input$expr, " ∨ "))
  })

  observeEvent(input$imp, {
    updateTextInput(session, "expr", value = paste0(input$expr, " → "))
  })

  observeEvent(input$iff, {
    updateTextInput(session, "expr", value = paste0(input$expr, " ↔ "))
  })

  # Brackets (NEW)
  observeEvent(input$lpar, {
    updateTextInput(session, "expr", value = paste0(input$expr, "("))
  })

  observeEvent(input$rpar, {
    updateTextInput(session, "expr", value = paste0(input$expr, ")"))
  })

  observeEvent(input$clear, {
    updateTextInput(session, "expr", value = "")
  })

  # Render table
  output$table <- renderUI({

    req(vars())

    df <- generate_truth_table(vars())

    if (nchar(input$expr) > 0) {
      df$result <- evaluate_expr(input$expr, df)
    }

    format_val <- function(x) {
      if (isTRUE(x)) return('<span class="T">T</span>')
      if (identical(x, FALSE)) return('<span class="F">F</span>')
      return("")
    }

    html <- "<table>"

    # Header
    html <- paste0(html, "<thead><tr>",
                   paste0("<th>", c(vars(), "Result"), "</th>", collapse = ""),
                   "</tr></thead>")

    # Body
    html <- paste0(html, "<tbody>")

    for (i in 1:nrow(df)) {
      html <- paste0(html, "<tr>")

      for (v in vars()) {
        html <- paste0(html, "<td>", format_val(df[[v]][i]), "</td>")
      }

      if ("result" %in% colnames(df)) {
        html <- paste0(html, "<td>", format_val(df$result[i]), "</td>")
      }

      html <- paste0(html, "</tr>")
    }

    html <- paste0(html, "</tbody></table>")

    HTML(html)
  })
}

shinyApp(ui, server)
