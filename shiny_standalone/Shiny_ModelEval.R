
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        #formula_info {
            padding: 5px;
            font-size: 0.9em;
            line-height: 1.2;
            max-height: 100px;
            overflow-y: auto;
            margin: 0;
        }
    "))
  ),
  fluidRow(
    column(3,
           selectInput(inputId = "results_list",
                       label = "Select Results List:",
                       choices = NULL),
           
           radioButtons(inputId = "model_type",
                        label = "Model Type:",
                        choices = c("LM" = "lm",
                                    "GAM" = "gam",
                                    "PLS" = "pls",
                                    "PLS - VIP > 0.5" = "vip")),
           
           conditionalPanel(
             condition = "input.model_type == 'lm' || input.model_type == 'gam'",
             br(),
             radioGroupButtons(
               inputId = "model_variation",
               label = "Top 5 Models",
               choices = 1:5,
               selected = 1,
               status = "primary",
               size = "sm",
               justified = TRUE
             )
           ),
           
           conditionalPanel(
             condition = "input.model_type == 'gam'",
             radioButtons("plot_type",
                          "Plot Type:",
                          choices = c("Model Diagnostics" = "appraise",
                                      "Partial Effects" = "draw"))
           ),
           
           conditionalPanel(
             condition = "input.model_type == 'pls' || input.model_type == 'vip'",
             br(),
             radioGroupButtons(
               inputId = "n_components",
               label = "Number of Components:",
               choices = 1:10,
               selected = 1,
               status = "primary",
               size = "sm",
               justified = TRUE
             )
           ),
           
           br(),
           radioGroupButtons(
             inputId = "cv_split",
             label = "Select CV Split:",
             choices = 1:10,
             selected = 1,
             status = "primary",
             size = "sm",
             justified = TRUE
           )
    ),
    column(9,
           plotOutput(outputId = "model_plot"),
           conditionalPanel(
             condition = "input.model_type == 'lm' || input.model_type == 'gam'",
             br(),
             wellPanel(
               style = "padding: 5px; margin-bottom: 5px;",
               h4("Model Formula:", align = "center", style = "margin: 5px;"),
               verbatimTextOutput("formula_info", placeholder = FALSE)
             )
           ),
           br(),
           DT::dataTableOutput(outputId = "results_table")
    ),
    align = "center"
  )
)

server <- function(input, output, session) {
  
  observe({
    available_lists <- ls(envir = .GlobalEnv)[grep("^mods_", ls(envir = .GlobalEnv))]
    updateSelectInput(session, "results_list",
                      choices = available_lists)
  })
  
  selected_model <- reactive({
    req(input$results_list)
    model_results <- get(input$results_list, envir = .GlobalEnv)
    model_list <- model_results[[input$model_type]]
    cv_split <- model_list[[as.numeric(input$cv_split)]]
    
    if(input$model_type %in% c("lm", "gam")) {
      return(cv_split[[as.numeric(input$model_variation)]])
    } else {
      return(cv_split)
    }
  })
  
  selected_results <- reactive({
    req(input$results_list)
    results_name <- gsub("mods_", "results_", input$results_list)
    tryCatch({
      results_df <- get(results_name, envir = .GlobalEnv)
      return(results_df)
    }, error = function(e) {
      return(data.frame(Message = "No results available"))
    })
  })
  
  output$model_plot <- renderPlot({
    req(selected_model())
    model <- selected_model()
    
    tryCatch({
      switch(input$model_type,
             "lm" = gglm(model),
             "gam" = {
               if(input$plot_type == "appraise") {
                 appraise(model)
               } else {
                 draw(model)
               }
             },
             "pls" = plot(model, ncomp = as.numeric(input$n_components)),
             "vip" = plot(model, ncomp = as.numeric(input$n_components)))
    }, error = function(e) {
      validate(need(FALSE, paste("Error in plotting:", e$message)))
    })
  })
  
  output$formula_info <- renderText({
    req(selected_model())
    model <- selected_model()
    
    if(input$model_type %in% c("lm", "gam")) {
      deparse(formula(model))
    }
  })
  
  output$results_table <- DT::renderDataTable({
    selected_results()
  }, 
  options = list(
    ordering = TRUE,
    dom = 't',
    searching = FALSE
  ),
  rownames = FALSE,
  class = 'cell-border stripe hover'
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
