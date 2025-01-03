library(knitr)
library(shiny)
# Shiny app to look at model results from CV folds
# Define UI
ui <- fluidPage(
  fluidRow(
    column(3, 
           radioButtons(inputId = "model_list", 
                        label = "Select model list:", 
                        choices = c("MLR", "GAM", "GAM - Partial", "PLS", "PLS - VIP > 1")),
           selectInput(inputId = "model_index",
                       label = "Select model:",
                       choices = as.character(1:10)),
           tableOutput(outputId = "table")
    ),
    column(9, 
           plotOutput(outputId = "model_plot")
    ), 
    align="center"
  )
)

# Define server logic
server <- function(input, output) {
  model_list <- reactive({
    if (input$model_list == "PLS") {
      mods.pls} 
    else if (input$model_list == "PLS - VIP > 1") {
      mods.vip} 
    else if (input$model_list == "MLR") {
      mods.lm} 
    else if (input$model_list == "GAM") {
      mods.GAM} 
    else if (input$model_list == "GAM - Partial") {
      mods.GAM}
  })
  output$model_plot <- renderPlot({
    # Get selected model  
    select_model <- model_list()
    # Plot the model using appropriate plot function
    if (input$model_list == "GAM") {
      appraise(select_model[[as.integer(input$model_index)]])} 
    else if (input$model_list == "GAM - Partial") {
      draw(select_model[[as.integer(input$model_index)]])} 
    else {switch(input$model_list,
                 "PLS" = plot(select_model[[as.integer(input$model_index)]]),
                 "PLS - VIP > 1" = plot(select_model[[as.integer(input$model_index)]]),
                 "MLR" = gglm(select_model[[as.integer(input$model_index)]]))}
  })
  output$table <- renderTable({
    mod.summary
  }, align = "c")
}
shinyApp(ui = ui, server = server)