# --- 1. Load Packages ---
library(shiny)
library(ggplot2)
library(tidyverse)

# --- 2. Load Data (Copied from your script) ---
# This section loads and filters your data just as your main script does,
# so the app has the 'final_filtered_data' to work with.

cat("Loading data...\n")

# Load and re-create the 'df_filtered' dataframe
df_raw <- readRDS("RDS_dataframes/ICP-MS_clean.RDS")
df_raw <- df_raw[c(30:184)]
df_list_raw <- list()
for(i in 1:length(df_raw)){
  spec <- names(df_raw[i])
  df_list_raw[[i]] <- df_raw[[i]]
  df_list_raw[[i]]$specimen <- as.factor(str_sub(spec, start= 0,end = 5))
  df_list_raw[[i]]$transect <- as.factor(as.numeric(str_sub(spec, start= -1)) + 1)
}
df <- do.call("rbind", df_list_raw)
df <- rename(df, "time" = `Elapsed Time`)
unusable <- paste0("oto", c(05, 10, 12, 14, 30, 37, 38, 40))
bad <- paste0("oto", c(16, 18, 20, 28, 35))
good <- as.character(unique(df$specimen))
good <- good[!good %in% c(unusable, bad)]
df_filtered <- df %>% filter(specimen %in% good)

# Load Manual Cutoffs
cutoff_file <- "manual_cutoffs.csv"
if (!file.exists(cutoff_file)) {
  stop("Error: 'manual_cutoffs.csv' not found. Please place it in the same directory.")
}
manual_cutoffs <- read_csv(cutoff_file, 
                           col_types = cols(
                             specimen = col_factor(),
                             transect = col_factor(),
                             start_time = col_double(),
                             end_time = col_double()
                           ))

# Join and Filter Data
df_joined <- df_filtered %>%
  inner_join(manual_cutoffs, by = c("specimen", "transect"))

final_filtered_data <- df_joined %>%
  filter(time >= start_time & time <= end_time) %>%
  select(-start_time, -end_time) %>%
  # Add a row index *within each transect*
  group_by(specimen, transect) %>%
  mutate(index = row_number()) %>%
  ungroup()

# ‼️ NEW: Get a list of elements to plot
# Exclude non-numeric/identifier columns
non_plot_cols <- c("specimen", "transect", "time", "index")
plottable_elements <- colnames(final_filtered_data)[!(colnames(final_filtered_data) %in% non_plot_cols)]
# Exclude ones user doesn't want
plottable_elements <- plottable_elements[!(plottable_elements %in% c("Ca43", "Mn55_ppm"))]


cat("Data loading complete. Launching app...\n")

# --- 3. Shiny App ---

ui <- fluidPage(
  titlePanel("Otolith Core Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select specimen
      selectInput("specimen", "Select Specimen:",
                  choices = unique(final_filtered_data$specimen)),
      
      # ‼️ NEW: Dropdown to select element
      selectInput("element_select", "Select Element to Plot:",
                  choices = plottable_elements,
                  selected = "Mg24_ppm"),
      
      hr(),
      
      h4("Click Info:"),
      # Text output to show click coordinates
      verbatimTextOutput("click_info")
    ),
    
    mainPanel(
      # UPDATED: Increased plot height
      plotOutput("transect_plot", 
                 click = "plot_click",
                 height = "900px") # ID for click event
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to get the data for the selected specimen
  selected_specimen_data <- reactive({
    req(input$specimen)
    
    final_filtered_data %>%
      filter(specimen == input$specimen)
  })
  
  # Render the plot
  output$transect_plot <- renderPlot({
    # ‼️ NEW: Require input$element_select
    req(selected_specimen_data(), input$element_select)
    
    # ‼️ UPDATED: Plot only the selected element, facet by transect
    # We use aes_string() to pass the element name as a string
    ggplot(selected_specimen_data(), aes_string(x = "index", y = input$element_select)) +
      geom_line(color = "blue", linewidth = 0.8) +
      # Facet by transect, stacked vertically, with independent y-axes
      facet_wrap(~ transect, ncol = 1, scales = "free_y") +
      # ‼️ NEW: Dynamic labs
      labs(title = paste("Specimen:", input$specimen, "-", input$element_select, "Signal"),
           x = "Row Index (from filtered start point)",
           y = paste(input$element_select, "Signal Value (Free Scales)")) +
      theme_bw() +
      theme(legend.position = "none",
            strip.text = element_text(size = 12, face="bold"))
  })
  
  # Show click info
  output$click_info <- renderPrint({
    # ‼️ NEW: Require input$element_select
    req(input$plot_click, input$element_select)
    
    # ‼️ UPDATED: Use selected_specimen_data() and check against the selected element
    # Store the selected element name in a variable
    yvar_selected <- input$element_select
    
    clicked_point <- nearPoints(
      selected_specimen_data(),
      input$plot_click,
      xvar = "index",
      yvar = yvar_selected, # Use the variable here
      threshold = 100, # Large threshold to catch nearest point
      maxpoints = 1
    )
    
    if (nrow(clicked_point) > 0) {
      # ‼️ NEW: Dynamically show the clicked element's name and value
      cat(paste("Clicked Point:\n",
                "Transect:", clicked_point$transect[1], "\n",
                "Row Index:", clicked_point$index[1], "\n",
                "Time:", round(clicked_point$time[1], 3), "\n",
                paste0(yvar_selected, ":"), round(clicked_point[[yvar_selected]][1], 3)
      ))
    } else {
      cat("Click on a plot to see point details.")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

