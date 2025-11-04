# --- 1. Preamble: Load necessary packages ---
# Make sure you have these installed: install.packages(c("shiny", "dplyr", "ggplot2", "DT", "readr"))
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)


df <- readRDS("RDS_dataframes/ICP-MS_meta_09032025.RDS")
df <- df %>% mutate(Sr87_86 = Sr87_ppm / Sr86_ppm)
bad_otos <- c("oto05", "oto12", "oto14", "oto40")
df <- df %>% filter(!specimen %in% bad_otos)
# --- Logic to select the best transects for ALL specimens (run once) ---
top_transects_list <- df %>%
  distinct(specimen, transect, grade) %>%
  filter(grade %in% c(1, 1.5, 2)) %>%
  group_by(specimen) %>%
  arrange(grade) %>%
  mutate(transect_rank = row_number()) %>%
  filter(grade <= 1.5 | transect_rank <= 2) %>%
  slice_head(n = 2) %>% # Ensure a maximum of 2
  ungroup()

# Filter the main dataframe to only include the selected transects
df_to_plot <- df %>%
  semi_join(top_transects_list, by = c("specimen", "transect")) %>%
  # Make sure transect is a factor for plotting
  mutate(transect = factor(transect))

# --- 3. Define the User Interface (UI) ---
ui <- fluidPage(
  titlePanel("Interactive Otolith Region Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("1. Upload your CSV of core regions using the button below."),
      p("2. Select a specimen to view the classified 'core', 'middle', and 'edge' regions."),
      p("3. You can still manually select regions on plots that don't have saved data and save/download them."),
      
      hr(),
      
      # UI element to upload a CSV file
      fileInput("upload_csv", "Upload Saved Regions CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # Dropdown menu for specimen selection
      selectInput("specimen_selector", "Select Specimen:",
                  choices = unique(df_to_plot$specimen)),
      
      # Action button to save the brushed regions
      actionButton("save_button", "Save Selections", icon = icon("save"), class = "btn-primary"),
      
      # Download button for the final data
      downloadButton("download_button", "Download Data as CSV")
    ),
    
    mainPanel(
      # We will create the plot outputs dynamically in the server
      uiOutput("plots_ui"),
      
      hr(),
      h4("Saved Core Regions"),
      # Table to display the saved data
      DT::dataTableOutput("saved_data_table")
    )
  )
)


# --- 4. Define the Server Logic ---
server <- function(input, output, session) {
  
  # Reactive value to store all the manually selected regions
  saved_regions <- reactiveVal(
    tibble(
      specimen = character(),
      transect = character(), # Use character to avoid factor level issues
      core_start_time = numeric(),
      core_end_time = numeric()
    )
  )
  
  # Observer to handle CSV file upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    tryCatch({
      uploaded_data <- read_csv(input$upload_csv$datapath, col_types = cols(
        specimen = "c",
        transect = "c",
        core_start_time = "d",
        core_end_time = "d"
      ))
      
      # Validate required columns
      req("specimen" %in% names(uploaded_data), "transect" %in% names(uploaded_data),
          "core_start_time" %in% names(uploaded_data), "core_end_time" %in% names(uploaded_data))
      
      saved_regions(uploaded_data)
      showNotification("CSV data loaded successfully!", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error reading CSV file:", e$message), type = "error", duration = 8)
    })
  })
  
  
  # Get the data for the currently selected specimen
  current_specimen_data <- reactive({
    req(input$specimen_selector)
    df_to_plot %>%
      filter(specimen == input$specimen_selector)
  })
  
  # Dynamically create plot outputs based on the number of transects
  output$plots_ui <- renderUI({
    transects <- unique(current_specimen_data()$transect)
    plot_outputs <- lapply(seq_along(transects), function(i) {
      plotOutput(paste0("plot_", i), brush = brushOpts(id = paste0("brush_", i), resetOnNew = TRUE))
    })
    tagList(plot_outputs)
  })
  
  # Generate the plots
  observe({
    transects <- sort(unique(current_specimen_data()$transect))
    
    for (i in seq_along(transects)) {
      local({
        transect_id <- transects[i]
        
        output[[paste0("plot_", i)]] <- renderPlot({
          plot_data <- current_specimen_data() %>% filter(transect == transect_id)
          
          # Check for an existing saved region for this specific transect
          existing_region <- saved_regions() %>%
            filter(specimen == input$specimen_selector, transect == as.character(transect_id))
          
          # --- NEW: Conditional plotting logic ---
          if (nrow(existing_region) > 0) {
            # If region data is loaded/saved, classify and color the plot
            classified_data <- plot_data %>%
              mutate(
                region = case_when(
                  time >= (max(time) - 5) ~ "edge",
                  time >= existing_region$core_start_time & time <= existing_region$core_end_time ~ "core",
                  TRUE ~ "middle"
                ),
                region = factor(region, levels = c("core", "middle", "edge"))
              )
            
            p <- ggplot(classified_data, aes(x = time, y = Ba138_ppm, color = region)) +
              geom_line(linewidth = 1.2) +
              scale_color_manual(values = c("core" = "#d9534f", "middle" = "black", "edge" = "#5bc0de"), name = "Region") +
              labs(
                title = paste("Specimen:", input$specimen_selector, "- Transect:", transect_id),
                subtitle = "Regions classified from saved data",
                x = "Time", y = "Ba138_ppm"
              ) +
              theme_bw(base_size = 14)
            
          } else {
            # Otherwise, show the default plot for manual selection
            p <- ggplot(plot_data, aes(x = time, y = Ba138_ppm)) +
              geom_line(color = "grey60") +
              geom_line(aes(y = zoo::rollmean(Ba138_ppm, k = 11, fill = NA, align = "center")), color = "black") +
              labs(
                title = paste("Specimen:", input$specimen_selector, "- Transect:", transect_id),
                subtitle = "Click and drag to select the core region",
                x = "Time", y = "Ba138_ppm"
              ) +
              theme_bw(base_size = 14)
          }
          p
        })
      })
    }
  })
  
  # Logic to run when the "Save Selections" button is clicked
  observeEvent(input$save_button, {
    transects <- sort(unique(current_specimen_data()$transect))
    new_data <- list()
    
    for (i in seq_along(transects)) {
      brush <- input[[paste0("brush_", i)]]
      if (!is.null(brush)) {
        new_data[[i]] <- tibble(
          specimen = input$specimen_selector,
          transect = as.character(transects[i]), # Store as character
          core_start_time = round(brush$xmin, 2),
          core_end_time = round(brush$xmax, 2)
        )
      }
    }
    
    if (length(new_data) > 0) {
      new_regions <- bind_rows(new_data)
      
      # Remove any old entries for the specimens/transects being saved
      current_saved <- saved_regions()
      
      updated_regions <- current_saved %>%
        anti_join(new_regions, by = c("specimen", "transect")) %>%
        bind_rows(new_regions) %>%
        arrange(specimen, transect)
      
      saved_regions(updated_regions)
      
      # Show a confirmation notification
      showNotification("Selections saved!", type = "message", duration = 3)
    } else {
      showNotification("No region selected. Please click and drag on a plot first.", type = "warning", duration = 5)
    }
  })
  
  # Display the saved data in a table
  output$saved_data_table <- DT::renderDataTable({
    DT::datatable(saved_regions(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Handle the download
  output$download_button <- downloadHandler(
    filename = function() {
      paste0("otolith_core_regions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(saved_regions(), file)
    }
  )
}

# --- 5. Run the application ---
shinyApp(ui = ui, server = server)

