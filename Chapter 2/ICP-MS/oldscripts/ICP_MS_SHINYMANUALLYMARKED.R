# --- 1. Load Packages ---
# Install these if you don't have them
# install.packages(c("shiny", "tidyverse", "shinyWidgets"))
library(shiny)
library(shinyWidgets) # For a nicer dropdown
library(tidyverse)


# --- 2. Load & Prepare All Data ---
# This section contains all the code from our previous steps to make
# this script fully reproducible.

# Load and select otoliths
df_raw <- readRDS("RDS_dataframes/ICP-MS_clean.RDS")
df_raw <- df_raw[c(30:184)]

# Create single dataframe
df_list_raw <- list()
for(i in 1:length(df_raw)){
  spec <- names(df_raw[i])
  df_list_raw[[i]] <- df_raw[[i]]
  df_list_raw[[i]]$specimen <- as.factor(str_sub(spec, start= 0,end = 5))
  df_list_raw[[i]]$transect <- as.factor(as.numeric(str_sub(spec, start= -1)) + 1)
}
df <- do.call("rbind", df_list_raw)
df <- rename(df, "time" = `Elapsed Time`)

# Filter out junk specimens
unusable <- paste0("oto", c(05, 10, 12, 14, 30, 37, 38, 40))
bad <- paste0("oto", c(16, 18, 20, 28, 35))
good <- as.character(unique(df$specimen))
good <- good[!good %in% c(unusable, bad)]

# This is our main dataframe, unfiltered by time
df_filtered <- df %>% filter(specimen %in% good)


# --- 3. Run Initial Algorithmic Filtering ---

# Define the Cu-based "bowl" function
find_otolith_boundaries_cu <- function(ca43_data, cu65_data, 
                                       window_size = 7, 
                                       threshold_offset = 1.0, # Using your optimized 1.0
                                       buffer = 3) {            # Using a tighter buffer
  
  # --- 1. Preparation ---
  n_points <- length(cu65_data)
  min_stable_points <- 5
  ma_cu <- asinh(stats::runmed(cu65_data, k = window_size))
  
  # --- 2. Find Baseline & Threshold ---
  mid_start <- ceiling(n_points * 0.3)
  mid_end <- floor(n_points * 0.7)
  cu_baseline <- median(ma_cu[mid_start:mid_end], na.rm = TRUE)
  cu_threshold <- cu_baseline + threshold_offset 
  
  # --- 3. Start Detection (Find the drop) ---
  start_idx <- NULL
  for(i in window_size:(mid_end)) {
    window <- ma_cu[i:(i + min_stable_points - 1)]
    if(all(window < cu_threshold, na.rm = TRUE)) {
      start_idx <- max(1, i - buffer) 
      break
    }
  }
  if(is.null(start_idx)) { start_idx <- ceiling(n_points * 0.1) }
  
  # --- 4. End Detection (Find the rise) ---
  end_idx <- n_points 
  for(i in mid_start:(n_points - min_stable_points)) {
    if(is.na(ma_cu[i])) next 
    if(ma_cu[i] > cu_threshold) {
      end_idx <- min(n_points, i + buffer) 
      break
    }
  }
  
  # --- 5. Validate and Return ---
  if(end_idx - start_idx < 100) { 
    start_idx <- max(1, start_idx - buffer*2)
    end_idx <- min(n_points, end_idx + buffer*2)
  }
  if (end_idx <= start_idx) {
    start_idx <- ceiling(n_points * 0.1)
    end_idx <- n_points
  }
  
  return(list(
    start = start_idx,
    end = end_idx,
    filtered_data = ca43_data[start_idx:end_idx]
  ))
}

# Define the processor
process_otoliths <- function(otolith_list) {
  results <- list()
  for(name in names(otolith_list)) {
    tryCatch({
      bounds <- find_otolith_boundaries_cu( # Using the Cu-based function
        otolith_list[[name]]$Ca43,
        otolith_list[[name]]$Cu65_ppm
      )
      results[[name]] <- bounds
    }, error = function(e) {
      cat("Error processing", name, ":", e$message, "\n")
    })
  }
  summary_df <- data.frame(
    transect = names(results),
    start_index = sapply(results, `[[`, "start"),
    end_index = sapply(results, `[[`, "end")
  )
  return(summary_df)
}

# --- 4. Create Initial Cutoff Table (for the app) ---
# This is the "starting point" for our manual review

# 1. Split df_filtered into a named list
df_list <- df_filtered %>%
  group_split(specimen, transect)

list_names <- purrr::map_chr(df_list, function(transect_df) {
  paste(as.character(transect_df$specimen[1]), 
        as.character(transect_df$transect[1]), sep = "-")
})
names(df_list) <- list_names

# 2. Run the algorithmic processing
results_from_algo <- process_otoliths(df_list)

# 3. Add row-number index *within each transect* to df_filtered
df_with_index <- df_filtered %>%
  group_by(specimen, transect) %>%
  mutate(index = row_number()) %>%
  ungroup()

# 4. Split 'results' to match df_filtered columns
results_split <- results_from_algo %>%
  tidyr::separate(transect, into = c("specimen_char", "transect_char"), sep = "-", remove = FALSE) %>%
  mutate(
    specimen = factor(specimen_char),
    transect = factor(transect_char)
  ) %>%
  select(specimen, transect, start_index, end_index)

# 5. Find the 'time' value for the start_index
start_times <- df_with_index %>%
  inner_join(results_split, by = c("specimen", "transect", "index" = "start_index")) %>%
  select(specimen, transect, start_time = time)

# 6. Find the 'time' value for the end_index
end_times <- df_with_index %>%
  inner_join(results_split, by = c("specimen", "transect", "index" = "end_index")) %>%
  select(specimen, transect, end_time = time)

# 7. This is the master cutoff table we will edit in the app
initial_cutoff_data <- start_times %>%
  inner_join(end_times, by = c("specimen", "transect")) %>%
  arrange(specimen, transect)

# --- 5. Prepare Plotting Data ---
# Pivot data into a long format for plotting
df_long_transformed <- df_filtered %>%
  select(specimen, transect, time, Ca43, Cu65_ppm) %>%
  pivot_longer(
    cols = c(Ca43, Cu65_ppm),
    names_to = "element",
    values_to = "value"
  ) %>%
  mutate(
    value = ifelse(element == "Cu65_ppm", asinh(value), value),
    element = factor(element, 
                     levels = c("Ca43", "Cu65_ppm"), 
                     labels = c("Ca43", "Cu65_ppm (asinh)"))
  )


# --- 6. Shiny App ---

ui <- fluidPage(
  titlePanel("Otolith Transect Review and Editing Tool"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Select Specimen"),
      # Use pickerInput for a searchable dropdown
      pickerInput(
        inputId = "specimen_select",
        label = "Select specimen to review:",
        choices = unique(initial_cutoff_data$specimen),
        options = list(`live-search` = TRUE)
      ),
      
      hr(),
      
      h4("2. Adjust Cutoffs"),
      p("Change values below and click 'Apply Changes'"),
      # Dynamic UI for transect controls will appear here
      uiOutput("transect_controls"),
      
      actionButton("apply_changes", "Apply Changes to This Specimen", 
                   icon = icon("check"), class = "btn-primary"),
      
      hr(),
      
      h4("3. Save & Download"),
      p("Save your manually reviewed cutoff times to a CSV file."),
      # Use a simple button to save, not a downloadHandler
      actionButton("save_cutoffs_csv", "Save Cutoffs File", 
                   icon = icon("save"), class = "btn-success"),
      
      br(), br(),
      
      p("Download the final, filtered ICP-MS data."),
      downloadButton("download_filtered_data", "Download Filtered Data")
    ),
    
    mainPanel(
      width = 9,
      # The plots for the selected specimen will appear here
      plotOutput("transect_plots", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # --- Reactive Value to hold the master cutoff table ---
  rv_cutoffs <- reactiveVal(initial_cutoff_data)
  
  # --- 1. Dynamic UI for Transect Controls ---
  output$transect_controls <- renderUI({
    
    req(input$specimen_select) # Wait until a specimen is selected
    
    # Get the cutoffs for the *currently selected* specimen
    spec_cutoffs <- rv_cutoffs() %>%
      filter(specimen == input$specimen_select)
    
    # Create a list of UI elements (numeric inputs)
    lapply(1:nrow(spec_cutoffs), function(i) {
      tran_num <- as.character(spec_cutoffs$transect[i])
      
      wellPanel(
        style = "padding: 10px; margin-bottom: 10px;",
        h5(paste("Transect:", tran_num)),
        fluidRow(
          column(6,
                 numericInput(
                   inputId = paste0("start_", tran_num),
                   label = "Start Time",
                   value = round(spec_cutoffs$start_time[i], 2),
                   step = 0.1
                 )
          ),
          column(6,
                 numericInput(
                   inputId = paste0("end_", tran_num),
                   label = "End Time",
                   value = round(spec_cutoffs$end_time[i], 2),
                   step = 0.1
                 )
          )
        )
      )
    })
  })
  
  # --- 2. Observe "Apply Changes" Button ---
  observeEvent(input$apply_changes, {
    
    req(input$specimen_select)
    
    # Get the current master table
    current_table <- rv_cutoffs()
    
    # Get transects for the selected specimen
    transects_to_update <- current_table %>%
      filter(specimen == input$specimen_select) %>%
      pull(transect)
    
    # Loop through the transects *for this specimen*
    for (tran_num in transects_to_update) {
      
      # Get the new values from the UI controls
      start_input_id <- paste0("start_", tran_num)
      end_input_id <- paste0("end_", tran_num)
      
      if (!is.null(input[[start_input_id]]) && !is.null(input[[end_input_id]])) {
        new_start <- input[[start_input_id]]
        new_end <- input[[end_input_id]]
        
        # Find the row in the master table and update it
        row_index <- which(current_table$specimen == input$specimen_select & 
                             current_table$transect == tran_num)
        
        if(length(row_index) == 1) {
          current_table$start_time[row_index] <- new_start
          current_table$end_time[row_index] <- new_end
        }
      }
    }
    
    # Set the reactive value, which will trigger the plot to re-render
    rv_cutoffs(current_table)
    
    # Show a confirmation
    showNotification("Cutoffs updated for this specimen.", type = "message", duration = 3)
  })
  
  # --- 3. Render the Plots ---
  output$transect_plots <- renderPlot({
    
    req(input$specimen_select)
    
    # Get plot data for the selected specimen
    plot_data <- df_long_transformed %>%
      filter(specimen == input$specimen_select)
    
    # Get the *current* cutoff values from the reactive value
    spec_cutoffs <- rv_cutoffs() %>%
      filter(specimen == input$specimen_select)
    
    # Pivot cutoffs to long format for geom_vline
    cutoff_lines <- spec_cutoffs %>%
      pivot_longer(
        cols = c(start_time, end_time),
        names_to = "cutoff_type",
        values_to = "time_value"
      )
    
    # Generate the plot (same as our PDF script)
    ggplot(plot_data, aes(x = time, y = value)) +
      geom_line(aes(color = element), linewidth = 0.7) +
      
      # Add the reactive vertical lines
      geom_vline(
        data = cutoff_lines,
        aes(xintercept = time_value),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
      ) +
      
      facet_wrap(transect ~ element, scales = "free", ncol = 2) +
      
      scale_color_manual(values = c("Ca43" = "#0060A0", "Cu65_ppm (asinh)" = "#D55E00")) +
      
      labs(
        title = paste("Specimen:", input$specimen_select),
        subtitle = "Review and adjust cutoff times. Click 'Apply Changes' to update.",
        x = "Time (Elapsed)",
        y = "Signal Value / asinh(Value)"
      ) +
      theme_bw(base_size = 14) +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        legend.position = "none"
      )
  })
  
  # --- 4. Save Cutoffs Button ---
  observeEvent(input$save_cutoffs_csv, {
    
    # Get the final, manually-reviewed data
    final_cutoffs_to_save <- rv_cutoffs()
    
    # Save it to a CSV
    file_name <- "manual_cutoffs.csv"
    write.csv(final_cutoffs_to_save, file_name, row.names = FALSE)
    
    # Show a modal confirmation
    showModal(modalDialog(
      title = "File Saved!",
      paste("Your manually reviewed cutoffs have been saved to:", file_name)
    ))
  })
  
  # --- 5. Download Filtered Data ---
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      # Get the final table of cutoffs from the reactive value
      final_cutoffs <- rv_cutoffs()
      
      # Join the original data with the final cutoffs
      # and filter based on the (now manually reviewed) times
      final_filtered_df <- df_filtered %>%
        inner_join(final_cutoffs, by = c("specimen", "transect")) %>%
        filter(time >= start_time & time <= end_time) %>%
        # Clean up the extra columns
        select(-start_time, -end_time)
      
      # Write the filtered data to the CSV
      write.csv(final_filtered_df, file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
