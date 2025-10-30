# --- 0. Load All Required Packages ---
# Make sure you have these installed first:
# install.packages(c("shiny", "dplyr", "tidyr", "readr", "magick", "DT", "shinyjs", "readxl", "zoo"))

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(magick)
library(DT)
library(shinyjs)
library(readxl)
library(zoo)

# --- 1. Data Pre-processing Script ---
# This code will run ONCE when the app is started.

cat("Preparing data...\n")

# Load raw data and manually marked core regions
# IMPORTANT: Make sure these file paths are correct for your system
df <- readRDS("/Users/zachstamplis/Desktop/Thesis and Otoliths/Github/LPW_FT-NIRS/RDS_dataframes/ICP-MS_meta_09032025.RDS")
marked_cores <- read_csv("/Users/zachstamplis/Desktop/Thesis and Otoliths/Github/LPW_FT-NIRS/otolith_core_regions_2025-09-04.csv")

# Perform mutations, filtering, and classification
df_raw <- df %>% 
  mutate(Sr87_86 = Sr87_ppm / Sr86_ppm) %>%
  filter(!specimen %in% c("oto05", "oto12", "oto14", "oto40"))

# --- THIS IS THE FIX ---
# We ensure the 'transect' column is a character in BOTH dataframes before joining.
marked_cores <- marked_cores %>% mutate(transect = as.character(transect))

df_classified <- df_raw %>%
  mutate(transect = as.character(transect)) %>%
  left_join(marked_cores, by = c("specimen", "transect"))

df_final <- df_classified %>%
  group_by(specimen, transect) %>%
  filter(time >= core_start_time) %>%
  mutate(
    region = case_when(
      time >= (max(time, na.rm = TRUE) - 5) ~ "edge",
      !is.na(core_start_time) & time >= core_start_time & time <= core_end_time ~ "core",
      TRUE ~ "middle"
    ),
    region = factor(region, levels = c("core", "middle", "edge"))
  ) %>%
  ungroup() %>%
  select(-core_start_time, -core_end_time)

cat("Data preparation complete. Launching app...\n")


# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Visualizing Raw vs. Smoothed Transect Data"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Select Transect & Image"),
      selectInput("image_select", "Choose Otolith Image", choices = NULL),
      selectInput("specimen_select", "Choose Specimen", choices = NULL),
      selectInput("transect_select", "Choose Transect", choices = NULL),
      
      hr(),
      
      h4("Step 2: Visualize"),
      radioButtons("color_by", "Color Line By:",
                   choices = c("Region" = "region", "Element Concentration" = "element"),
                   selected = "region"),
      
      conditionalPanel(
        condition = "input.color_by == 'element'",
        selectInput("element_select", "Element to Visualize", choices = NULL),
        checkboxInput("show_smoothed", "Display Smoothed Data", value = FALSE),
        sliderInput("smoothing_window", "Smoothing Window Size:", 
                    min = 3, max = 51, value = 5, step = 2)
      ),
      
      helpText("Use the controls above the image to switch between Action (for drawing) and Zoom.")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               radioButtons("interaction_mode", h4("Interaction Mode"), 
                            choices = c("Action (Click)" = "action", "Zoom (Drag Box)" = "zoom"), 
                            selected = "action", inline = TRUE)
        ),
        column(2, offset = 2,
               actionButton("reset_zoom_button", "Reset Zoom", style="margin-top: 25px;")
        )
      ),
      plotOutput("oto_plot", height = "500px",
                 click = "plot_click",
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
      hr(),
      plotOutput("profile_plot", height = "200px")
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = df_final, 
    transect_pts = list(),
    plot_lims = NULL
  )
  
  # --- UI Population Logic ---
  observe({
    image_files <- list.files("www", pattern = "\\.png$|\\.jpg$|\\.jpeg$|\\.tif$|\\.tiff$")
    updateSelectInput(session, "image_select", choices = image_files)
  })
  
  observe({
    req(rv$data)
    
    specimens <- unique(rv$data$specimen)
    updateSelectInput(session, "specimen_select", choices = specimens)
    
    element_choices <- rv$data %>%
      select(where(is.numeric), -any_of(c("time", "grade", "specimen_number", "transect"))) %>%
      names()
    updateSelectInput(session, "element_select", choices = element_choices)
  })
  
  observeEvent(input$specimen_select, {
    req(rv$data)
    transects <- unique(rv$data$transect[rv$data$specimen == input$specimen_select])
    updateSelectInput(session, "transect_select", choices = transects)
  })
  
  # --- Reactive expression for filtered and smoothed data ---
  processed_data <- reactive({
    req(rv$data, input$specimen_select, input$transect_select)
    
    transect_df <- rv$data %>%
      filter(specimen == input$specimen_select, transect == as.character(input$transect_select)) %>%
      arrange(time)
    
    k_smooth <- if (input$smoothing_window %% 2 == 0) input$smoothing_window + 1 else input$smoothing_window
    
    transect_df %>%
      mutate(across(
        .cols = where(is.numeric) & !any_of(c("time", "grade", "specimen_number")),
        .fns = ~ zoo::rollmean(.x, k = k_smooth, fill = NA, align = "center"),
        .names = "{.col}_smooth"
      ))
  })
  
  # --- Click and Zoom Logic ---
  observeEvent(c(input$image_select, input$specimen_select, input$transect_select), {
    rv$transect_pts <- list(); rv$plot_lims <- NULL
  })
  
  observeEvent(input$plot_click, {
    req(input$interaction_mode == "action")
    click_coords <- list(x = input$plot_click$x, y = input$plot_click$y)
    
    if (length(rv$transect_pts) < 2) {
      rv$transect_pts <- c(rv$transect_pts, list(click_coords))
    } else {
      rv$transect_pts <- list(click_coords)
    }
  })
  
  observeEvent(input$plot_brush, {
    req(input$interaction_mode == "zoom")
    rv$plot_lims <- list(xlim = c(input$plot_brush$xmin, input$plot_brush$xmax),
                         ylim = c(input$plot_brush$ymin, input$plot_brush$ymax))
    session$resetBrush("plot_brush")
  })
  observeEvent(input$reset_zoom_button, { rv$plot_lims <- NULL })
  
  # --- Main Plot Rendering ---
  output$oto_plot <- renderPlot({
    req(input$image_select)
    img <- image_read(file.path("www", input$image_select)); img_info <- image_info(img)
    
    lims <- rv$plot_lims
    if (is.null(lims)) { lims <- list(xlim = c(0, img_info$width), ylim = c(0, img_info$height)) }
    
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = "n", xlim = lims$xlim, ylim = lims$ylim, asp = 1, xlab = "", ylab = "", axes = FALSE)
    rasterImage(img, 0, 0, img_info$width, img_info$height)
    
    if (length(rv$transect_pts) == 2) {
      start_pt <- rv$transect_pts[[1]]; end_pt <- rv$transect_pts[[2]]
      
      current_data <- processed_data()
      req(nrow(current_data) > 1)
      
      colors <- "grey"
      
      if (input$color_by == 'region') {
        region_colors <- c("core" = "#E41A1C", "middle" = "#4DAF4A", "edge" = "#377EB8") 
        colors <- region_colors[current_data$region]
      } else if (input$color_by == 'element') {
        col_to_use <- if (input$show_smoothed) paste0(input$element_select, "_smooth") else input$element_select
        element_vals <- current_data[[col_to_use]]
        
        time_cutoff <- max(current_data$time, na.rm = TRUE) * 0.95
        vals_for_scaling <- current_data[[col_to_use]][current_data$time <= time_cutoff]
        q_vals <- quantile(vals_for_scaling, probs = c(0.01, 0.99), na.rm = TRUE)
        
        if (is.na(q_vals[1]) || is.na(q_vals[2]) || q_vals[1] == q_vals[2]) {
          colors <- "green"
        } else {
          color_ramp <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
          breaks <- seq(q_vals[1], q_vals[2], length.out = 101)
          colors <- color_ramp(100)[cut(element_vals, breaks = breaks, include.lowest = TRUE)]
          colors[is.na(colors)] <- "grey"
        }
      }
      
      n_segments <- nrow(current_data)
      x_coords <- seq(start_pt$x, end_pt$x, length.out = n_segments + 1)
      y_coords <- seq(start_pt$y, end_pt$y, length.out = n_segments + 1)
      segments(x_coords[-length(x_coords)], y_coords[-length(y_coords)], x_coords[-1], y_coords[-1], col = colors, lwd = 5)
    }
  })
  
  # --- 1D Profile Plot Rendering ---
  output$profile_plot <- renderPlot({
    req(processed_data(), input$color_by == 'element', length(rv$transect_pts) == 2)
    
    df <- processed_data()
    raw_col <- input$element_select
    smooth_col <- paste0(raw_col, "_smooth")
    
    ggplot(df, aes(x = time)) +
      geom_line(aes(y = .data[[raw_col]]), color = "grey70", linewidth = 1) +
      geom_line(aes(y = .data[[smooth_col]]), color = "firebrick", linewidth = 1.2) +
      labs(title = paste("Profile for", raw_col),
           subtitle = "Grey = Raw Data, Red = Smoothed Data",
           x = "Time (seconds)", y = "Concentration") +
      theme_minimal(base_size = 14)
  })
  
}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)