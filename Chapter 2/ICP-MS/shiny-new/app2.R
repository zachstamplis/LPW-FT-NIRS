# --- 1. Load required packages and increase upload limit ---
# Make sure you have these installed first:
# install.packages(c("shiny", "dplyr", "ggplot2", "magick", "DT", "shinyjs", "readxl"))

options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)
library(dplyr)
library(ggplot2)
library(magick)
library(DT)
library(shinyjs)
library(readxl)

# --- Helper function to calculate distance ---
euc_dist <- function(p1, p2) {
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Advanced Otolith Transect Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Load Data"),
      fileInput("data_file", "Upload ICP-MS Data (.RDS, .csv, or .xlsx)",
                accept = c(".rds", ".csv", ".xlsx", ".xls")),
      
      hr(),
      
      h4("Step 2: Select Transect & Image"),
      selectInput("image_select", "Choose Otolith Image", choices = NULL),
      selectInput("specimen_select", "Choose Specimen", choices = NULL),
      selectInput("transect_select", "Choose Transect", choices = NULL),
      selectInput("element_select", "Element to Visualize", choices = NULL),
      
      hr(),
      
      h4("Step 3: Analysis Workflow"),
      wellPanel(
        radioButtons("workflow_step", "Current Task:",
                     choices = c("Calibrate Scale" = "calibrate",
                                 "Place & Rotate Transect" = "place",
                                 "Annotate Points" = "annotate"),
                     selected = "calibrate"),
        
        conditionalPanel(
          condition = "input.workflow_step == 'calibrate'",
          helpText("Click 2 points on the image to define the scale bar."),
          numericInput("scale_bar_length", "Scale Bar Length (µm)", value = 100),
          actionButton("calculate_scale_button", "Calculate Scale"),
          # NEW: Manual override for the scale
          uiOutput("manual_scale_ui")
        ),
        
        conditionalPanel(
          condition = "input.workflow_step == 'place'",
          helpText("Use the buttons to enable clicking for the start point and angle."),
          actionButton("set_start_button", "1. Enable Set Start", class = "btn-info"),
          actionButton("set_end_button", "2. Enable Set Angle", class = "btn-info")
        ),
        
        conditionalPanel(
          condition = "input.workflow_step == 'annotate'",
          radioButtons("point_type", "Point to Mark:",
                       choices = c("Core Start", "Core End", "Otolith Edge", "Custom"),
                       selected = "Core Start"),
          helpText("Click on the image near the placed transect to mark a point.")
        )
      ),
      
      hr(),
      
      h4("Step 4: Save & Export"),
      actionButton("save_button", "Save Annotations", class = "btn-primary"),
      actionButton("clear_button", "Clear Annotations"),
      downloadButton("download_data", "Download All Annotations")
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
      plotOutput("oto_plot", height = "700px",
                 click = "plot_click",
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
      DTOutput("annotations_table")
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = NULL, annotations = tibble(), scale_pts = list(),
    pixels_per_um = NULL, transect_length_pixels = NULL,
    transect_pts = list(), annotation_pts = list(),
    placement_mode = NULL, plot_lims = NULL,
    calibrations = list()
  )
  
  observe({
    image_files <- list.files("www", pattern = "\\.png$|\\.jpg$|\\.jpeg$|\\.tif$|\\.tiff$")
    updateSelectInput(session, "image_select", choices = image_files)
  })
  
  observeEvent(input$data_file, {
    req(input$data_file)
    file_path <- input$data_file$datapath; file_name <- tolower(input$data_file$name)
    if (endsWith(file_name, ".rds")) { rv$data <- readRDS(file_path) }
    else if (endsWith(file_name, ".csv")) { rv$data <- read.csv(file_path) }
    else if (endsWith(file_name, ".xlsx") || endsWith(file_name, ".xls")) { rv$data <- read_excel(file_path, sheet = 1) }
    
    specimens <- unique(rv$data$specimen)
    updateSelectInput(session, "specimen_select", choices = specimens)
    elements <- names(rv$data)[!names(rv$data) %in% c("specimen", "transect", "time")]
    updateSelectInput(session, "element_select", choices = elements)
  })
  
  observeEvent(input$specimen_select, {
    req(rv$data)
    transects <- unique(rv$data$transect[rv$data$specimen == input$specimen_select])
    updateSelectInput(session, "transect_select", choices = transects)
  })
  
  observeEvent(c(input$image_select, input$specimen_select, input$transect_select), {
    rv$transect_pts <- list(); rv$annotation_pts <- list(); rv$plot_lims <- NULL
    
    current_image <- isolate(input$image_select)
    if (!is.null(current_image) && current_image %in% names(rv$calibrations)) {
      rv$pixels_per_um <- rv$calibrations[[current_image]]
    } else {
      rv$scale_pts <- list(); rv$pixels_per_um <- NULL
    }
  })
  
  observeEvent(input$set_start_button, { rv$placement_mode <- "start" })
  observeEvent(input$set_end_button, {
    req(length(rv$transect_pts) > 0, msg = "Please set a start point first.")
    rv$placement_mode <- "end"
  })
  
  observeEvent(input$plot_click, {
    req(input$interaction_mode == "action")
    click_coords <- list(x = input$plot_click$x, y = input$plot_click$y)
    
    if (input$workflow_step == 'calibrate') {
      if (length(rv$scale_pts) < 2) { rv$scale_pts <- c(rv$scale_pts, list(click_coords)) } 
      else { rv$scale_pts <- list(click_coords) }
    } else if (input$workflow_step == 'place' && !is.null(rv$placement_mode)) {
      req(rv$pixels_per_um, rv$transect_length_pixels)
      
      if (rv$placement_mode == "start") {
        start_pt <- click_coords
        end_pt <- list(x = start_pt$x + rv$transect_length_pixels, y = start_pt$y)
        rv$transect_pts <- list(start = start_pt, end = end_pt)
      } else if (rv$placement_mode == "end") {
        start_pt <- rv$transect_pts$start
        vec_x <- click_coords$x - start_pt$x; vec_y <- click_coords$y - start_pt$y
        current_dist <- sqrt(vec_x^2 + vec_y^2)
        if (current_dist > 0) {
          unit_vec_x <- vec_x / current_dist; unit_vec_y <- vec_y / current_dist
          new_end_x <- start_pt$x + unit_vec_x * rv$transect_length_pixels
          new_end_y <- start_pt$y + unit_vec_y * rv$transect_length_pixels
          rv$transect_pts$end <- list(x = new_end_x, y = new_end_y)
        }
      }
      rv$placement_mode <- NULL
    } else if (input$workflow_step == 'annotate') {
      req(length(rv$transect_pts) == 2)
      start_pt <- rv$transect_pts$start; end_pt <- rv$transect_pts$end
      dx <- end_pt$x - start_pt$x; dy <- end_pt$y - start_pt$y
      total_length_sq <- dx^2 + dy^2
      t_val <- if (total_length_sq == 0) 0 else { ((click_coords$x - start_pt$x) * dx + (click_coords$y - start_pt$y) * dy) / total_length_sq }
      t_val <- pmax(0, pmin(1, t_val))
      proj_x <- start_pt$x + t_val * dx; proj_y <- start_pt$y + t_val * dy
      new_point <- list(point_type = input$point_type, x = proj_x, y = proj_y)
      existing_idx <- which(sapply(rv$annotation_pts, function(p) p$point_type == new_point$point_type))
      if (length(existing_idx) > 0) { rv$annotation_pts[[existing_idx]] <- new_point }
      else { rv$annotation_pts <- c(rv$annotation_pts, list(new_point)) }
    }
  })
  
  observeEvent(input$calculate_scale_button, {
    req(length(rv$scale_pts) == 2, input$scale_bar_length > 0)
    pixel_dist <- euc_dist(rv$scale_pts[[1]], rv$scale_pts[[2]])
    rv$pixels_per_um <- pixel_dist / input$scale_bar_length
    rv$calibrations[[input$image_select]] <- rv$pixels_per_um
  })
  
  # NEW: UI for manual scale adjustment
  output$manual_scale_ui <- renderUI({
    req(rv$pixels_per_um)
    numericInput("manual_scale_input", "Adjust Scale (px/µm):", value = rv$pixels_per_um, step = 0.01)
  })
  
  # UPDATED: Transect length now depends on the manual input if available
  observe({
    # Use manual scale if it exists, otherwise use the calculated one
    final_scale <- if (!is.null(input$manual_scale_input)) {
      input$manual_scale_input
    } else {
      rv$pixels_per_um
    }
    
    req(rv$data, final_scale, input$specimen_select, input$transect_select)
    
    # Save the final scale back to the persistent list for the image
    rv$calibrations[[input$image_select]] <- final_scale
    
    max_time <- rv$data %>%
      filter(specimen == input$specimen_select, transect == input$transect_select) %>%
      summarise(max_t = max(time, na.rm = TRUE)) %>% pull(max_t)
    
    transect_um <- max_time * 3.0294
    rv$transect_length_pixels <- transect_um * final_scale
  })
  
  observeEvent(input$plot_brush, {
    req(input$interaction_mode == "zoom")
    rv$plot_lims <- list(xlim = c(input$plot_brush$xmin, input$plot_brush$xmax),
                         ylim = c(input$plot_brush$ymin, input$plot_brush$ymax))
    session$resetBrush("plot_brush")
  })
  observeEvent(input$reset_zoom_button, { rv$plot_lims <- NULL })
  
  output$oto_plot <- renderPlot({
    req(input$image_select)
    img <- image_read(file.path("www", input$image_select)); img_info <- image_info(img)
    
    lims <- rv$plot_lims
    if (is.null(lims)) { lims <- list(xlim = c(0, img_info$width), ylim = c(0, img_info$height)) }
    
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = "n", xlim = lims$xlim, ylim = lims$ylim, asp = 1, xlab = "", ylab = "", axes = FALSE)
    rasterImage(img, 0, 0, img_info$width, img_info$height)
    
    if (length(rv$scale_pts) == 2) {
      lines(x = c(rv$scale_pts[[1]]$x, rv$scale_pts[[2]]$x), y = c(rv$scale_pts[[1]]$y, rv$scale_pts[[2]]$y), col = "yellow", lwd = 3)
    }
    
    if (length(rv$transect_pts) == 2) {
      start_pt <- rv$transect_pts$start; end_pt <- rv$transect_pts$end
      transect_data <- rv$data %>%
        filter(specimen == input$specimen_select, transect == input$transect_select) %>%
        arrange(time)
      req(nrow(transect_data) > 1, input$element_select %in% names(transect_data))
      
      element_vals <- transect_data[[input$element_select]]
      q_vals <- quantile(element_vals, probs = c(0.01, 0.99), na.rm = TRUE)
      
      if (length(unique(na.omit(q_vals))) < 2 || all(is.na(q_vals)) || q_vals[1] == q_vals[2]) { colors <- "green" }
      else {
        color_ramp <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
        breaks <- seq(q_vals[1], q_vals[2], length.out = 101)
        colors <- color_ramp(100)[cut(element_vals, breaks = breaks, include.lowest = TRUE)]
        colors[is.na(colors)] <- "grey"
      }
      
      n_segments <- nrow(transect_data)
      x_coords <- seq(start_pt$x, end_pt$x, length.out = n_segments + 1)
      y_coords <- seq(start_pt$y, end_pt$y, length.out = n_segments + 1)
      segments(x_coords[-length(x_coords)], y_coords[-length(y_coords)], x_coords[-1], y_coords[-1], col = colors, lwd = 5)
      
      points(x = start_pt$x, y = start_pt$y, col = "green", pch = 19, cex = 1.5)
      points(x = end_pt$x, y = end_pt$y, col = "blue", pch = 19, cex = 1.5)
    }
    
    for (pt in rv$annotation_pts) {
      points(pt$x, pt$y, col = "deeppink", pch = 16, cex = 2)
      text(pt$x, pt$y, labels = pt$point_type, col = "white", pos = 3)
    }
  })
  
  observeEvent(input$clear_button, {
    rv$annotation_pts <- list(); rv$transect_pts <- list(); rv$scale_pts <- list()
  })
  
  observeEvent(input$save_button, {
    req(length(rv$transect_pts) == 2, length(rv$annotation_pts) > 0)
    transect_data <- rv$data %>%
      filter(specimen == input$specimen_select, transect == input$transect_select) %>%
      arrange(time)
    start_pt <- rv$transect_pts$start; end_pt <- rv$transect_pts$end
    total_length_sq <- (end_pt$x - start_pt$x)^2 + (end_pt$y - start_pt$y)^2
    
    new_annotations <- lapply(rv$annotation_pts, function(pt) {
      dx <- end_pt$x - start_pt$x; dy <- end_pt$y - start_pt$y
      t_val <- if (total_length_sq == 0) 0 else { ((pt$x - start_pt$x) * dx + (pt$y - start_pt$y) * dy) / total_length_sq }
      time_index <- round(1 + t_val * (nrow(transect_data) - 1))
      corresponding_time <- transect_data$time[time_index]
      tibble(specimen = input$specimen_select, transect = input$transect_select,
             point_type = pt$point_type, time = corresponding_time, 
             image_x = pt$x, image_y = pt$y)
    }) %>% bind_rows()
    
    rv$annotations <- rv$annotations %>%
      filter(!(specimen == input$specimen_select & transect == input$transect_select)) %>%
      bind_rows(new_annotations)
    
    shinyjs::info(paste("Saved annotations for", input$specimen_select, "transect", input$transect_select))
    rv$annotation_pts <- list()
  })
  
  output$annotations_table <- renderDT({
    datatable(rv$annotations, options = list(pageLength = 5))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("otolith_annotations_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(rv$annotations, file, row.names = FALSE) }
  )
}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)