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

# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Otolith Transect Annotation Tool"),
  
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
      
      hr(),
      
      h4("Step 3: Annotate"),
      selectInput("element_select", "Element to Visualize", choices = NULL),
      
      # The percentile slider has been removed as requested
      
      radioButtons("point_type", "Point to Mark:",
                   choices = c("Transect Start", "Transect End", "Core Start", "Core End", "Otolith Edge"),
                   selected = "Transect Start"),
      
      helpText("Use the controls above the image to switch between Annotate and Zoom modes."),
      
      hr(),
      
      h4("Step 4: Save & Export"),
      actionButton("save_button", "Save Current Annotations", class = "btn-primary"),
      actionButton("clear_button", "Clear Points for this Transect"),
      hr(),
      downloadButton("download_data", "Download All Annotations")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               radioButtons("interaction_mode", h4("Interaction Mode"), 
                            choices = c("Annotate (Click Points)" = "click", "Zoom (Drag Box)" = "brush"), 
                            selected = "click", inline = TRUE)
        ),
        column(2, offset = 2,
               actionButton("reset_zoom_button", "Reset Zoom", style="margin-top: 25px;")
        )
      ),
      
      plotOutput("oto_plot", height = "600px",
                 click = "plot_click",
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
      hr(),
      h4("Saved Annotations"),
      DTOutput("annotations_table")
    )
  )
)

# --- 3. Define the Server Logic ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = NULL,
    annotations = tibble(
      specimen = character(), transect = character(), point_type = character(),
      time = numeric(), image_x = numeric(), image_y = numeric()
    ),
    points_buffer = list(),
    plot_lims = NULL 
  )
  
  observe({
    image_files <- list.files("www", pattern = "\\.png$|\\.jpg$|\\.jpeg$|\\.tif$|\\.tiff$")
    updateSelectInput(session, "image_select", choices = image_files)
  })
  
  observeEvent(input$data_file, {
    req(input$data_file)
    file_path <- input$data_file$datapath
    file_name <- tolower(input$data_file$name)
    
    if (endsWith(file_name, ".rds")) {
      rv$data <- readRDS(file_path)
    } else if (endsWith(file_name, ".csv")) {
      rv$data <- read.csv(file_path)
    } else if (endsWith(file_name, ".xlsx") || endsWith(file_name, ".xls")) {
      rv$data <- read_excel(file_path, sheet = 1)
    } else {
      showModal(modalDialog(title = "Error", "Unsupported file type."))
      return()
    }
    
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
  
  observeEvent(input$plot_click, {
    req(input$interaction_mode == "click", input$image_select)
    
    new_point <- list(point_type = input$point_type, x = input$plot_click$x, y = input$plot_click$y)
    buffer_point_types <- sapply(rv$points_buffer, function(p) p$point_type)
    existing_idx <- match(new_point$point_type, buffer_point_types)
    if (!is.na(existing_idx)) {
      rv$points_buffer[[existing_idx]] <- new_point
    } else {
      rv$points_buffer <- c(rv$points_buffer, list(new_point))
    }
  })
  
  observeEvent(input$plot_brush, {
    req(input$interaction_mode == "brush")
    
    rv$plot_lims <- list(
      xlim = c(input$plot_brush$xmin, input$plot_brush$xmax),
      ylim = c(input$plot_brush$ymin, input$plot_brush$ymax)
    )
    session$resetBrush("plot_brush")
  })
  
  observeEvent(input$reset_zoom_button, {
    rv$plot_lims <- NULL
  })
  
  output$oto_plot <- renderPlot({
    req(input$image_select, rv$data)
    
    img <- image_read(file.path("www", input$image_select))
    img_info <- image_info(img)
    
    lims <- rv$plot_lims
    if (is.null(lims)) {
      lims <- list(xlim = c(0, img_info$width), ylim = c(0, img_info$height))
    }
    
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = "n", xlim = lims$xlim, ylim = lims$ylim,
         asp = 1, xlab = "", ylab = "", axes = FALSE)
    rasterImage(img, 0, 0, img_info$width, img_info$height)
    
    start_pt <- Filter(function(p) p$point_type == "Transect Start", rv$points_buffer)
    end_pt <- Filter(function(p) p$point_type == "Transect End", rv$points_buffer)
    
    if (length(start_pt) > 0 && length(end_pt) > 0) {
      start_x <- start_pt[[1]]$x; start_y <- start_pt[[1]]$y
      end_x <- end_pt[[1]]$x; end_y <- end_pt[[1]]$y
      transect_data <- rv$data %>%
        filter(specimen == input$specimen_select, transect == input$transect_select) %>%
        arrange(time)
      req(nrow(transect_data) > 1)
      
      # --- UPDATED: Color scale calculation logic ---
      
      # 1. Get all element values for the whole transect
      all_element_vals <- transect_data[[input$element_select]]
      
      # 2. Define the cutoff time (95% of the max time)
      time_cutoff <- max(transect_data$time, na.rm = TRUE) * 0.95
      
      # 3. Get the subset of element values from the first 95% of the transect
      vals_for_scaling <- transect_data[[input$element_select]][transect_data$time <= time_cutoff]
      
      # 4. Calculate the color scale range (quantiles) based on this subset
      q_vals <- quantile(vals_for_scaling, probs = c(0.01, 0.99), na.rm = TRUE)
      
      # 5. Apply the color scale to the ENTIRE transect
      if (is.na(q_vals[1]) || is.na(q_vals[2]) || q_vals[1] == q_vals[2]) {
        colors <- "green"
      } else {
        color_ramp <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))
        colors <- color_ramp(100)[cut(all_element_vals, breaks = seq(q_vals[1], q_vals[2], length.out=101), include.lowest=TRUE)]
        colors[is.na(colors)] <- "grey" # Color values outside the 1-99 percentile of the subset
      }
      
      n_segments <- nrow(transect_data)
      x_coords <- seq(start_x, end_x, length.out = n_segments + 1)
      y_coords <- seq(start_y, end_y, length.out = n_segments + 1)
      segments(x0 = x_coords[1:n_segments], y0 = y_coords[1:n_segments], x1 = x_coords[2:(n_segments + 1)], y1 = y_coords[2:(n_segments + 1)], col = colors, lwd = 5)
    }
    
    if (length(rv$points_buffer) > 0) {
      for (pt in rv$points_buffer) {
        points(pt$x, pt$y, col = "deeppink", pch = 16, cex = 2)
        text(pt$x, pt$y, labels = pt$point_type, col = "white", pos = 3)
      }
    }
  })
  
  observeEvent(input$clear_button, {
    rv$points_buffer <- list()
  })
  
  observeEvent(input$save_button, {
    req(rv$points_buffer, nrow(rv$data) > 0)
    start_pt <- Filter(function(p) p$point_type == "Transect Start", rv$points_buffer)
    end_pt <- Filter(function(p) p$point_type == "Transect End", rv$points_buffer)
    if (length(start_pt) == 0 || length(end_pt) == 0) {
      showModal(modalDialog(title = "Error", "Please mark at least 'Transect Start' and 'Transect End' before saving."))
      return()
    }
    transect_data <- rv$data %>%
      filter(specimen == input$specimen_select, transect == input$transect_select) %>%
      arrange(time)
    start_x <- start_pt[[1]]$x; start_y <- start_pt[[1]]$y
    end_x <- end_pt[[1]]$x; end_y <- end_pt[[1]]$y
    total_length_sq <- (end_x - start_x)^2 + (end_y - start_y)^2
    new_annotations <- lapply(rv$points_buffer, function(pt) {
      dx <- end_x - start_x; dy <- end_y - start_y
      t <- if (total_length_sq == 0) 0 else { ((pt$x - start_x) * dx + (pt$y - start_y) * dy) / total_length_sq }
      t <- pmax(0, pmin(1, t))
      time_index <- round(1 + t * (nrow(transect_data) - 1))
      corresponding_time <- transect_data$time[time_index]
      tibble(specimen = input$specimen_select, transect = input$transect_select, point_type = pt$point_type, time = corresponding_time, image_x = pt$x, image_y = pt$y)
    }) %>% bind_rows()
    rv$annotations <- rv$annotations %>%
      filter(!(specimen == input$specimen_select & transect == input$transect_select)) %>%
      bind_rows(new_annotations)
    shinyjs::info(paste("Saved annotations for", input$specimen_select, "transect", input$transect_select))
    rv$points_buffer <- list()
  })
  
  output$annotations_table <- renderDT({
    datatable(rv$annotations, options = list(pageLength = 5))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("otolith_annotations_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$annotations, file, row.names = FALSE)
    }
  )
}

# --- 4. Run the application ---
shinyApp(ui = ui, server = server)