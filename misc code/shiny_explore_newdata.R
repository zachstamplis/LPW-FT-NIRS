# --- 3. Join Marked Regions with Main Data ---
# Example: df_raw <- read.csv("path/to/your/full_otolith_data.csv")


df <- readRDS("RDS_dataframes/ICP-MS_meta_09032025.RDS")
df <- df %>% mutate(Sr87_86 = Sr87_ppm / Sr86_ppm)
bad_otos <- c("oto05", "oto12", "oto14", "oto40")
df <- df %>% filter(!specimen %in% bad_otos)
df_raw <- df # Using the sample 'df' from the app for this example

# Load the CSV file with your manually marked core regions
# This is the file you downloaded from the Shiny app.
# Example: marked_cores <- read_csv("path/to/your/otolith_core_regions.csv")
marked_cores <- read_csv("otolith_core_regions_2025-09-04.csv", col_types = cols(
  specimen = "c",
  transect = "c",
  core_start_time = "d",
  core_end_time = "d"
))


# --- 3. Join Marked Regions with Main Data ---

# We'll use a left_join to add the core_start_time and core_end_time
# to every row of the corresponding specimen and transect in your main data.
# Note: We ensure 'transect' is a character in both for a clean join.
df_classified <- df_raw %>%
  mutate(transect = as.character(transect)) %>%
  left_join(marked_cores, by = c("specimen", "transect"))


# --- 4. Classify Each Point into a Region ---

# Now, we create the 'region' column based on your rules.
# The case_when() function checks these rules in order for each row.
df_final <- df_classified %>%
  # We must group by specimen and transect for the following operations
  group_by(specimen, transect) %>%
  # For each transect, snip off all data points occurring before the marked core region.
  # This will also implicitly remove any transects that were not in the marked_cores CSV.
  filter(time >= core_start_time) %>%
  mutate(
    region = case_when(
      # Rule 1: The 'edge' is the last 5 seconds of the transect.
      time >= (max(time, na.rm = TRUE) - 5) ~ "edge",
      
      # Rule 2: The 'core' is between the start and end times from your CSV.
      # This will only apply to rows that have start/end times (i.e., were marked).
      !is.na(core_start_time) & time >= core_start_time & time <= core_end_time ~ "core",
      
      # Rule 3: Anything else is the 'middle'.
      TRUE ~ "middle"
    ),
    # Optional: Convert the region column to a factor for easier plotting
    region = factor(region, levels = c("core", "middle", "edge"))
  ) %>%
  # Ungroup to prevent accidental grouped operations later
  ungroup() %>%
  # Optional: Remove the temporary start/end time columns if you don't need them
  select(-core_start_time, -core_end_time)

# Automatically get the list of elements available for plotting
# This filters out non-numeric or identifier columns.
element_choices <- df_final %>%
  select(where(is.numeric), -any_of(c("time", "grade", "specimen_number"))) %>%
  names()

# Pivot the data from "wide" to "long" format for easier plotting with ggplot
df_long <- df_final %>%
  pivot_longer(
    cols = all_of(element_choices),
    names_to = "element",
    values_to = "value"
  )


# --- 3. Define the User Interface (UI) ---
ui <- fluidPage(
  titlePanel("Otolith Elemental Data Visualizer"),
  sidebarLayout(
    sidebarPanel(
      h4("Plot Controls"),
      # Dropdown for specimen selection
      selectInput("specimen_choice", "Select Specimen:",
                  choices = unique(df_long$specimen)),
      
      # UI Output for the transect dropdown (dynamically generated)
      uiOutput("transect_ui"),
      
      # Dropdown for element selection
      selectInput("element_choice", "Select Element:",
                  choices = element_choices, selected = "Ba138_ppm")
    ),
    mainPanel(
      # Output for the plot
      plotOutput("element_plot", height = "600px")
    )
  )
)


# --- 4. Define the Server Logic ---
server <- function(input, output, session) {
  
  # Dynamically generate the transect selector based on the chosen specimen
  output$transect_ui <- renderUI({
    req(input$specimen_choice)
    
    available_transects <- df_long %>%
      filter(specimen == input$specimen_choice) %>%
      distinct(transect) %>%
      pull(transect)
    
    selectInput("transect_choice", "Select Transect:",
                choices = available_transects)
  })
  
  # Filter the data based on user selections
  plot_data <- reactive({
    req(input$specimen_choice, input$transect_choice, input$element_choice)
    
    df_long %>%
      filter(
        specimen == input$specimen_choice,
        transect == input$transect_choice,
        element == input$element_choice
      )
  })
  
  # Render the plot
  output$element_plot <- renderPlot({
    # Ensure data is available before trying to plot
    validate(
      need(nrow(plot_data()) > 0, "No data available for the selected options. Please check your data.")
    )
    
    ggplot(plot_data(), aes(x = time, y = value, color = region)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(
        name = "Region",
        values = c("core" = "#d9534f", "middle" = "black", "edge" = "#5bc0de"),
        drop = FALSE # Prevents colors from changing if a region is missing
      ) +
      labs(
        title = paste("Element Profile:", input$element_choice),
        subtitle = paste("Specimen:", input$specimen_choice, "| Transect:", input$transect_choice),
        x = "Time",
        y = "Concentration / Value"
      ) +
      theme_bw(base_size = 16) +
      theme(legend.position = "top")
  })
}


# --- 5. Run the application ---
shinyApp(ui = ui, server = server)
