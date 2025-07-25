# server.R
# This file defines the server-side logic for the Shiny application.

library(shiny) # Load the shiny package
library(ggplot2) # Load ggplot2 for scatter plot
library(dplyr) # Load dplyr for data manipulation (filter, distinct)
library(DT) # For more advanced table rendering (optional, but good for large tables)
library(htmltools) # For building HTML elements dynamically

# Define the server logic
function(input, output, session) { # Added 'session' argument for updateSelectInput
  
  # --- Histogram Tab Logic ---
  # Reactive expression for histogram data
  # This ensures data is regenerated only when sampleSizeHist changes
  histData <- reactive({
    rnorm(input$sampleSizeHist) # Generate random normal data based on input sample size
  })
  
  # Render the histogram plot
  output$distPlot <- renderPlot({
    x <- histData() # Get the reactive data
    bins <- seq(min(x), max(x), length.out = input$bins + 1) # Calculate bin breaks
    
    # Draw the histogram
    hist(x,
         breaks = bins,
         col = 'steelblue', # Bar color
         border = 'white',  # Border color for bars
         xlab = 'Value', # X-axis label
         main = paste('Histogram of Random Normal Data (N=', input$sampleSizeHist, ')'), # Dynamic title
         ylab = 'Frequency') # Y-axis label
  })
  
  # --- Scatter Plot Tab Logic ---
  # Reactive expression for scatter plot data
  # This ensures data is regenerated only when sampleSizeScatter changes
  scatterData <- reactive({
    n <- input$sampleSizeScatter # Get the reactive sample size
    data.frame(
      x = rnorm(n, mean = 0, sd = 1), # X-values from a normal distribution
      y = rnorm(n, mean = 0, sd = 1) + rnorm(n, mean = 0, sd = 0.5) * 0.5 # Y-values with some correlation
    )
  })
  
  # Render the scatter plot
  output$scatterPlot <- renderPlot({
    df <- scatterData() # Get the reactive data for the scatter plot
    
    # Create the scatter plot using ggplot2
    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = input$pointColor, alpha = 0.7) + # Points with user-selected color and transparency
      theme_minimal() + # Minimal theme for a clean look
      labs(
        title = paste("Scatter Plot of X vs Y (N=", input$sampleSizeScatter, ")"), # Dynamic title
        x = "X-Value", # X-axis label
        y = "Y-Value" # Y-axis label
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
            axis.title = element_text(face = "bold")) # Bold axis titles
  })
  
  # --- CSV Uploader Tab Logic ---
  
  # Reactive value to store the currently selected stage filter
  # Initialized to NULL, meaning no stage filter is applied
  currentStageFilter <- reactiveVal(NULL)
  
  # Reactive value to store the IDs of rows that are temporarily "removed" (grayed out)
  # Initialized as an empty character vector
  removedRows <- reactiveVal(character(0))
  
  # Reactive expression to read the uploaded CSV file
  # This will re-run whenever a new file is uploaded or file input options change
  uploadedData <- reactive({
    req(input$file1) # Require that a file has been uploaded
    
    # Reset removedRows when a new file is uploaded
    removedRows(character(0))
    
    # Read the CSV file based on user-selected options
    df <- tryCatch(
      {
        df_raw <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
        # Add a unique row_id if it doesn't already exist
        if (!"row_id" %in% colnames(df_raw)) {
          df_raw$row_id <- 1:nrow(df_raw)
        }
        df_raw
      },
      error = function(e) {
        # Display an error message if the file cannot be read
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL) # Return NULL to prevent further errors
      }
    )
    return(df)
  })
  
  # Reactive expression to get unique 'case' values from the uploaded data
  # This will re-run when uploadedData changes
  uniqueCases <- reactive({
    df <- uploadedData()
    if (!is.null(df) && "case" %in% colnames(df)) {
      # Get distinct values from the 'case' column
      # Convert to character to handle factors or other types consistently
      c("All", as.character(unique(df$case)))
    } else {
      NULL # Return NULL if 'case' column doesn't exist or no data
    }
  })
  
  # Reactive expression to get unique 'stage_id' values from the uploaded data
  uniqueStageIDs <- reactive({
    df <- uploadedData()
    if (!is.null(df) && "stage_id" %in% colnames(df)) {
      # Get distinct values from the 'stage_id' column
      # Convert to character to handle factors or other types consistently
      as.character(unique(df$stage_id))
    } else {
      NULL # Return NULL if 'stage_id' column doesn't exist or no data
    }
  })
  
  # Render the UI for the 'case' selector dynamically
  output$caseSelector <- renderUI({
    cases <- uniqueCases()
    if (!is.null(cases)) {
      # Create a selectInput dropdown with unique 'case' values
      selectInput(
        inputId = "selectedCase",
        label = "Select Case:",
        choices = cases,
        selected = "All" # Default selection
      )
    } else {
      # Message to display if 'case' column is not found
      p("No 'case' column found in the uploaded file or no file uploaded.")
    }
  })
  
  # Render the UI for the 'stage_id' filter buttons dynamically
  output$stageFilterButtons <- renderUI({
    stage_ids <- uniqueStageIDs()
    if (!is.null(stage_ids)) {
      # Create action buttons for each unique stage_id
      # Also add a "Clear Filter" button
      tagList(
        lapply(stage_ids, function(id) {
          actionButton(
            inputId = paste0("filterStage_", id), # Unique ID for each button
            label = if_else(id == 1, "PRE", "POST"),
            class = "filter-button" # Apply custom CSS class
          )
        }),
        actionButton(
          inputId = "clearStageFilter",
          label = "Clear Stage Filter",
          class = "filter-button clear-filter-button" # Apply custom CSS class
        )
      )
    } else {
      # Message to display if 'stage_id' column is not found
      p("No 'stage_id' column found in the uploaded file or no file uploaded.")
    }
  })
  
  # Observe events for the dynamically created stage filter buttons
  # This loop creates an observer for each potential stage_id button
  observe({
    stage_ids <- uniqueStageIDs()
    if (!is.null(stage_ids)) {
      lapply(stage_ids, function(id) {
        observeEvent(input[[paste0("filterStage_", id)]], {
          currentStageFilter(id) # Set the reactive value to the clicked stage_id
        })
      })
    }
  })
  
  # Observe event for the "Clear Stage Filter" button
  observeEvent(input$clearStageFilter, {
    currentStageFilter(NULL) # Reset the stage filter to NULL
  })
  
  # Reactive expression to filter the data based on selected 'case' AND 'stage_id'
  # This will re-run when uploadedData, selectedCase, or currentStageFilter changes
  filteredData <- reactive({
    df <- uploadedData()
    req(df) # Ensure data is available
    
    # Filter by 'case' column
    selected_case <- input$selectedCase
    if ("case" %in% colnames(df) && !is.null(selected_case) && selected_case != "All") {
      df <- df %>% filter(case == selected_case)
    }
    
    # Filter by 'stage_id' based on button clicks
    selected_stage <- currentStageFilter()
    if ("stage_id" %in% colnames(df) && !is.null(selected_stage)) {
      df <- df %>% filter(stage_id == selected_stage)
    }
    
    return(df)
  })
  
  # Reactive expression for data to be displayed in the table (after row toggles)
  displayData <- reactive({
    df <- filteredData()
    req(df)
    
    # Filter out rows that are in the 'removedRows' list
    # Ensure row_id is treated as character for consistent comparison
    df %>% filter(!as.character(row_id) %in% removedRows())
  })
  
  # Reactive expression to calculate the sum of column "C1" from the displayed data
  sumC1 <- reactive({
    df <- displayData()
    if (!is.null(df) && "C1" %in% colnames(df) && is.numeric(df$C1)) {
      sum(df$C1, na.rm = TRUE)
    } else {
      "N/A (Column 'C1' not found or not numeric)"
    }
  })
  
  # Render the sum of C1
  output$sumC1Display <- renderText({
    paste("Sum of 'C1':", sumC1())
  })
  
  # Render the table preview with toggle buttons and dynamic styling
  output$filePreview <- renderUI({
    df_display <- filteredData() # Use filteredData here to show all rows before individual toggling
    req(df_display)
    
    # Get the list of currently removed row IDs
    current_removed_rows <- removedRows()
    
    # Create table header
    # The "Toggle Row" header is now the first one
    table_header <- tags$thead(
      tags$tr(
        tags$th("Toggle Row"), # Moved to the first position
        lapply(colnames(df_display), tags$th) # Remaining headers
      )
    )
    
    # Create table rows
    table_rows <- lapply(1:nrow(df_display), function(i) {
      row_data <- df_display[i, ]
      row_id <- as.character(row_data$row_id) # Get the unique row_id
      
      # Determine if the row is currently removed (grayed out)
      is_removed <- row_id %in% current_removed_rows
      row_class <- if (is_removed) "grayed-out" else ""
      button_label <- if (is_removed) "Add Back" else "Remove"
      button_class <- if (is_removed) "filter-button clear-filter-button" else "filter-button"
      
      tags$tr(
        class = row_class, # Apply class for styling
        tags$td( # This td contains the toggle button and is now the first cell
          actionButton(
            inputId = paste0("toggleRow_", row_id), # Unique ID for each toggle button
            label = button_label,
            class = button_class
          )
        ),
        lapply(row_data, function(cell) tags$td(as.character(cell))) # Remaining cell data
      )
    })
    
    # Combine header and rows into a table
    tags$table(
      table_header,
      tags$tbody(table_rows)
    )
  })
  
  # Observe events for the dynamically created toggle row buttons
  observe({
    df_ids <- uploadedData()$row_id # Get all potential row_ids from the original uploaded data
    if (!is.null(df_ids)) {
      lapply(as.character(df_ids), function(id) {
        observeEvent(input[[paste0("toggleRow_", id)]], {
          current_removed <- removedRows()
          if (id %in% current_removed) {
            # If already removed, add it back
            removedRows(setdiff(current_removed, id))
          } else {
            # If not removed, add it to the removed list
            removedRows(c(current_removed, id))
          }
        })
      })
    }
  })
}
