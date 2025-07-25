# server.R
# This file defines the server-side logic for the Shiny application.

library(shiny) # Load the shiny package
library(ggplot2) # Load ggplot2 for scatter plot
library(dplyr) # Load dplyr for data manipulation (filter, distinct)

# Define the server logic
function(input, output) {
  
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
  
  # Reactive expression to read the uploaded CSV file
  # This will re-run whenever a new file is uploaded or file input options change
  uploadedData <- reactive({
    req(input$file1) # Require that a file has been uploaded
    
    # Read the CSV file based on user-selected options
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # Return a message if there's an error reading the file
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  # Reactive expression to get unique 'case' values from the uploaded data
  # This will re-run when uploadedData changes
  uniqueCases <- reactive({
    df <- uploadedData()
    if ("case" %in% colnames(df)) {
      # Get distinct values from the 'case' column
      # Convert to character to handle factors or other types consistently
      c("All", as.character(unique(df$case)))
    } else {
      NULL # Return NULL if 'case' column doesn't exist
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
      p("No 'case' column found in the uploaded file.")
    }
  })
  
  # Reactive expression to filter the data based on selected 'case'
  # This will re-run when uploadedData or selectedCase changes
  filteredData <- reactive({
    df <- uploadedData()
    selected_case <- input$selectedCase
    
    if ("case" %in% colnames(df) && !is.null(selected_case) && selected_case != "All") {
      # Filter the data if a specific case is selected
      df %>% filter(case == selected_case)
    } else {
      # Return the full data if 'All' is selected or 'case' column is missing
      df
    }
  })
  
  # Render the table preview of the filtered data
  output$filePreview <- renderTable({
    filteredData() # Display the filtered (or full) data
  })
}
