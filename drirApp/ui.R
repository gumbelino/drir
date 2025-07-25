# ui.R
# This file defines the user interface for the Shiny application.

library(shiny) # Load the shiny package

# Define the UI using navbarPage for a multi-page layout
navbarPage(
  # Application title displayed in the navigation bar
  title = "Enhanced Shiny App",
  
  # Add custom CSS for a slightly prettier look (e.g., font, background)
  # This uses tags$head to insert HTML into the document's head
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f8f8f8; /* Light grey background */
      }
      .well {
        background-color: #ffffff; /* White background for sidebars/panels */
        border: 1px solid #e3e3e3;
        border-radius: 8px; /* Rounded corners for panels */
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); /* Subtle shadow */
      }
      .navbar {
        background-color: #4CAF50; /* Green navigation bar */
        color: white;
      }
      .navbar .navbar-brand {
        color: white !important;
        font-weight: bold;
      }
      .navbar .nav > li > a {
        color: white !important;
      }
      .navbar .nav > li > a:hover {
        background-color: #45a049; /* Darker green on hover */
      }
      h2 {
        color: #333;
      }
      /* Style for the table output */
      #filePreview table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 20px;
      }
      #filePreview th, #filePreview td {
        border: 1px solid #ddd;
        padding: 8px;
        text-align: left;
      }
      #filePreview th {
        background-color: #f2f2f2;
        font-weight: bold;
      }
      /* Style for the filter buttons */
      .filter-button {
        background-color: #007bff; /* Blue button */
        color: white;
        border: none;
        padding: 8px 15px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 14px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 5px;
        transition: background-color 0.3s ease;
      }
      .filter-button:hover {
        background-color: #0056b3; /* Darker blue on hover */
      }
      .clear-filter-button {
        background-color: #dc3545; /* Red button for clearing */
      }
      .clear-filter-button:hover {
        background-color: #c82333;
      }
      /* Style for grayed-out rows */
      .grayed-out {
        opacity: 0.5;
        background-color: #f0f0f0;
        text-decoration: line-through;
      }
    "))
  ),
  
  # First Tab: Histogram
  tabPanel(
    "Histogram", # Title for this tab
    icon = icon("chart-bar"), # Icon for the tab (requires fontawesome)
    
    # Sidebar layout for the Histogram tab
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        h2("Histogram Controls"), # Section title for controls
        # Slider input for number of bins
        sliderInput(
          inputId = "bins",
          label = "Number of Bins:",
          min = 1,
          max = 50,
          value = 30
        ),
        # Numeric input for sample size (for the histogram's data generation)
        numericInput(
          inputId = "sampleSizeHist",
          label = "Sample Size (for Histogram):",
          value = 1000,
          min = 100,
          max = 10000
        )
      ),
      
      # Main panel for displaying the histogram output
      mainPanel(
        h2("Distribution of Data"), # Section title for output
        plotOutput(outputId = "distPlot")
      )
    )
  ),
  
  # Second Tab: Scatter Plot
  tabPanel(
    "Scatter Plot", # Title for this tab
    icon = icon("scatter-chart"), # Icon for the tab
    
    # Sidebar layout for the Scatter Plot tab
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        h2("Scatter Plot Controls"), # Section title for controls
        # Numeric input for sample size for the scatter plot
        numericInput(
          inputId = "sampleSizeScatter",
          label = "Number of Points:",
          value = 500,
          min = 10,
          max = 5000
        ),
        # Select input for choosing point color
        selectInput(
          inputId = "pointColor",
          label = "Point Color:",
          choices = c(
            "Blue" = "blue",
            "Red" = "red",
            "Green" = "green",
            "Purple" = "purple",
            "Orange" = "orange"
          ),
          selected = "blue"
        )
      ),
      
      # Main panel for displaying the scatter plot output
      mainPanel(
        h2("Relationship Between Variables"), # Section title for output
        plotOutput(outputId = "scatterPlot")
      )
    )
  ),
  
  # Third Tab: CSV Uploader and Viewer
  tabPanel(
    "CSV Uploader", # Title for this new tab
    icon = icon("file-csv"), # Icon for the tab
    
    # Sidebar layout for the CSV Uploader tab
    sidebarLayout(
      # Sidebar panel for file input and column selection
      sidebarPanel(
        h2("Upload CSV File"), # Section title
        # File input for CSV upload
        fileInput(
          inputId = "file1",
          label = "Choose CSV File",
          multiple = FALSE, # Allow only single file upload
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv") # Accepted file types
        ),
        tags$hr(), # Horizontal rule for separation
        
        # Checkbox for header
        checkboxInput("header", "Header", TRUE),
        
        # Radio buttons for separator
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Radio buttons for quote
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        tags$hr(),
        
        h2("Filter by 'case'"), # Section title for filtering
        # UI output for dynamic select input for 'case' column
        uiOutput("caseSelector"),
        tags$hr(),
        
        h2("Filter by 'stage_id'"), # New section for stage_id filtering
        # UI output for dynamic stage_id filter buttons
        uiOutput("stageFilterButtons")
      ),
      
      # Main panel for displaying the file preview
      mainPanel(
        h2("CSV File Preview"), # Section title
        textOutput("sumC1Display"), # Display the sum of 'C1'
        uiOutput("filePreview") # Output for the table preview (now uiOutput)
      )
    )
  )
)
