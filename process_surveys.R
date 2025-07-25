library(readxl)
library(readr)

# input file
SURVEY_XLS_FILE <- "data/surveys.xlsx"

# output file
SURVEY_FILE <- "data/surveys.csv"

# read the sheet names of the Excel file
survey_names <- excel_sheets(SURVEY_XLS_FILE)

# remove invalid and "template" 
survey_names <- sort(survey_names[!grepl("^~", survey_names) & survey_names != "template"])

get_surveys <- function(survey_names) {
  
  surveys <- list()
  
  # Iterate over each sheet in the workbook
  for (survey_name in survey_names) {
    
    # Read the current sheet into a data frame
    df <- read_excel(SURVEY_XLS_FILE, sheet = survey_name)
    
    # Check if required columns exist
    required_columns <- c("considerations", "policies", "scale_max", "q-method")
    missing_cols <- setdiff(required_columns, colnames(df))
    if (length(missing_cols) > 0) {
      cat(
        "Sheet",
        survey_name,
        "is missing the following columns:",
        paste(missing_cols, collapse = ", "),
        "\n\n"
      )
      next
    }
    
    # Calculate the number of non-NA rows in "considerations" column
    n_c <- sum(!is.na(df$considerations))
    
    # get considerations
    c <- df %>%
      select(considerations_order, considerations) %>%
      filter(!is.na(considerations)) %>%
      arrange(considerations_order) %>%
      pivot_wider(names_prefix = "C", names_from = considerations_order,
                  values_from = considerations)
    
    # ensure c has columns up to C50
    for (ci in (n_c + 1):50) {
      c[[paste0("C", ci)]] <- NA
    }
    
    # Calculate the number of non-NA rows in "policies" column
    n_p <- sum(!is.na(df$policies))
    
    # get policies
    p <- df %>%
      select(policies_order, policies) %>%
      filter(!is.na(policies)) %>%
      arrange(policies_order) %>%
      pivot_wider(names_prefix = "P", names_from = policies_order,
                  values_from = policies)
    
    # ensure p has columns up to P10
    for (pi in (n_p + 1):10) {
      p[[paste0("P", pi)]] <- NA
    }
    
    # Extract integer values from "scale_max" column, assuming they are already integers
    scale_max <- as.integer(na.omit(df$scale_max))
    
    # Extract logical (boolean) values from "q-method" column
    q_method <- as.logical(na.omit(df$`q-method`))
    
    surveys[[length(surveys) + 1]] <- tibble(
      survey = survey_name,
      considerations = n_c,
      policies = n_p,
      scale_max,
      q_method,
      c,
      p,
    )
    
  }
  
  surveys <- bind_rows(surveys)
  surveys
  
}


surveys <- get_surveys(survey_names)

write_csv(surveys, SURVEY_FILE)
