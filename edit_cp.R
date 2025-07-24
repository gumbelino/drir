library(readr)
library(dplyr)

# input data files
HUMAN_DATA_FILE <- "data/human_data.csv"

# load data
human_data <- read_csv(HUMAN_DATA_FILE, show_col_types = FALSE)

# source helper functions
source("dri_functions.R")

# get cases
cases <- unique(human_data$case)

optimal_human_data <- list()
optimization_res <- list()

# optimize each case
for (case in cases) {
  
  # get case data, post-deliberation
  case_data <- human_data %>% filter(case == !!case, stage_id == 2)
  
  # get original dri and number of considerations
  original_dri <- get_dri(case_data)
  original_nc <- length(getc(case_data))
  
  # optimize set of considerations
  optimal_data <- optimize_dri(case_data)
  
  # get optimized dri
  optimal_dri <- get_dri(optimal_data)
  optimal_nc <- length(getc(optimal_data))
  
  # calculate differences
  diff_dri <- optimal_dri - original_dri
  ratio_nc <- round(optimal_nc / original_nc, 4) * 100
  
  # save results
  optimal_human_data[[length(optimal_human_data)+1]] <- optimal_data
  optimization_res[[length(optimization_res)+1]] <- tibble(
    case,
    original_dri,
    original_nc,
    optimal_dri,
    optimal_nc
  )
  
  # print result
  cat("RESULTS FOR", case, "\n",
    "\tOPTIMAL DRI:", optimal_dri, "| ORIGINAL DRI:", original_dri, "| DIFF:", 
      diff_dri, 
      "\n\tKEEP", optimal_nc, "/", original_nc, "considerations |", ratio_nc, "%\n")
  
}

optimization_res <- bind_rows(optimization_res)
optimal_human_data <- bind_rows(optimal_human_data)

write_csv(optimization_res, "optimal")
