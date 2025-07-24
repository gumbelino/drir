library(readr)
library(dplyr)
library(tidyr)

# input data files
HUMAN_DATA_FILE <- "data/human_data.csv"

# output data files
MARGINAL_RESULTS_FILE <- "output/marginal_results.csv"

# load data
human_data <- read_csv(HUMAN_DATA_FILE, show_col_types = FALSE)

# source helper functions
source("dri_functions.R")

# get cases
cases <- unique(human_data$case)

# get stages 
stage_ids <- sort(unique(human_data$stage_id))

marginals_results <- list()

# for each case...
for (case in cases) {
  
  # for each stage (pre/post)...
  for (stage_id in stage_ids) {
    
    # get case data
    case_data <- human_data %>% filter(case == !!case, stage_id == !!stage_id)
    
    # initialize case results 
    case_res <- tibble(
      case,
      stage_id,
    )
    
    # get original dri
    original_dri <- get_dri(case_data)
    
    # get number of considerations
    nc <- ncol(getc(case_data))
    
    # to track marginal dri
    marginal_dri <- list()
    
    for (ci in 1:nc) {
      
      # get test data
      test_data <- case_data
      
      # column to remove
      cx <- paste0("C", ci)
      
      # remove column
      test_data[[cx]] <- NA
      
      # get updated dri
      dri <- get_dri(test_data)
      
      marginal_dri[[length(marginal_dri)+1]] <- tibble(
        cx,
        mdri = dri - original_dri ## FIXME: I think it should be the opposite.
      )
      
    }
    
    marginal_dri <- bind_rows(marginal_dri)
    
    case_marginals <- pivot_wider(marginal_dri,
                                  names_from = cx,
                                  values_from = mdri)
    
    # create results row
    case_res <- bind_cols(case_res, case_marginals)
    
    # save results
    marginals_results[[length(marginals_results)+1]] <- case_res

  }
  
}

marginals_results <- bind_rows(marginals_results)

write_csv(marginals_results, MARGINAL_RESULTS_FILE)