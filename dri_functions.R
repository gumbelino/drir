
getc <- function(case_data) {
  
  # get consideration data
  cdf <- case_data %>% select(C1:C50)
  
  # remove NA columns
  cdf <- cdf[,colSums(is.na(cdf)) < nrow(cdf)]
  
  return(cdf)
  
} 


popc <- function(cdf, ci) {
  
  # column to remove
  cx <- paste0("C", ci)
  
  # remove column
  cdf <- cdf %>% select(-all_of(cx))
  
  # rename columns
  cdf <- cdf %>% rename_with(~ paste0("C", seq_along(.)), everything())
  
  return(cdf)
  
}


calculate_dri <- function(data, v1, v2) {
  d <- abs((data[[v1]] - data[[v2]]) / sqrt(2))
  lambda <- 1 - (sqrt(2) / 2)
  
  # Scalar penalty based on strength of signal (|r| and |q|)
  penalty <- ifelse(pmax(abs(data[[v1]]), abs(data[[v2]])) <= 0.2, pmax(abs(data[[v1]]), abs(data[[v2]])) / 0.2, 1)
  
  consistency <- (1 - d) * penalty
  avg_consistency <- mean(consistency)
  
  dri <- 2 * ((avg_consistency - lambda) / (1 - lambda)) - 1
  
  return(dri)
}

get_dri <- function(data) {
  if (nrow(data) < 1) {
    return(NA)
  }
  
  pnums <- data$pnum
  
  Q <- data %>% select(C1:C50)
  R <- data %>% select(P1:P10)
  
  # remove all NA columns (in case there are less than 50
  Q <- Q[, colSums(is.na(Q)) != nrow(Q)]
  R <- R[, colSums(is.na(R)) != nrow(R)]
  
  # transpose data
  Q <- t(Q) %>% as.data.frame()
  R <- t(R) %>% as.data.frame()
  
  # name columns with participant numbers
  colnames(Q) <- pnums
  colnames(R) <- pnums
  
  # obtain a list of correlations without duplicates
  # cor() returns a correlation matrix between Var1 and Var2
  # Var1 and Var2 are the variables being correlated
  # Freq is the correlation
  QWrite <- subset(as.data.frame(as.table(cor(Q, method = "spearman"))),
                   match(Var1, names(Q)) > match(Var2, names(Q)))
  
  RWrite <- subset(as.data.frame(as.table(cor(R, method = "spearman"))),
                   match(Var1, names(R)) > match(Var2, names(R)))
  
  # initialize the output in the first iteration
  IC <- data.frame("P_P" = paste0(QWrite$Var1, '-', QWrite$Var2))
  IC$P1 <- as.numeric(as.character(QWrite$Var1))
  IC$P2 <- as.numeric(as.character(QWrite$Var2))
  
  # prepare QWrite
  QWrite <- as.data.frame(QWrite$Freq)
  names(QWrite) <- "Q2"
  
  # prepare RWrite for merge
  RWrite <- as.data.frame(RWrite$Freq)
  names(RWrite) <- "R2"
  
  # merge
  IC <- cbind(IC, QWrite, RWrite)
  
  ## IC Points calculations ##
  IC$IC_POST <- 1 - abs((IC$R2 - IC$Q2) / sqrt(2))
  
  ## Group DRI level V3 ##
  DRI_POST_V3 <- calculate_dri(IC, 'R2', 'Q2')
  
  return(DRI_POST_V3)
  
}


optimize_dri <- function(case_data, greedy=TRUE) {
  
  # get current optimal dri
  optimal_dri <- get_dri(case_data)
  
  # get number of considerations
  nc <- ncol(getc(case_data))
  
  # current optimal subset of considerations
  optimal_subset <- case_data
  
  if (!greedy) {
    warning("Non-greedy approach not yet implemented!")
    return(NA)  
  }
  
  # initialize loop variables
  test_data <- optimal_subset
  
  # track which considerations to keep
  # keep <- c()
  
  ## this is a greedy way [O(n)] of increasing the value of dri 
  ## NOTE: it does NOT find the optimal subset of considerations;
  ## it only reduces the amount of considerations to
  ## increase the value of dri
  for (ci in 1:nc) {
    
    # column to remove
    cx <- paste0("C", ci)
    
    # remove column
    test_data[[cx]] <- NA
    
    # get updated dri
    dri <- get_dri(test_data)
    
    # found a better dri
    if (dri > optimal_dri) {
      # cat("POP", cx, optimal_dri, "\n")
      optimal_dri <- dri
      optimal_subset <- test_data
    } else {
      # cat("KEEP", cx, optimal_dri, "\n")
      test_data <- optimal_subset
      # keep <- c(keep, cx)
    }
    
  }
  
  return(optimal_subset)
  
}

