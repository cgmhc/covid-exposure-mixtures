
# Function to evaluate qgcomp on test data

qgcomp.test.eval <- function(object, test_data, observation_wt_col){
  
  ## Check class
  if (inherits(object, "qgcompfit")) {
    message("This is a qgcomp object.")
  } else {
    stop("Pass a valid qgcomp object.")
  }
  
  ## Check if test_data is a data.frame
  if (!is.data.frame(test_data)) {
    stop("test_data must be a data.frame.")
  }
  
  # ## Check if observation_wt_col is a non-empty string of length 1
  # if (!is.character(observation_wt_col) || length(observation_wt_col) != 1 || observation_wt_col == "") {
  #   stop("observation_wt_col must be a non-empty character vector of length 1.")
  # }
  
  ## Extract weights
  training_model_wts <- c(object$pos.weights, object$neg.weights)
  
  ## Multiply exposure training weights with exposure columns
  test_data$test_target_mix <- rowSums(training_model_wts*test_data[, names(training_model_wts)])
  
  print("Summary of derived test mixture weight column")
  summary(test_data$test_target_mix)
  
  # Evaluate effect
  ## pull formula from qgcomp
  lhs <- as.character(object$fit$formula)[2]
  rhs <- as.character(object$fit$formula)[3]
  formula_str <- paste0(lhs, "~", "test_target_mix" , "+" , rhs)
  formula_str
  
  length(test_data$test_target_mix)
  length(test_data)
  
  ## Model lm
  model_test <- lm(data = test_data,formula = formula_str, weights = observation_wt)
  
  ## extract beta coefficient and SE for test_target_mix
  mix_coeff <- unname(model_test$coefficients["test_target_mix"])
  
  ## Cluster robust adjustment for SE
  mix_se <-  broom::tidy(model_test)[[2, "std.error"]]
  
  ## Create dataframe to return
  df <- data.frame(
    beta = mix_coeff,
    std_error = mix_se
  )
  
  df$lower_ci <- df$beta - qnorm(0.975) * df$std_error
  df$upper_ci <- df$beta + qnorm(0.975) * df$std_error
  
  return(df)
  
}

