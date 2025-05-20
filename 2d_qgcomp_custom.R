
qgcomp.test.eval <- function(object, test_data, observation_wt_col, cluster_col){
  
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
  
  ## Check if observation_wt_col is a non-empty string of length 1
  if (!is.character(observation_wt_col) || length(observation_wt_col) != 1 || observation_wt_col == "") {
    stop("observation_wt_col must be a non-empty character vector of length 1.")
  }
  
  ## Check if cluster_col is a non-empty string of length 1
  if (!is.character(cluster_col) || length(cluster_col) != 1 || cluster_col == "") {
    stop("cluster_col must be a non-empty character vector of length 1.")
  }
  
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
  
  ## Model lm
  model_test <- lm(data = test_data,formula = formula_str, weights = get(observation_wt_col))
  
  ## Cluster Robust
  cluster_var <- test_data[[cluster_col]]
  robust_results <- clubSandwich::coef_test(model_test, 
                                            vcov = "CR2", 
                                            cluster = cluster_var,
                                            test = "Satterthwaite", 
                                            coefs = "test_target_mix")
  
  
  ## extract robust beta coefficient, p and SE for test_target_mix
  mix_coeff <- robust_results$beta
  mix_p_val <- robust_results$p_Satt
  mix_se <- robust_results$SE
  mix_lower_ci <- mix_coeff - qnorm(0.975) * robust_results$SE
  mix_upper_ci <- mix_coeff + qnorm(0.975) * robust_results$SE
  
  df <- data.frame(
    beta = mix_coeff,
    p_val = mix_p_val,
    std_error = mix_se,
    lower_ci = mix_lower_ci,
    upper_ci = mix_upper_ci
  )
  
  
  return(df)
  
}

