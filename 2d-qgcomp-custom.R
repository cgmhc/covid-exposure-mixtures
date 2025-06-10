# Function to evaluate qgcomp mixture score on new data -------------------------
qgcomp.model.mixture <- function(object, 
                             data_eval, 
                             observation_wt_col,
                             exposure_names){
  
  ## Check class
  if (inherits(object, "qgcompfit")) {
    message("This is a qgcomp object.")
  } else {
    stop("Pass a valid qgcomp object.")
  }
  
  ## Check if data_eval is a data.frame
  if (!is.data.frame(data_eval)) {
    stop("data_eval must be a data.frame.")
  }
  
  ## Check if observation_wt_col is a non-empty string of length 1
  if (!is.character(observation_wt_col) || length(observation_wt_col) != 1 || observation_wt_col == "") {
    stop("observation_wt_col must be a non-empty character vector of length 1.")
  }
  
  ## Extract weights
  wts <- c(object$pos.weights, object$neg.weights)
  
  ## Multiply exposure training weights with exposure columns
  data_eval$mixture_score <- rowSums(wts*data_eval[, names(wts), drop = FALSE])
  
  print("Summary of test mixture score column")
  print(summary(data_eval$mixture_score))
  
  # Evaluate effect
  ## pull formula from qgcomp
  lhs <- as.character(object$fit$formula)[2]
  rhs <- as.character(object$fit$formula)[3]
  
  ## Clean rhs string
  rhs_clean <- gsub("[\n\r]", " ", rhs)       # Replace line breaks with space
  rhs_clean <- gsub("\\s+", " ", rhs_clean) # Collapse multiple spaces
  rhs_clean
  
  ## Remove exposure terms: to avoid multicollinearity
  rhs_terms <- strsplit(rhs_clean, " \\+ ")[[1]]
  
  rhs_new <- if (length(rhs_terms[!rhs_terms %in% exposure_names]) > 0) {
    paste0(" + ", paste(rhs_terms[!rhs_terms %in% exposure_names], collapse = " + "))
  } else {
    ""
  }
  
  formula_str <- paste0(lhs, "~", "mixture_score" , "+" , rhs_new)
  formula_str
  
  ## Model glm
  # model_test <- lm(data = test_data,formula = formula_str, weights = (observation_wt_col))
  model_mix <- eval(substitute(glm(formula = formula_str, family = "gaussian", data = data_eval, weights = W), list(W = as.name(observation_wt_col))))
  model_mix
  
  # Return
  ## return object
  eval_object <- list()
  eval_object$model <- copy(model_mix)
  eval_object$new_data <- copy(data_eval)
  
  return(eval_object)
  
}

# Function to check Heteroskedasticity and Intra-Class Correlation Coefficient ------------------
qgcomp.hicc <- function(fit.object, 
                        data = NULL,
                        cluster_col = NULL){
  
  ## Check class
  if (inherits(fit.object, "lm")) {
    message("This is a lm object.")
  } else {
    stop("Pass a valid lm object.")
  }
  
  ## Check data
  if (is.null(data)){
    stop("Pass va valid data object")
  }
  
  ## Check if cluster_var is a non-empty string of length 1
  if (length(cluster_col) == 0 || is.null(cluster_col) || cluster_col == ""){
    stop("cluster_col must be non-empty vector of length 1")
  }
  
  
  # Heteroskedasticity test: if pval is significant, need HC correction (Robust SE)
  hc_result <- lmtest::bptest(fit.object) 
  
  # ICC: explains cluster level effects: should be higher than 0.05 (Cluster Robust SE)
  model_formula <- formula(fit.object)
  lmer_model <- eval(
    substitute(
      lme4::lmer(update(model_formula, . ~ . + (1 | CLUSTER)), data = data),
      list(CLUSTER = as.name(cluster_col))
    )
  )
  
  # Check for singular model
  if(lme4::isSingular(lmer_model, tol = 1e-4)){
    message("Model is singular. setting ICC scores to 0")
    
    df <- data.frame(
      hc_p_val = hc_result$p.value,
      icc = 0,
      icc_adj = 0,
      row.names = NULL
    )
    
  } else {
    
    icc_result <- performance::icc(lmer_model)
    
    df <- data.frame(
      hc_p_val = hc_result$p.value,
      icc = icc_result$ICC_unadjusted,
      icc_adj = icc_result$ICC_adjusted,
      row.names = NULL
    )
    
  }
  
  return(df)
}


# Function to get Robust estimates for qgcomp ------------------
qgcomp.robust.est <- function(fit.object,
                             cluster_robust = FALSE, 
                             robust_type = "HC1", 
                             cluster_var = NULL, 
                             exposure_names = NULL){
  
  
  ## Check class
  if (inherits(fit.object, "lm")) {
    message("This is a lm object.")
  } else {
    stop("Pass a valid lm object.")
  }
  
  ## Check if robust_type is a non-empty string of length 1
  if (length(robust_type) == 0 || is.null(robust_type)) {
    stop("robust_type must be a non-empty vector")
  }
  
  ## Init types
  HC_types <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
  CR_types <- c("CR0", "CR1", "CR1S", "CR2", "CR3")
  
  ## Check if robust_type in HC types for cluster_robust = FALSE
  if ((cluster_robust == FALSE) & (robust_type %in% CR_types)) {
    stop("Set cluster_robust to TRUE before proceeding with CR types")
  }
  
  ## Check if robust_type is accurate
  if (!(robust_type %in% c(HC_types, CR_types))) {
    stop("Invalid robust_type. Must be one of: ", paste(c(HC_types, CR_types), collapse = ", "))
  }
  
  ## Check if cluster_var is non-empty
  if ((cluster_robust == TRUE) & (length(cluster_var) == 0 || is.null(cluster_var))) {
    stop("cluster_var must be a non-empty vector when cluster_robust = TRUE")
  }
  
  
  if(!cluster_robust){ # Robust Estimation
      message("Using vcovHC + manual t-test(df = df.residual))")
      hc_vcov <- sandwich::vcovHC(fit.object, type = robust_type)
      
      ## Clean: required due to high memory usage
      gc()
      
      ## Extract robust beta coefficient, p and SE
      new_coeff <- sum(fit.object$coefficients[exposure_names])     # same as qgcomp object$psi
      new_se <- sqrt(sum(hc_vcov[exposure_names, exposure_names]))  # square root of covariance
      new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get tstat by coeff/SE
                             df = fit.object$df.residual,           # degree of freedom is residual degree of freedom from model object
                             lower.tail = FALSE)
      new_lower_ci <- new_coeff - qt(0.975, df = fit.object$df.residual) * new_se
      new_upper_ci <- new_coeff + qt(0.975, df = fit.object$df.residual) * new_se
  } else {
    if(robust_type %in% HC_types){ # Cluster Robust Estimation
      
      message("Using vcovCL + manual t-test (df = G - 1)")
      cl_vcov <- sandwich::vcovCL(fit.object, cluster = cluster_var, type = robust_type)
      
      ## Clean: required due to high memory usage
      gc()
      
      ### Extract robust beta coefficient, p and SE
      new_coeff <- sum(fit.object$coefficients[exposure_names])     # same as qgcomp object$psi
      new_se <- sqrt(sum(cl_vcov[exposure_names, exposure_names, 
                                 drop = FALSE]))                    # square root of covariance
      new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                             df = length(unique(cluster_var)) - 1,  # degree of freedom is number of clusters - 1
                             lower.tail = FALSE)
      new_lower_ci <- new_coeff - qt(0.975, df = length(unique(cluster_var)) - 1 ) * new_se
      new_upper_ci <- new_coeff + qt(0.975, df = length(unique(cluster_var)) - 1) * new_se
      
    } else if (robust_type %in% CR_types){ # Cluster Robust Estimation for CR
      message("Using vcovCR + manual t-test")
      cl_vcov <- clubSandwich::vcovCR(fit.object, cluster = cluster_var, type = robust_type)
      
      ## Clean: required due to high memory usage
      gc()
      
      ## Extract robust beta coefficient, p and SE
      new_coeff <- sum(fit.object$coefficients[exposure_names])     # same as qgcomp object$psi
      new_se <- sqrt(sum(cl_vcov[exposure_names, exposure_names, 
                                 drop = FALSE]))                    # square root of covariance
      
      ## Satterthwaite calc
      which_beta <- names(coef(fit.object)) %in% exposure_names
      GH_obj <- clubSandwich:::get_GH(fit.object, cl_vcov)          # NOTE: using ::: to access internal function
      P_array <- clubSandwich:::get_P_array(GH_obj)                 # NOTE: using ::: to access internal function
      
      ## Get projection
      P_combined <- array(0, dim = c(dim(P_array)[1:2], 1))
      exposure_indices <- which(names(coef(fit.object)) %in% exposure_names)
      for (g in seq_len(dim(P_array)[1])) {
        for (h in seq_len(dim(P_array)[2])) {
          P_combined[g, h, 1] <- sum(P_array[g, h, exposure_indices])
        }
      }
      
      ## Get Satterthwaite df for the combined effect
      df_satt <- clubSandwich:::Satterthwaite_df(P_combined)  # NOTE: using ::: to access internal function
      
      ## Clean: required due to high memory usage
      gc()
      
      ## Continue with p val calculations
      new_p_val <- 2 * pt(abs(new_coeff/new_se),                    # get stat by coeff/SE
                          df = df_satt,                             # degree of freedom is Satterthwaite df
                          lower.tail = FALSE)
      new_lower_ci <- new_coeff - qt(0.975, df = df_satt) * new_se
      new_upper_ci <- new_coeff + qt(0.975, df = df_satt) * new_se
    }
  } 
    
  # Output df
  df <- data.frame(
    beta = new_coeff,
    p_val = new_p_val,
    std_error = new_se,
    lower_ci = new_lower_ci,
    upper_ci = new_upper_ci,
    row.names = NULL
  )

    
  # Return
  return(df)
  
}