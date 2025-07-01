#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_analysis.R  
#' @description R script containing all functions relative to data analysis
#' @author Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to fit the models for the first analysis at plot level
#' @param data_explanatory dataframe with all explanatory variables at plot-level
#' @param data_services dataframe with all services data
#' @param service_table distribution and title of each service
fit_models_plot = function(data_explanatory, data_services, service_table){
  
  # Change distribution of Carbon stock
  service_table = service_table %>%
    mutate(distrib = ifelse(service == "Cstock_t.ha", "pos0", distrib))
  
  # Identify your explanatory and response variables
  explanatory_vars <- c("ba.tot", "dqm", "str.div", "shannon", "elev", "pca_clim")
  response_vars <- service_table$service
  
  # Add square to the explanatory variables
  data_explanatory = data_explanatory %>%
    filter(ba.tot < 110)
  for(i in 1:length(explanatory_vars)) eval(parse(text = paste0(
    "data_explanatory$", explanatory_vars[i], "2 = data_explanatory$", 
    explanatory_vars[i], "^2")))
  # explanatory_vars = c(explanatory_vars, paste0(explanatory_vars, "2"))
  
  # First, join the two datasets
  combined_data <- inner_join(data_explanatory, data_services, by = "IDP") %>%
    mutate(across(all_of(explanatory_vars), ~ as.numeric(scale(., center = TRUE, scale = TRUE))))
  
  
  # Format response distributions dataset
  response_distributions = service_table %>% select(
    response_var = service, distribution = distrib) %>%
    as_tibble()
  
  # Function to fit glm with interactions and quadratic terms
  fit_glm <- function(response_var, data, explanatory_vars, distribution) {
    
    print(paste0("Fitting models for ", response_var))
    
    # Create initial formula with main effects
    formula_main <- as.formula(paste(response_var, "~", paste(
      c(explanatory_vars, paste0(explanatory_vars, "2")), collapse = " + ")))
    
    # Create all possible two-way interactions between main variables
    if(length(explanatory_vars) > 1) {
      interactions <- combn(explanatory_vars, 2, simplify = FALSE)
      interaction_terms <- sapply(interactions, function(x) paste(x, collapse = ":"))
      formula_full <- as.formula(paste(response_var, "~", paste(c(
        explanatory_vars, paste0(explanatory_vars, "2"), interaction_terms), collapse = " + ")))
    } else {
      formula_full <- formula_main
    }
    
    # Fit initial full model based on distribution
    if (distribution == "pos") {
      model <- glm(formula_full, data = data, family = gaussian(link = "log"))
    } else if (distribution == "beta") {
      model <- glm(formula_full, data = data, family = quasibinomial(link = "logit"))
    } else if (distribution == "norm") {
      model <- lm(formula_full, data = data)
    } else if (distribution == "pos0") {
      model <- glm(formula_full, data = data, 
                   family = tweedie(var.power = 1, link.power = 0))
    }
    
    # Perform model selection based on family
    if(length(explanatory_vars) > 1) {
      if(distribution %in% c("beta", "pos0")) {
        # For quasibinomial, use significance-based backward selection
        model <- backward_selection_quasi(model, data, explanatory_vars)
      } else {
        # For other families, use AIC-based backward selection
        model <- step(model, direction = "backward", trace = 0)
      }
    }
    
    return(model)
  }
  
  # Helper function for quasibinomial backward selection
  backward_selection_quasi <- function(model, data, explanatory_vars) {
    current_model <- model
    changed <- TRUE
    
    while(changed) {
      changed <- FALSE
      terms <- attr(terms(current_model), "term.labels")
      
      # Don't remove main effects, only interactions and squared terme
      eligible_for_removal <- setdiff(terms, explanatory_vars)
      
      # Check each eligible term
      for(term in eligible_for_removal) {
        reduced_formula <- as.formula(
          paste(". ~ . -", term)
        )
        reduced_model <- update(current_model, reduced_formula)
        
        # Compare models using F-test (appropriate for quasibinomial)
        test_result <- anova(current_model, reduced_model, test = "F")
        
        if(test_result$`Pr(>F)`[2] > 0.05) {  # If p-value > 0.05, term can be removed
          current_model <- reduced_model
          changed <- TRUE
          break  # Restart the process after removing a term
        }
      }
    }
    
    return(current_model)
  }
  
  # Function to generate diagnostics
  generate_diagnostics <- function(model, response_var, distribution) {
    print(paste0("Generating diagnostics for model of ", response_var))
    # Get residuals and fitted values
    if (distribution == "pos0") {
      # Tweedie requires special handling
      residuals <- residuals(model, type = "pearson")
      fitted <- fitted(model)
      sqrt_abs_resid <- sqrt(abs(residuals))
    } else {
      residuals <- residuals(model, type = "deviance")
      fitted <- fitted(model)
      sqrt_abs_resid <- sqrt(abs(residuals))
    }
    
    diag_data <- tibble(
      fitted = fitted,
      residuals = residuals,
      sqrt_abs_resid = sqrt_abs_resid
    )
    
    # Residuals vs Fitted
    print("---plot residual vs fitted")
    p1 <- ggplot(diag_data, aes(fitted, residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, color = "blue") +
      labs(title = "Residuals vs Fitted", 
           subtitle = paste("Distribution:", distribution),
           x = "Fitted Values", y = "Residuals")
    
    # Q-Q Plot
    print("---qqplot")
    p2 <- ggplot(diag_data, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
    
    # Scale-Location
    print("---scale-location")
    p3 <- ggplot(diag_data, aes(fitted, sqrt_abs_resid)) +
      geom_point(alpha = 0.6) +
      geom_smooth(se = FALSE, color = "blue") +
      labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
    
    # Combine plots
    (p1 | p2 | p3) + 
      plot_annotation(title = paste("Diagnostics for:", response_var))
  }
  
  # Initialize list of outputs
  model.list = vector(mode = "list", length = length(response_vars))
  names(model.list) = response_vars
  diag.list = model.list; interval.list = model.list
  
  # Run models 
  for(i in 1:length(model.list)){
    model.list[[i]] = fit_glm(
      response_vars[i], combined_data, explanatory_vars, service_table$distrib[i])
    diag.list[[i]] = generate_diagnostics(
      model.list[[i]], response_vars[i], service_table$distrib[i])
    interval.list[[i]] = confint(model.list[[i]])
  } 
  
  # Put all together in a single list
  list.out = list(models = model.list, diag = diag.list, interval = interval.list)
  
  # return list
  return(list.out)
  
  
}