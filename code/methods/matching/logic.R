# Author: Henry Chen, then Megan Ayers
# Date: 9/1/2025
# Function: Function to apply matching methodology to a single project.

if (interactive()) {
  library(MatchIt)
  library(estimatr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
}


### EXAMPLE:
# project_name <- "adpml"
# start_year <- 9
# params <- list(method = "nearest", distance = "logit", ratio = 1)
execute_method <- function(project_name, start_year, params, n_cores = 1) {
  
  if (!all(c("method", "distance", "ratio") %in% names(params))) {
    stop("Matching parameters are incorrectly specified.")
  }
  
  if (is.null(params$std.caliper)) {
    params$std.caliper <- TRUE
  }
  
  # Load the preprocessed data.
  load(paste0("data/processed/", project_name, "/dat_matching.Rdata"))
  start_year <- as.numeric(start_year)
  
  # Add a new column for treecover in the year right before start date.
  dat_long <- dat_long %>% 
    filter(year == start_year - 1) %>% 
    select(ID, treecover_remaining) %>%
    rename(treecover_past = treecover_remaining) %>% 
    right_join(dat_long, by = "ID")
  
  # Filter to the year right before start date for matching (these properties
  # should be static).
  dat_long_past <- dat_long %>% filter(year == start_year - 1)

  
  # Perform matching according to params.
  form <- as.formula(paste("treated ~", paste(params$covariates[[1]],
                                              collapse = " + ")))
  m_out <- matchit(formula = form, data = dat_long_past, method = params$method, 
                   distance = params$distance, ratio = params$ratio,
                   pop.size = params$pop.size, caliper = params$caliper[[1]],
                   std.caliper = params$std.caliper, replace = TRUE)
  m_data <- match.data(m_out)
  
  
  # If calipers are on and less than 90% of treated pixels were successfully
  # matched, do not return results for this run (Guizar-Coutino 2022.
  flag <- sum(m_out$weights == 0 & dat_long_past$treated == 1) / nrow(dat_long_past)
  if (flag > 0.1 & !is.null(params$caliper)) {
    ates_by_year <- data.frame(year = list((start_year - 3) : 22),
                               coef = NA, lower = NA, upper = NA)
    return(ates_by_year)
  }
  
  # Attach weights back to data set, only include points that were actually
  # matched. Use dat_long going forward as it now contains everything needed.
  m_data <- m_data %>% select(ID, weights)
  dat_long <- dat_long %>%
    left_join(m_data, by = "ID") %>% drop_na(weights)
  
  
  # Calculate the ATE for all the years from 3 years before project start year
  # to 2022.
  ates_by_year <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
                           c("year", "coef", "lower", "upper"))
  years <- list((start_year - 3) : 22) 
  
  for (i in years[[1]]) {
    
    # Filter to the current year.
    dat_i <- dat_long %>% filter(year == i)
    
    # Calculate the ATE for the current year using the same matching output.
    ate_i <- lm_robust(treecover_remaining ~ treated, data = dat_i,
                       weights = dat_i$weights)
    
    # Record the coefficient and confidence interval.
    coef <- coef(ate_i)[2]
    upper <- coef(summary(ate_i))[2, 6]
    lower <- coef(summary(ate_i))[2, 5]

    add <- data.frame(i, coef, lower, upper) %>%
      setNames(c("year", "coef", "lower", "upper"))
    ates_by_year <- bind_rows(ates_by_year, add)
  }
  
  
  # Plot the ATE over time.
  # ggplot(ates_by_year, aes(x = year, y = coef)) +
  #   geom_point() +
  #   geom_line() +
  #   geom_vline(aes(xintercept = start_year), color = "red", linetype = "dashed",
  #              linewidth = 0.7) +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
  #               fill = "forestgreen") +
  #   labs(x = "Years since 2000", y = "ATE (treecover remaining)",
  #        title = paste0("ATEs by Year for ", project_name, " project")) +
  #   theme_bw()
  
  # Return the full data frame with results.
  return(ates_by_year)
  
}

