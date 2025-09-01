# Author: Henry Chen
# Date: 7/25/2023
# Function: applies the matching function from functions.R and matching_function.R to all projects

library(dplyr)
library(here)

source(here("code", "methods", "matching", "matching_processing.R"))

# applying the matching processing function (i.e. create the Rdata files)
process_matching_data("adpml")
process_matching_data("agrocortex")
process_matching_data("florestal")
process_matching_data("jari")
process_matching_data("maisa")
process_matching_data("manoa")
process_matching_data("purus")
process_matching_data("riopreto")
process_matching_data("rmdlt")
process_matching_data("russas")
process_matching_data("surui")
process_matching_data("valparaiso")


source(here("code", "methods", "matching", "matching_logic.R"))

# apply the match and ATE plot procedures (project_name, start_year - 2000)

# match_data("adpml", 9)
# match_data("agrocortex", 14)
# match_data("florestal", 9)
# match_data("jari", 11)
# match_data("maisa", 12)
# match_data("manoa", 13)
# match_data("purus", 11)
# match_data("riopreto", 12)
# match_data("rmdlt", 8)
# match_data("russas", 11)
# match_data("surui", 9)
# match_data("valparaiso", 11)


### Specification curve for ADPML

# List of combinations to try
methods <- c("nearest", 
             # "genetic",
             "cem"
)
distances <- c("logit", "mahalanobis", "euclidean")
ratios <- c(1,3,5)

# Dataframe to store results
results <- data.frame(
  method = character(),
  distance = character(),
  ratio = numeric(),
  ATT = numeric(),
  lower = numeric(),
  upper = numeric()
)

# Iterate through combinations
for (method in methods) {
  for (distance in distances) {
    for (ratio in ratios) {
      # Call match_data function with a given combination
      print(c(method, distance, ratio))
      ates_by_year <- match_data(project_name = "adpml", start_year = 9, method = method, distance = distance, ratio = ratio)
      
      print(ates_by_year)
      
      # Retrieve the ATT, lower and upper CI bounds for the year 2022
      result_2022 <- ates_by_year %>% filter(year == 22)
      
      # Add the results to the dataframe
      results <- rbind(results, data.frame(
        method = method,
        distance = distance,
        ratio = ratio,
        ATT = result_2022$coef,
        lower = result_2022$lower,
        upper = result_2022$upper
      ))
    }
  }
}

# Print or return results
results

# Create a specification curve plot
library(ggplot2)

ggplot(results, aes(x = interaction(method, distance, ratio), y = ATT, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # coord_flip() + # Rotate the plot for better readability
  labs(
    x = "Combinations of Method, Distance, and Ratio",
    y = "Average Treatment Effect (ATT)",
    title = "Specification Curve Plot for Matching Specifications"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# load matching spec curve function
source(here("code", "spec_curve", "create_spec_chart_function.R"))

# return matching specification curve
create_spec_chart(project_name = "adpml", results, spec_order = "increasing", color = "royalblue")

