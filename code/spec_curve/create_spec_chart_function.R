# Author: Albert
# Date: Fall 2023
# Purpose: 

if (interactive()) {
  library(here)
  library(stringr)
}


# load base schart function
source(here::here("code", "spec_curve", "schart_ortiz.R"))

create_spec_chart <- function(project_name, results, spec_order = "asis",
                              color = "black", leftmargin = 7) {
  
  results <- results[, names(results) != "project_name"]
  label_colnames <- colnames(results %>% select(-c(ATT, lower, upper)))
  
  
  for (col in label_colnames) {
    
    # Handle NAs.
    if (anyNA(results[, col])) {
      results[, col] <- as.character(results[, col])
      results[, col] <- ifelse(is.na(results[, col]), "NA",
                               results[, col])
    }
    
    # Handle vector-valued parameters
    if (class(results[, col]) == "list") {
      results[, col] <- sapply(1:nrow(results), function(i) {
        if (is.null(results[i, col][[1]])) {
          "none"
        } else {
          paste(results[i, col][[1]], collapse = ", ")
        }})
      # These might be too long to display comfortably - if so just call them
      # set 1, set 2, ..., set n. 
      if (any(str_count(unique(results[, col]), ".") > 20)) {
        these <- results[, col] != "none"
        results[these, col] <- paste("set",
                                     as.numeric(factor(results[these, col])))
      }
    }
  }
  
  
  these_results <- results %>% distinct %>%
    rowid_to_column("ID")
  
  labels <- c()
  for(col in label_colnames){
    labelname <- col
    
    labels <- c(labels,
                unique(results[col])
    )
    
    these_results <- these_results %>%
      mutate("TRUE" = TRUE) %>% 
      pivot_wider(names_from = col, values_from = "TRUE", values_fill = FALSE,
                  names_repair = "unique")
    
  }
  
  schart_results <- these_results %>% as.data.frame() %>%
    select(ATT, everything(), -ID)
  
  index.ci <- match(c("upper","lower"), names(schart_results))
  
  par(oma=c(1,0,1,1))
  
  schart(schart_results, 
         labels = labels, 
         # highlight = 2,
         #ylim = ylim, 
         axes = FALSE, 
         index.ci=index.ci,
         ylab="ATE",
         leftmargin = leftmargin,
         order=spec_order,
         col.est=c(color,"royalblue"), 
         col.dot=c(color,"grey95","grey95","royalblue"),
         bg.dot=c(color,"grey95","grey95","white")
  )
  # print(project_name) # in format of (project_name, start_year)
  text(x=mean(1:nrow(schart_results)), y=max(schart_results$upper), project_name[1], col="black", font=2)
  
}

