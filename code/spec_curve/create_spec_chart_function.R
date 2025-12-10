# Author: Albert
# Date: Fall 2023
# Purpose: 

if (interactive()) {
  library(stringr)
}


# load base schart function
source("code/spec_curve/schart_ortiz.R")

create_spec_chart <- function(project_name, results, spec_order = "asis",
                              color = "black", leftmargin = 7,
                              highlight = NULL, ylabel = "") {
  
  results <- results[, names(results) != "project_name"]
  label_colnames <- colnames(results %>% select(-c(ATT, lower, upper)))
  ylim <- c(min(0, max(min(2*results$ATT), min(results$lower))),
            max(0, min(max(2*results$ATT), max(results$upper))))
  
  # Handle boolean columns
  bool_cols <- which(sapply(1:ncol(results), function(i) class(results[1, i])) == "logical")
  for (k in bool_cols) {
    results[, k] <- ifelse(results[, k], "Yes",
                    ifelse(!results[, k], "No", "NA"))
  }
  
  for (col in label_colnames) {
    
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
    
    results[, col] <- as.character(results[, col])

  }
  
  # Keep record of which rows to highight before reordering.
  if (!is.null(highlight)) {
    results$highlight <- FALSE
    results$highlight[highlight] <- TRUE
  }
  
  results <- results %>%  # Sort labels
    arrange(across(all_of(label_colnames)))
  
  # Handle NAs.
  for (col in label_colnames) {
    if (anyNA(results[, col])) {
      results[, col] <- ifelse(is.na(results[, col]), "NA",
                               results[, col])
    }
  }
  
  
  
  these_results <- results %>% distinct %>%
    rowid_to_column("ID")
  
  labels <- c()
  for (col in label_colnames) {
    labelname <- col
    
    labels <- c(labels, unique(results[col]))
    
    these_results <- these_results %>%
      mutate("TRUE" = TRUE) %>% 
      pivot_wider(names_from = col, values_from = "TRUE", values_fill = FALSE,
                  names_repair = "unique")
    
  }
  
  schart_results <- these_results %>% as.data.frame() %>%
    select(ATT, everything(), -ID, -highlight)
  
  index.ci <- match(c("upper","lower"), names(schart_results))
  
  highlight <- if (!is.null(highlight)) which(these_results$highlight) else NULL
  
  if (ylabel != "") {
    ylabel <- paste0("ATE (", ylabel, ")")
  } else ylabel <- "ATE"
  
  par(oma=c(1,0,1,1))
  
  schart(schart_results, 
         labels = labels, 
         highlight = highlight,
         #ylim = ylim, 
         axes = FALSE, 
         index.ci=index.ci,
         ylim = ylim,
         ylab=ylabel,
         leftmargin = leftmargin,
         order=spec_order,
         col.est=c(color,"magenta3"), 
         col.dot=c(color,"grey95","grey95","magenta3"),
         bg.dot=c(color,"grey95","grey95","magenta3"),
         pch.dot=c(22,22,22,22)
  )
  # print(project_name) # in format of (project_name, start_year)
  text(x=mean(1:nrow(schart_results)), y=max(schart_results$upper), project_name[1], col="black", font=2)
  
}

