# Author: Albert
# Date: Fall 2023
# Purpose: 

library(here)


# load base schart function
source(here::here("code", "Spec_Curve", "schart_ortiz.R"))

create_spec_chart <- function(project_name, results, spec_order = "asis", color = "black") {
  
  label_colnames <- colnames(results %>% select(-c(ATT, lower, upper)))
  
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
      )
    
  }
  
  schart_results <- these_results %>% as.data.frame()%>%
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
         leftmargin = 5,
         order=spec_order,
         col.est=c(color,"royalblue"), 
         col.dot=c(color,"grey95","grey95","royalblue"),
         bg.dot=c(color,"grey95","grey95","white")
  )
  print(project_name) # in format of (project_name, start_year)
  text(x=mean(1:nrow(schart_results)), y=max(schart_results$upper), project_name[1], col="black", font=2)
  
}

