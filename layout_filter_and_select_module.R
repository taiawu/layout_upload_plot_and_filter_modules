# library(tidyverse)
# library(shinyWidgets) # has pickerInput
# library(glue) # required for custom names within the selectors

contract_layout <- function(layout) {
  layout %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(-well, names_to = "variable", values_to = "value") %>%
    unite(picker_val, c(variable, value), sep  = "<LAYOUT_GROUP>", remove = FALSE)
}

expand_layout <- function(layout_cont) {
  layout_cont %>% 
    select(-picker_val) %>%
    pivot_wider(id_cols = well, names_from = variable, values_from = value)
  
}

layout_to_picker_list <- function(layout_cont) {
  
  layout_exp <- expand_layout(layout_cont)
  
  layout_names <- layout_exp %>% 
    as.list() %>% 
    lapply(unique)
  
  layout_vals <- map2(layout_names, names(layout_names), paste, sep = "<LAYOUT_GROUP>")
  
  layout_list <- map2(layout_vals, layout_names, set_names)  %>%
    .[order(sapply(., length))]
  
}

picked_to_layout <- function(layout, picked_vec) {
  
  layout %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = everything(), names_to = "group", values_to = "value") %>%
    unite(picker_val, c(value, group), sep  = "<LAYOUT_GROUP>", remove = FALSE)  %>%
    filter(picker_val %in% picked_vec) 
  
}