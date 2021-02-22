# library(tidyverse)
# library(shinyWidgets) # has pickerInput
# library(glue) # required for custom names within the selectors

contract_layout <- function(layout) {
  layout %>%
    mutate(across(everything(), as.character)) %>%
    mutate(well_id = well) %>%
    pivot_longer(-well_id, names_to = "variable", values_to = "value") %>%
    unite(picker_val, c(value, variable), sep  = "<LAYOUT_GROUP>", remove = FALSE)
}

expand_layout <- function(layout_cont) {
  layout_cont %>% 
    select(-picker_val) %>%
    pivot_wider(id_cols = well_id, names_from = variable, values_from = value) %>%
    select(-well_id)
  
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

picked_to_layout <- function(layout_raw, picked_vec) {
  
  layout <- layout_raw %>% 
            mutate(across(everything(), as.character)) 
  
  tryCatch({ 
    
    layout_c_picked <- contract_layout(layout) %>%
      filter(picker_val %in% picked_vec) %>%
      expand_layout()
    
    layout %>%
      right_join(., layout_c_picked) %>%
      drop_na() %>%
      distinct() 
    
  }, error = function(e) { 
    return(layout[1,] %>% mutate(across(where(is.character), ~NA_character_)) )
  })
  
}