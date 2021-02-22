# this app is a minimal example of uploading a layout file using the layout uploads module
library(tidyverse)
library(shiny)
library(shinyalert) # needed to upload layout files
library(shinycssloaders) # needed for the spinner on the layout plots
library(varhandle) 
library(shinyWidgets)
library(glue)
source("upload_layout_module.R")
source("layout_color_plot_module.R")
source("layout_filter_and_select_module.R")

##### working on the modules
filterlayoutUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        uiOutput(ns("picker")),
        verbatimTextOutput(ns("selection"))
    )
}

# should return something that can be used to filter the layout outside of the module
filterlayoutServer <- function(id, layout, text_name) {

    stopifnot(is.reactive(layout)) # layout should be reactive
    
    moduleServer(id, function(input, output, session) {
        lower_case_name <- tolower(text_name)
        
        observeEvent(layout(), {
            
            layout_cont <- reactive(contract_layout(layout()))
            filtered_layout <- reactive(layout())
            picker_list_raw <-  reactive(layout_to_picker_list(layout_cont()))
            
            output$picker <- renderUI({ # called in UI module
                
                pickerInput(
                    # THANK YOU for renderUI in modules https://gist.github.com/tbadams45/38f1f56e0f2d7ced3507ef11b0a2fdce 
                   inputId =  session$ns("take_these"), 
                    choices = picker_list_raw(),
                   selected = picker_list_raw(),
                    multiple = TRUE,
                    options = pickerOptions(
                        actionsBox = TRUE,
                        selectAllText = glue("{text_name} all"),
                        deselectAllText = glue("{text_name} none"),
                        title = glue("Select data to {lower_case_name}"),
                        tickIcon = "glyphicon-eye-open",
                        selectedTextFormat= "count",
                        countSelectedText = "{0} selections"
                    )
                )
            })
            
          output$selection <- renderPrint({picked_to_layout(layout(), input$take_these)})
        })
        
        reactive(picked_to_layout(layout(), input$take_these))
    })
}

ui <- fluidPage(useShinyalert(),
                titlePanel("Filter layouts module"),
                
                sidebarLayout(
                    sidebarPanel(
                        filterlayoutUI("filter_layout")[[1]]
                    ),
                    mainPanel(
                        p("accessed externally"),
                      verbatimTextOutput("selection_external"),
                      p("accessed internal to module"),
                        filterlayoutUI("filter_layout")[[2]]
                    )
                )
)


server <- function(input, output, session) {
    ### layout munching
    layout_ex <- reactive(tibble(well = c("A1", "A2", "A3", "A4"),
                                 dye = c("A001", "A001", "A001", "A004"),
                                 dye_conc = c(1, 2, 3, 4)))
    
    filter_list <- filterlayoutServer("filter_layout", layout_ex, text_name = "Plot")

    output$selection_external <- renderPrint(filter_list())

}

shinyApp(ui = ui, server = server)
