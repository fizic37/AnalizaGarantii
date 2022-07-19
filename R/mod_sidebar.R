#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  
  tagList(br(),
          #verbatimTextOutput(ns("diverse")),
  bs4Dash::sidebarMenuOutput(outputId = ns("sidebar"))
  )
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id, vals) {
  
  moduleServer(id, function(input, output, session) {
    tagList( 
    
     risk_user <- bs4Dash::sidebarMenu(
      id = session$ns("tabs"),
      bs4Dash::menuItem(
        tabName = "crc",
        text = "CRC",
        icon = icon("home"),
        selected = TRUE )
      
     )
    )
        
     
  
  output$sidebar <- bs4Dash::renderMenu(risk_user)
  
  #output$diverse <- renderPrint(sum("crc" == vals$sidebar_selected)==1)
 
  observeEvent(input$tabs,{ 
    # I use this in order to have a selection of all inputs in sidebar. This way, I don`t have to call modules
    # every time a sidebar is selected, I only call modules ones.`
    vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)  })
    
  } )  
   
  
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_ui_1")
