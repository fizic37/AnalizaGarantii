#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  sidebar_selected <- c()
  
  vals <- reactiveValues( sidebar_selected = sidebar_selected)
  
  mod_sidebar_server("sidebar_ui_1", vals = vals)
  
  mod_crc_new_server("crc_new_1")
  
}
