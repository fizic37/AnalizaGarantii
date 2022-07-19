#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bs4Dash::dashboardPage(skin = "blue", dark = FALSE,  title = "Analiza Garantii",
                           sidebar = bs4Dash::dashboardSidebar( mod_sidebar_ui("sidebar_ui_1"),skin = "light",collapsed = TRUE),
                           body = bs4Dash::dashboardBody( 
                             bs4Dash::tabItems(
                              bs4Dash::tabItem(tabName = "crc",  mod_crc_new_ui("crc_new_1") )
                             ) ),
                           header = bs4Dash::dashboardHeader(title = "Analiza Garantii"))
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'AnalizaGarantii'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

