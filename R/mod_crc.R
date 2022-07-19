#' crc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_crc_ui <- function(id){
  ns <- NS(id)
  bs4Dash::box(title = "Upload interogarea CRC",footer = "Se uploadaeaza interogarea csv. Aplicatia va prelucra acest fisier si va
               genera un buton de download a fisierului CRC potrivit pentru a putea fi folosit in web.plafon. Suplimentar,
               aplicatia va genera si info din CRC: scor al serviciului datoriei, rate datorate etc.",
               width = 12,status = "info",collapsible = T,collapsed = F,maximizable = T,icon = icon("file-csv"),
               fluidRow(
                 column(width = 4, fileInput(ns("crc_input"),"Upload interogarea CRC",accept = ".csv",
                                                    buttonLabel = "CSV only",placeholder = "No file uploaded")),
                        column(width = 4, uiOutput(ns("show_down"))),
                        
                        column(width = 4, verbatimTextOutput(ns("diverse")))))
}
    
#' crc Server Functions
#'
#' @noRd 
mod_crc_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    output$diverse <- renderPrint( {#req(input$crc_input)
      "It finally works" })
    
    text_read <- eventReactive(input$crc_input,{
      text_read  <- readLines(input$crc_input$datapath) })
    
      index_begin_risc_global <- reactive({ req( text_read() )
        stringr::str_which(string = text_read(),pattern = "II. Riscul global") })
      
      index_begin_istoric <- reactive({ req( text_read() )
        stringr::str_which(string = text_read,pattern = "IIIa. Istoricul") })
      
      risc_global <- reactive({ req( text_read() )
        text_read[(index_begin_risc_global+4):(index_begin_istoric-2)] %>%
        stringr::str_split(pattern = ";", simplify = T) %>% as.data.frame() %>% dplyr::select(-26) })
      
      
      denumiri_coloane <- reactive({ req( text_read() )
        text_read[(index_begin_risc_global+1):(index_begin_risc_global+3)] %>% 
        stringr::str_split(pattern = ";", simplify = T) %>% as.data.frame() %>% dplyr::select(-26)  })
      
      coloane_finale <- reactive({ req( text_read() )
        c(denumiri_coloane %>% dplyr::slice(1) %>% paste0(collapse = ";"),
                          denumiri_coloane %>% dplyr::slice(2) %>% paste0(collapse = ";"),
                          denumiri_coloane %>% dplyr::slice(3) %>% paste0(collapse = ";")) })
    
    output$show_down <- renderUI( { req( text_read(),index_begin_risc_global(),index_begin_istoric(), risc_global(),
                                         denumiri_coloane(), coloane_finale())
      shinyWidgets::downloadBttn(ns("down_crc_prelucrat"),label = "Download CRC pemtru web plafon",
                        style = "stretch",color = "primary")
    })  
  
    output$down_crc_prelucrat <- downloadHandler(filename = function() {"prelucrat.csv"},content = function(file) {
      write.table(x = text_read()[1:index_begin_risc_global()],file = file,quote = F,col.names = F, row.names =FALSE )
      
      
      write.table(x = coloane_finale(),append = TRUE,
                  file = file,quote = F,col.names = F, row.names =FALSE)
      
      write.table(x = risc_global(),append = TRUE,col.names = FALSE,
                  file = file,sep = ";",row.names = F,quote = F)
      
      write.table(x = text_read()[(index_begin_istoric()-1):length(text_read())],
                  file = file,quote = F,col.names = F, row.names =FALSE, append = TRUE)
      
    })
    
    
     
  })
}
    
## To be copied in the UI
# mod_crc_ui("crc_1")
    
## To be copied in the server
# mod_crc_server("crc_1")
