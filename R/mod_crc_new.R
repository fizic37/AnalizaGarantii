#' crc_new UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_crc_new_ui <- function(id){
  ns <- NS(id)
  bs4Dash::box(title = "Upload interogarea CRC",footer = "Se uploadeaza interogarea csv. Aplicatia va prelucra acest fisier si va
               genera un buton de download a fisierului CRC potrivit pentru a putea fi folosit in web.plafon. Suplimentar,
               aplicatia va genera si info din CRC: scor al serviciului datoriei, rate datorate etc.",
               width = 12,status = "info",collapsible = T,collapsed = F,maximizable = T,icon = icon("file-csv"),
               fluidRow(
                 column(width = 4, fileInput(ns("crc_input"),"Upload interogarea CRC",accept = ".csv",
                                             buttonLabel = "CSV only",placeholder = "No file uploaded")),
                 column(width = 8, uiOutput(ns("show_down"))),
                 
                 column(width = 12, DT::dataTableOutput(ns("sinteza_crc")))
                 
                 ))
}
    
#' crc_new Server Functions
#'
#' @noRd 
mod_crc_new_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    library(magrittr)
    text_read <- eventReactive(input$crc_input,{
      text_read  <- readLines( input$crc_input$datapath, encoding = "native.enc" ) })
    
    index_begin_date_identificare <- reactive({ req(text_read())
      stringr::str_which(string = text_read(),pattern = "I. Date de identificare") })
    
    nume_beneficiar <- tryCatch(expr = {
      reactive({ req(text_read(), index_begin_date_identificare())
        (text_read()[index_begin_date_identificare()+3] %>% stringr::str_split(pattern = ";",simplify = T))[2]
      }) 
      },error = function() {"prelucrat"} )
    
    cui_beneficiar <- reactive({ req(text_read(), index_begin_date_identificare())
      (text_read()[index_begin_date_identificare()+3] %>% stringr::str_split(pattern = ";",simplify = T))[1]
      })
    
    index_begin_risc_global <- reactive({ req( text_read() )
      
      stringr::str_which(string = text_read(),pattern = "II. Riscul global") })
  
    is_risc_global <- reactive({ req( text_read() )
      ifelse(length( stringr::str_which(string = text_read(),pattern = "II. Riscul global")) == 0,"NO","YES")
    })
    
    
    index_begin_istoric <- reactive({ req( text_read() )
                stringr::str_which(string = text_read(),pattern = "IIIa. Istoricul") })
    
    
    denumiri_coloane <- reactive({ req( text_read(), index_begin_risc_global() )
      text_read()[( index_begin_risc_global()+1):(index_begin_risc_global()+3)] %>% 
        stringr::str_split(pattern = ";", simplify = T) %>% as.data.frame() %>% dplyr::select(-dplyr::starts_with("V26"))  })
    
    risc_global <- reactive({ req( text_read(),denumiri_coloane()  )
     text_read()[(index_begin_risc_global()+4):(index_begin_istoric()-2)] %>%
        stringr::str_split(pattern = ";", simplify = T) %>% as.data.frame() %>% dplyr::select(-dplyr::starts_with("V26")) %>%
        setNames(object = .,nm = purrr::map_chr(denumiri_coloane(), ~paste0(.x,collapse = " ") %>% 
                                                  stringr::str_trim(string = .,side = "both"))) %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::contains("Suma"), ~as.numeric(.x)))
    })
    
    
    
    sinteza_crc <- reactive({ req( is_risc_global() == "YES" )
      
      risc_global() %>% dplyr::group_by(`Serviciul datoriei`) %>% 
        dplyr::summarise(Total_suma_datorata = sum(`Total suma datorata`),
                         Total_suma_utilizata = sum(`Suma datorata utilizata`),
        Ponderi_suma_utilizata = sum(`Suma datorata utilizata`)/sum(risc_global()$`Suma datorata utilizata`)) %>%
        dplyr::mutate( Ponderi_serv_datorie = ifelse(`Serviciul datoriei`=="A",0.5,ifelse(
          `Serviciul datoriei`== "B",0.25, ifelse(`Serviciul datoriei`=="C",0.15, ifelse(`Serviciul datoriei`=="D",0.1,0)) ))) %>%
        dplyr::mutate(Scor_serv_datorie = Ponderi_serv_datorie * Ponderi_suma_utilizata)
        
      })
    
    caption_sinteza <- reactive({ req(sinteza_crc(),nume_beneficiar()  )
      paste0( "Sinteza fisier CRC pentru beneficiarul ", nume_beneficiar(), ", CUI ",cui_beneficiar(), ". Scorul serviciului datoriei este ",
            ifelse(sum(risc_global()$`Suma datorata utilizata`)==0,0.5, round(sum(sinteza_crc()$Scor_serv_datorie),6))
            )     })
    
    coloane_finale <- reactive({ req( text_read() )
      c(denumiri_coloane() %>% dplyr::slice(1) %>% paste0(collapse = ";"),
        denumiri_coloane() %>% dplyr::slice(2) %>% paste0(collapse = ";"),
        denumiri_coloane() %>% dplyr::slice(3) %>% paste0(collapse = ";")) })
    
    output$sinteza_crc <-   DT::renderDataTable({  req( is_risc_global() )
      switch( EXPR = is_risc_global(),
          "YES" =  DT::datatable( data = sinteza_crc() %>% dplyr::select(1:4) %>% janitor::adorn_totals(where = "row"),
          rownames = FALSE, options = list(dom = "Bt", buttons = c("copy", "excel")),extensions = "Buttons",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', caption_sinteza()) ) %>% 
          DT::formatRound(columns = 2:3, digits = 1) %>% DT::formatPercentage(columns = 4, digits = 1),
          "NO" = DT::datatable(data = data.frame('Am citit din fisierul uploadat' = text_read(),check.names = FALSE),
          rownames = FALSE, options = list(dom="t"),
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: #ff007b;',
                "STOP, nu am putut prelucra riscul global")
      ) )
      })
    
    output$show_down <- renderUI( { req( text_read(),index_begin_risc_global(),index_begin_istoric(),
                    risc_global(),nume_beneficiar(), denumiri_coloane(), coloane_finale())
      div(style= "padding-top:25px; padding-left: 125px;",
      shinyWidgets::downloadBttn(ns("down_crc_prelucrat"),label = nume_beneficiar(),
                                 style = "stretch",color = "primary") )
    }) 
    
    
    output$success_message <- renderText( { req( text_read(),index_begin_risc_global(),index_begin_istoric(), risc_global(),
                                               denumiri_coloane(), coloane_finale())
      "Fisier prelucrat cu succes" })
    
    output$down_crc_prelucrat <- downloadHandler(filename = function() {paste0(nume_beneficiar(), ".csv")},
          content = function(file) {
      write.table(x = text_read()[1:index_begin_risc_global()],file = file,quote = F,col.names = F, row.names =FALSE,
                  fileEncoding =  "native.enc")
      
      
      write.table(x = coloane_finale(),append = TRUE,fileEncoding =  "native.enc",
                  file = file,quote = F,col.names = F, row.names =FALSE)
      
      write.table(x = risc_global(),append = TRUE,col.names = FALSE,fileEncoding =  "native.enc",
                  file = file,sep = ";",row.names = F,quote = F)
      
      write.table(x = text_read()[(index_begin_istoric()-1):length(text_read())], fileEncoding =  "native.enc",
                  file = file,quote = F,col.names = F, row.names =FALSE, append = TRUE)
     
     })
    
  })
}
    
## To be copied in the UI
# mod_crc_new_ui("crc_new_1")
    
## To be copied in the server
# mod_crc_new_server("crc_new_1")
