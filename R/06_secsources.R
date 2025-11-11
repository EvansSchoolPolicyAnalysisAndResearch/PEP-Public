secsourcesUI <- function(id, ext_sources) { #only using ext_sources here to check.
  shinyjs::useShinyjs()
  if(is.null(ext_sources)){
    HTML("This section can hold additional sources of contextual information by updating the secondary_sources spreadsheet.")
  } else {
  tagList(
  HTML('<div style="font-size: 0.9em">'),
  fluidRow(HTML("<p>This table shows additional sources of contextual information. Updates can be made by downloading the "),
           downloadLink(NS(id, "secSourcesDL"), "associated spreadsheet."), HTML("</p>")),
  fluidRow(DTOutput(NS(id, 'secsources'))),
  HTML('</div>')
  #Interactive tables (under development)
  # if(interactive()){ fluidRow(
  #   column(2),
  #   column(2, actionButton(NS(id,"saveDT"), "Save Table")),
  #   column(2, actionButton(NS(id, "newDT"), "New Table")),
  #   column(6)
  #)
  #  }
  )
  }
}

secsourcesServer <- function(id, ext_data=NULL) {
  moduleServer(id, function(input, output, session) {
    #if(!interactive()){ #eventually/probably better to have an "Edit" mode and a "Presentation mode" in the configs?
    if(with(ext_data, exists("Link"))){
     ext_data$Link <- add_links(ext_data$Link) 
    }
    #}
    vals <- reactiveValues(ext_data=ext_data)
    newTable <- function() {
      data.frame(Source=c(""), `Relevant Variables`=c(""), Location=c(""))
     }
    

    observeEvent(input$saveDT, {
      write.csv(ext_data, "Update/secondary_sources.csv", row.names=F)
      showNotification("Edits saved!")
      shinyjs::disable(input$saveDT)
    })
    
    observeEvent(input$newDT, {
      if(file.exists("Update/secondary_sources.csv")){
        showModal(modalDialog("Warning!", "Existing table will be overwritten",
                              footer=tagList(modalButton("Cancel"), actionButton(NS(id, "ok"), "Proceed"))))
        
      } else {
        vals$ext_data <- newTable()
      }
    })
    
    observeEvent(input$ok, {
      vals$ext_data <- newTable()
      removeModal()
    })
    

    
    #To do 
    #output$secsources <- renderDT({datatable(vals$ext_data, editable=interactive(), escape=c(-which(names(vals$ext_data)=="Link")), options=list(dom="t"), rownames=F)})
    output$secsources <- renderDT({datatable(vals$ext_data, escape=c(-which(names(vals$ext_data)=="Link")), options=list(dom="t"), rownames=F)})
    observeEvent(input$secsources_cell_edit, {
      row  <- input$secsources_cell_edit$row
      clmn <- input$secsources_cell_edit$col
      vals$ext_data[row, clmn] <- input$secsources_cell_edit$value
      shinyjs::enable(input$saveDT)
    })
    
    output$secSourcesDL <- downloadHandler(
      filename="secondary_sources.csv",
      content=function(file){
        file.copy("Update/secondary_sources.csv")
      },
      contentType="text/csv"
    )
    
  })
}