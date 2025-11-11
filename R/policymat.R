policymatUI <- function(id) {
  tagList(
    HTML('<div style="font-size: 0.9em; margin: 20 0 0 0;">'),
    fluidRow(HTML('<p><h3>Policy Instruments by Goal</h3></p>')
    ),
    #fluidRow(HTML('<p><i>This reference set of variables may be extended and revised by suitably trained users through revisions to the source Excel file '),
    #         downloadLink('downloadPathways', label='here.'),
    #         HTML('See User Guide.</i></p><br>')),
    fluidRow(column(12, uiOutput(NS(id,"path_table")), uiOutput(NS(id,"path_tbl_err")))),
    HTML('</div>')
  )
}

#Defining a separate function for this is silly but it allows for future expansion.
stakeholderUI <- function(id) {
  uiOutput(NS(id,"stakeholdermaps"))
}


policymatServ <- function(id, pathwaysDT=NULL) {
  moduleServer(id, function(input, output, session) {
  if(!is.null(pathwaysDT)){
    formatCols <- vector()
    for(i in 1:length(names(pathwaysDT))){
      items <- unique(pathwaysDT[,i]) 
      if(any(c("\U2B07","\U2B06", "\U2B0D", "=") %in% items)){
        formatCols <- c(formatCols, names(pathwaysDT)[[i]]) 
      }
    }
    path_tabs <- lapply(1:length(pathway_names), function(x){ 
      #pathwaysFilt <- pathwaysDT[pathwaysDT$`Policy Goal`==x,] |> select(-`Policy Goal`) |> rename(`Instrument Category`=Instrument) |> rename(Instrument=Implementation) #ALT: TEMP RENAME PENDING PERMANENT DECISION HERE
      name <- pathway_names[[x]]
      pathwaysFilt <- pathwaysDT[pathwaysDT$`Policy Goal`==name,]
      pathwaysDT_out <- datatable(pathwaysFilt |> select(-`Policy Goal`),
                                  filter=list(position='top', clear=F),
                                  rownames=F,
                                  escape=F,
                                  editable=interactive(),
                                  options=list(columnDefs=list(list(className="dt-center", 
                                                                    targets=formatCols)
                                  ),
                                  scrollX=T,
                                  pageLength=10,
                                  lengthMenu=c(2,5,10),
                                  searching=T, 
                                  autoWidth=T)) |>
        formatStyle(formatCols, color=styleEqual(c("\U2B07","\U2B06", "\U2B0D", "="), c("#e03d3d","#32a852", "darkgrey", "darkgrey")), fontSize="250%")
      
      return(tabPanel(title=sprintf("Policy Goal #%i: %s", x, name),
                      fluidRow(column(10,renderDT(pathwaysDT_out)))
      ))
      
      
    })
    output$path_table <- renderUI({
      do.call(tabsetPanel, path_tabs)
    })
  } else {
    output$path_tbl_err <- renderUI(HTML("Error: Pathways file not found or improperly formatted"))
  }
  })
}

stakeholderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    image_list <- list.files("www/stakeholder_maps", "(\\.png$)|(\\.jpeg$)|(\\.jpg$)|(\\.gif$)|(\\.svg$)")
    if(length(image_list > 0)) { 
      output$stakeholdermaps <- renderUI({
        map(image_list, function(image) {
          renderImage(list(src=paste0("www/stakeholder_maps/", image), width='50%'), deleteFile=F)
        })
      })
    } else {
      output$stakeholdermaps <- renderText("No stakeholder maps found. Add image files to the stakeholder maps folder to begin.")
    }
  })
}