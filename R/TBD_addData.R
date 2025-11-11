addDataUI <- function(id) {
  tagList(
    fluidRow(fileInput(NS(id,"fileUpload"),"Choose file...", accept=c(".xlsx", ".xls", ".dta", ".csv", ".txt")),
             fileInput(NS(id, "metaUpload"), "Upload Metadata", accept=c(".xlsx", ".xls", ".dta", ".csv", ".txt"))
             ),
    fluidRow(textInput(NS(id, "fileName"), "File Name"),
    numericInput(NS(id, "fileYear"), "File Year", min=1000, max=2100, value=format(Sys.Date(), "%Y"))
    ),
    fluidRow(DTOutput(NS(id, "indictable"))),
    fluidRow(tags$p(style="color: #fc2403", textOutput(NS(id, "saveWarning")))),
    fluidRow(actionButton("saveTable", "Append to Existing Metadata", width="25%"), actionButton("overTable", "Clear Table", width="25%"))
  )
}

addDataServer <- function(id, dataset_list="") {
  moduleServer(id, function(input, output, session){
    vals <- reactiveValues(dataset=data.frame(),
                           indicators=data.frame(shortName=character(),
                                                 category=character(),
                                                 label=character(),
                                                 axis=character(),
                                                 units=character()),
                           saveFileName= reactive({req(input$fileName)
                             paste0(str_replace_all(input$fileName, "_", input$fileYear), " ", "_")
                             }))
    output$indictable <- renderDT({
      if(nrow(vals$indicators!=0)) { 
      datatable(vals$indicators, editable='row', options=list(dom="tip"), rownames=F)
      }
      })
    observeEvent(input$fileUpload, {
      file <- input$fileUpload
      ext <- tools::file_ext(file$datapath)
      vals$dataset <- tryCatch(switch(ext, 
                                 csv = read.csv(file$datapath),
                                 dta = haven::read_dta(file$datapath),
                                 xslx=readxl::read_xlsx(file$datapath),
                                 xls=readxl::read_xls(file$datapath),
                                 txt=read.delim(file$datapath)
      ), error=function(e){
        showNotification("Error: Cannot read file", type="error")
        return(data.frame())})
      
      if(nrow(vals$dataset) > 0) {
        fname <- file$name
        if(str_detect(fname, "([12][09][0-9]{2})")) {
          updateNumericInput(session, "fileYear", value=as.numeric(str_extract(fname, "([12][09][0-9]{2})")))
          fname <- str_remove(fname, "([_]*[12][09][0-9]{2})")
        }
          updateTextInput(session, "fileName", value=fname)
      vals$indicators <- data.frame(shortName = names(vals$dataset),
                                    label="",
                                    category="",
                                    axis="",
                                    units="")
      
      if(ext=="dta"){
        vals$indicators$label <- attr(vals$dataset, "label")
      }
      }
    })
    
    observeEvent(input$metaUpload, {
      metaset <- tryCatch(switch(ext, 
                                 csv = read.csv(file$datapath),
                                 dta = haven::read_dta(file$datapath),
                                 xslx=readxl::read_xlsx(file$datapath),
                                 xls=readxl::read_xls(file$datapath),
                                 txt=read.delim(file$datapath)
      ), error=function(e){
        showNotification("Error: Cannot read file", type="error")
        return(NA)})
      if(!all(names(metaset) %in% names(vals$indicators))) {
        showNotification("Uploaded metadata does not have the correct columns")
      } else {
        #In case of weird inputs
        vals$indicators <- data.frame(shortName = metaset$shortName,
                                      category=metaset$category,
                                      label=metaset$label,
                                      axis=metaset$axis,
                                      units=metaset$units)
      }
    })
    
    observeEvent(input$indictable_cell_edit, {
      row  <- input$indictable_cell_edit$row
      clmn <- input$indictable_cell_edit$col
      vals$indicators[row, clmn] <- input$indictable_cell_edit$value
    })
    
    observeEvent(c(input$fileName, input$fileYear), ignoreInit=T, {
      if(vals$saveFileName %in% req(dataset_list)) {
        output$saveWarning <- "Data file exists and will be overwritten"
      } else {
        output$saveWarning <- ""
      }
    })
    
    observeEvent(input$saveTable, {
      if(is.null(dataset)) { 
        showNotification("No data to save", type="error")
      } else {
        vals$indicators$file <- vals$saveFileName
      if(file.exists("Update/indicators.csv")){
        indicators_in <- read.csv("Update/indicators.csv")
        indicators_out <- bind_rows(indicators_in, vals$indicators)
      }
      indicators_out$dups <- duplicated(indicators_out$shortName, fromLast=TRUE)
      indicators_out <- indicators_out |> filter(dups==FALSE) |> select(-dups)
      write.csv(dataset, paste0("Data/", file$name, ".csv", row.names=F))
      write.csv(indicators_out, "Update/indicator_list.csv", row.names=F)
      }
    })
    observeEvent(input$overTable, {
      vals$indicators <- data.frame(shortName = names(vals$dataset),
                                    category="",
                                    label="",
                                    axis="",
                                    units="")
    })
  })
}

# metadataUI <- function(id) {
#   tagList(
#     DTOutput(NS(id, "metadatatable")),
#     actionButton(NS(id, "saveMeta"), "Save")
#   )
# }
# 
# metadataServer <- function(id, indicator_list=NULL) {
#   moduleServer(id, function(input, output, session){
#     req(indicator_list)
#     datatable(indicator_list, editable='row', options=list(dom="tip"), rownames=F)
#     observeEvent(input$saveMeta) {
#       write.csv("Update/indicator_list.csv", row.names=F)
#     }
#     
#   })
# }