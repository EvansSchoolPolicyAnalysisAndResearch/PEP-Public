evidenceUI <- function(id, goalNames, adm_levels, indicators){ 
  shinyjs::useShinyjs()
  ns <- shiny::NS(id)
  if(is.null(goalNames)){
    tagList(HTML("This section will be displayed when policy goals have been selected."))
  } else if(is.null(adm_levels)) {
    tagList(HTML("There is a problem with the adm_levels.csv spreadsheet. Contact your administrator for help."))
  } else if(is.null(indicators)) {
    tagList(HTML("This content cannot be displayed until some indicators have been selected using the indicators_list spreadsheet."))
  } else {
  tagList(
      tabPanel("Data",
      fluidRow(column(2, selectInput(NS(id, 'policiesBox'), "Select a policy goal", choices=c("None", goalNames))),
               column(2, uiOutput(NS(id, 'pathwaysBox')))),
      
      conditionalPanel("input.policiesBox!='None'", ns=NS(id),
      tabsetPanel(
        tabPanel("Variable Summary Table",
                 fluidRow(column(4, radioButtons(NS(id,'totsBtns'), label="Choose Statistic to Present", choices=c("Mean","Total")))),
                 fluidRow(HTML("<p><i> The values in this table represent the unit-level averages (or totals) across the entire dataset.</i></p>")),
                 dataTableOutput(NS(id, 'trendsTable')),
                 downloadButton(NS(id, 'downloadSummary'),
                                label='Download Table Data',
                                icon=icon('file-csv'))
        ),
      tabPanel("Mapping",
              #HTML('<div style="font-size: 0.9em;">'),
               #fluidRow(column(4, uiOutput(NS(id,"trendsErr")))),
               #to implement
              fluidRow(input_switch(ns("mapswitch"), "Show Second Map", value=F)),
              fluidRow(column(6, uiOutput(ns("mapout1"))),
                       conditionalPanel("input.mapswitch == true", ns=NS(id),
                       column(6, uiOutput(ns("mapout2")))
                       )),
      ))
      )     
    )
  )
  }
}

updateVarTable <- function(pathwaysIn1=NULL, policiesIn1, obsyear, totsBtns){
  pathwaysIn <- if(is.null(pathwaysIn1)){
    "0"
  } else {
    pathwaysIn1
  }
  filtered_tab <- filterVarTable(data_table_out$data_table, pathway_link, pathwaysIn, indicator_list, totsBtns)
  output$trendsTable <- renderDataTable(
    DT::datatable(filtered_tab |> select(-shortName), 
                  options=list(searching=F, pageLength=15, dom='tip'), rownames=F)  |>
      formatPercentage(5) #Hard coded, to fix
  )
  output$flagsTable <- renderDataTable(
    DT::datatable(
      filterFlagTable(data_table_out$flag_table, pathway_link, pathwaysIn, indicator_list) |> select(-shortName), 
      options=list(searching=F, pageLength=15), rownames=F)
  )
  output$trendVarChoose <- renderUI({
    trendVarList <- getIndics(pathway_link, indicator_list, indic_inventory, policiesIn1, pathwaysIn, obsyear, cats=T)
    trendVarList <- c("0", trendVarList)
    names(trendVarList)[[1]] <- "Select..."
    #trendVarList <- as.list(c("0", filtered_tab$shortName))
    #names(trendVarList) <- c("Select...", filtered_tab$Variable)
    selectizeInput('trendIn', "Choose a variable to map:", choices=trendVarList)
  })
}

#Helper function, to move.
match_regex <- function(pattern, vec) {
  regex_out <- regexec(pattern, vec)
  sapply(regex_out, FUN=function(x){
    x[[1]]!=-1
  })
}

evidenceServer <- function(id, globals, shps) {
  moduleServer(id, function(input, output, session) {
    if(any(is.null(globals))) {
      #error handling here
    } else {
    
    list2env(globals, environment())
    output$pathwaysBox <- renderUI({
      if(req(input$policiesBox)!="None") {
      pickerInput(NS(id, "pathwaysIn"), "Choose a pathway (optional)", 
                  choices=polic_Names[[input$policiesBox]], 
                  options=list(style="selectize-input"), 
                  choicesOpt=list(disabled=polic_activ[[input$policiesBox]]))
      }
      })
    
    indics_out <- reactive({
    if(req(input$policiesBox)!="None") {
      indicators <- if(req(input$pathwaysIn)==0){
        pathway_link |> filter(goalName==input$policiesBox) |> select(shortName) |> distinct()
      } else {
        pathway_link |> filter(pathwayID==input$pathwaysIn) |> select(shortName) |> distinct()
      }
    }
    })
    

    
    module_data <- reactive({
      if(req(input$policiesBox)!="None"){
      df <- getData(indic_inventory, xvars=pathway_link |> filter(goalName==input$policiesBox) |> select(shortName) |> distinct(), indicators=indicator_list, adm_levels=adm_levels)  #maybe a better way to specify these arguments #TODO: undo hard coding on UNITID - we need a specifications section.
      }
      }) |> bindCache(input$policiesBox)
    
    filtered_tab <- reactive({
      req(module_data(), input$pathwaysIn)
      filterVarTable(module_data()$adm0, pathway_link, input$pathwaysIn, indicator_list, input$totsBtns)
    }) 
    
    output$trendsTable <- renderDataTable({
      req(filtered_tab())
      DT::datatable(filtered_tab() |> select(-shortName), 
                    options=list(searching=F, pageLength=15, dom='tip'), rownames=F)  |>
        formatPercentage(5) #Hard coded, to fix
    })
    
    adm_levels_avail <- reactive({
      req(module_data())
      data.frame(admLevel=names(shps)) |> 
      inner_join(data.frame(admLevel=names(module_data()))) |>
      inner_join(adm_levels)
    })
    
    output$mapout1 <- renderUI({
      mapUI(NS(id, "map1"), year_list, adm_levels_avail(), territory_names, left_join(indics_out(), indicator_list))
    })
    
    output$mapout2 <- renderUI({
      mapUI(NS(id, "map2"), year_list, adm_levels_avail(), territory_names, left_join(indics_out(), indicator_list))
    })
    
    mapServer("map1", module_data(), shps, adm_levels_avail(), left_join(indics_out(), indicator_list))
    mapServer("map2", module_data(), shps, adm_levels_avail(), left_join(indics_out(), indicator_list))
    
    output$downloadSummary <- downloadHandler(
      filename=function(){
        paste0("data_summary_", Sys.Date(), ".csv")
      },
      content=function(file) {
        req(filtered_tab())
        write.csv(filtered_tab(), file, row.names=F)
      }
    )
}
  })
}

litUI <- function(id){
  DTOutput(NS(id, "evidence_tab"))
}




#Maybe we add more stuff here?
litServer <- function(id, ref_sources=ref_sources){
  moduleServer(id, function(input,output,session) {
  if(!is.null(ref_sources))
    if(with(ref_sources, exists("Link"))) {
      ref_sources$Link <- add_links(ref_sources$Link)
    }
    output$evidence_tab <- renderDT(ref_sources, rownames=F, escape=c(-which(names(ref_sources)=="Link")), options=list(autowidth=TRUE, dom='ftp'))
  })
}