diagnosticsUI <- function(id) {
  tagList(
  tabsetPanel(
    tabPanel("User Guide",
             fluidRow(column(1),
                      column(8, 
                             HTML("<div style='font-size:0.9em'>"),
                             includeHTML('Docs/user_guide.html'),
                             HTML("</div>")
                      ))
    ),
    tabPanel("App Diagnostics",
             uiOutput(NS(id, "data_list_status")),
             uiOutput(NS(id, "inst_list_status")),
             uiOutput(NS(id, "indics_status")),
             uiOutput(NS(id, "pathway_link_status")),
             uiOutput(NS(id, "groups_status")),
             uiOutput(NS(id, "pathway_table_status")),
             uiOutput(NS(id, "indicators_diags"))
    )
  )
  )
}

diagnosticsServer <- function(id, dataset_list=NULL, indic_inventory=NULL, instrument_list=NULL, indicator_list=NULL, pathway_link=NULL, policy_path=NULL) {
  moduleServer(id, function(input, output, session) {
    #if(exists("dataset_list")){
    #  output$data_list_status <- renderUI(HTML("<p style='color: #5ac447'>Data are available.</p>"))
    #} else {
    #  output$data_list_status <- renderUI(HTML("<p style='color: #c92031'>Data not found.</p>"))
    #} # To do: add error handling
    
    #if(is.list(instrument_list)){
    #  output$inst_list_status <- renderUI(HTML("<p style='color: #5ac447'>Instrument list is present and correctly formatted.</p>"))
    #} else {
    #  output$inst_list_status <- renderUI(HTML("<p style='color: #c92031'>Instrument list is missing or improperly formatted.</p>"))
    #}

    if(is.list(indicator_list)) {
      output$indics_status <- renderUI(HTML("<p style='color: #5ac447'>Variable list is present and correctly formatted.</p>"))
    } else {
      output$indics_status <- renderUI(HTML("<p style='color: #c92031'>Variable list is missing or improperly formatted.</p>"))
    }
    
    if(is.list(pathway_link)){
      output$pathway_link_status <- renderUI(HTML("<p style='color: #5ac447'>Pathway linking table is present and correctly formatted.</p>"))
    } else {
      output$pathway_link_status <- renderUI(HTML("<p style='color: #c92031'>Pathway linking table is missing or improperly formatted.</p>"))
    }
    
    if(is.list(groups_list)){
      output$groups_status <- renderUI(HTML("<p style='color: #5ac447'>Grouping variable metadata are present and correctly formatted.</p>"))
    } else {
      output$groups_status <- renderUI(HTML("<p style='color: #c92031'>Grouping variable metadata are missing or improperly formatted.</p>"))
    }
    
    if(is.list(policy_path)){
      output$pathway_table_status <- renderUI(HTML("<p style='color: #5ac447'>Policy pathway table is present and correctly formatted.</p>"))
    } else {
      output$pathway_table_status <- renderUI(HTML("<p style='color: #c92031'>Policy pathway table is missing or improperly formatted.</p>"))
    }
    
    indicatorCheck <- function(y){
      datasetPres <- y %in% indic_inventory$shortName
      indicatorPres <- y %in% indicator_list$shortName 
      goalPres <- y %in% pathway_link$shortName 
      if(goalPres){
        goalnum <- max(pathway_link |> filter(shortName==y) |> select(pathwayID))
        pathPres <- any(goalnum == policy_path$pathwayID)
      } else {
        pathPres <- F
      }
      if(indicatorPres) {
        rowpop <- indicator_list |> filter(shortName==y)
        if(nrow(rowpop)==1){
          dups <- F
        } else {
          dups <- T
        }
        if(dups){
          rowpop <- rowpop[1,]
        }
      }
      
      data_files <- indic_inventory |> filter(shortName==y) |> select(file) |> unique()
      dup_files <- length(data_files) > 1
      data_years <- indic_inventory |> filter(shortName==y) |> select(year) |> distinct() |> as.data.frame()
      res <- data.frame(shortName=y, 
                        Variable= if(indicatorPres) rowpop$label else "NA",
                        `Years Available` = if(datasetPres) paste(data_years[,1], collapse=", ") else "NA", 
                        `In Dataset`= if(datasetPres) as.character(icon("check")) else as.character(icon("xmark")),
                        `In Indicators List`=if(indicatorPres) as.character(icon("check")) else as.character(icon("xmark")),
                        `Assigned to a Policy Goal` = if(goalPres) as.character(icon("check")) else as.character(icon("xmark")),
                        `Assigned to a Pathway` = if(pathPres) as.character(icon("check")) else as.character(icon("xmark"))
      )
      return(res)
    }
    if(is.list(indic_inventory) & is.list(indicator_list) & is.list(pathway_link) & is.list(policy_path)) {
      files <- unique(indic_inventory$file)
      indics_list <- lapply(files, FUN=function(x){
        indics_sub <- indic_inventory |> filter(file==x)
        indics_chk <- lapply(unique(indics_sub$shortName), FUN=indicatorCheck)
        return(do.call(rbind, indics_chk))
      })
      rogueIndics <- indicator_list$shortName[which(!(indicator_list$shortName %in% indic_inventory$shortName))]
      indics_chk2 <- do.call(bind_rows, lapply(rogueIndics, FUN=indicatorCheck))
      indics_list <- c(indics_list, list(indics_chk2))
      names(indics_list) <- c(files, "Not Found in File")
      output$indicators_diags <- renderUI({
        tabs <- lapply(1:length(indics_list), FUN=function(x){
          names(indics_list[[x]]) <- str_replace_all(names(indics_list[[x]]), "\\.", " ")
          tabPanel(title=names(indics_list)[[x]],
                   DT::renderDT(indics_list[[x]], escape=F))
        })
        return(do.call(tabsetPanel, tabs))
      })
      
    }
  })
  
}