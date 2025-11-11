#============
#Functions
#============

#Leaving this here because it's only used in this module.
flexTableOut <- function(data=all_data(), adm_level=admMergeVar(), aggs_list=NULL, measurevars, lab, groupslab) {
  corrTab <- data |> 
    ungroup() |> 
    select(any_of(c(adm_level, aggs_list, measurevars))) |> 
    mutate_if(is.character, ~case_match(., "" ~ NA, .default=.)) |> 
    mutate_at(measurevars, ~signif(., 4)) |>
    na.omit()
  
  if(nrow(corrTab) <=60) { #arbitrary, to fix.
    corrTabFlx <- corrTab
    if(!is.null(aggs_list)) { 
      names(corrTabFlx) <- c(str_to_title(adm_level), groupslab, paste0(measurelabel, " (Obs unit Avg)")) #This won't work with multiple disagg vars; fix later.
      corrTabFlx <- flextable(corrTabFlx)
      corrTabFlx <- merge_v(corrTabFlx, j=str_to_title(adm_level)) |>
        autofit() |>
        htmltools_value() 
    } else { 
      names(corrTabFlx) <- c(str_to_title(adm_level), paste0(lab, " (Obs Unit Avg)")) #This won't work with multiple disagg vars; fix later.
      corrTabFlx <- flextable(corrTabFlx) |>
        autofit() |>
        htmltools_value()
    }
  } else {
    corrTabFlx <- HTML("Tables are not displayed at this level because they would be too long.")
  }
  return(list(corrTab,corrTabFlx)) #Need corrTab to download
}



#============
#UI
#============
comparisonsUI <- function(id, goalNames, year_list, adm_levels) {
  shinyjs::useShinyjs()
  adm_levels <- na.omit(adm_levels)
  if(is.null(goalNames)){
    tagList(HTML("This section will be displayed when policy goals have been selected."))
  } else if(is.null(adm_levels)) {
    tagList(HTML("There is a problem with the adm_levels.csv spreadsheet. Contact your administrator for help."))
  } else {
  tagList(
HTML('<div style="font-size: 0.9em">'),
fluidRow(column(4,uiOutput(NS(id, "explorerErr")))),
fluidRow(column(5, selectInput(NS(id,'policiesBox2'), "Select a policy goal", choices=c("None", goalNames)))),
conditionalPanel(condition="input.policiesBox2!='None'", ns=NS(id),
                 fluidRow(column(8, uiOutput(NS(id,'dataPathBox')))),
                 fluidRow(column(4, uiOutput(NS(id, "yearRadio"))), 
                          column(8, br(), actionButton(NS(id,'makeHeatMap'),"Show Heatmap"))),
                 fluidRow(column(4, wellPanel(style="background-color: #ededed; border-color: #9c9c9c; padding=10;",
                                              fluidRow(column(6, uiOutput(NS(id,'indicsBox'))),
                                                       column(6, uiOutput(NS(id,'corrsBox')))),
                                              fluidRow(column(6, align='center', uiOutput(NS(id,'indicsDesc'))), column(6, align='center', uiOutput('corrsDesc'))),
                                              hr(),
                                              fluidRow(checkboxInput(NS(id,'yChk'), 'Omit 0s from Y Variable')),
                                              fluidRow(radioButtons(NS(id, 'adm_levels'), "Choose Administrative Unit", choiceNames = adm_levels$admLabel, choiceValues=adm_levels$admLevel)),
                                              fluidRow(column(10, uiOutput(NS(id,"groupsBtn")))),
                                              fluidRow(actionButton(NS(id,'submitBtn'), "Compare Variables")),
                                              hr(),
                                              fluidRow(HTML("<b>Download Data</b>")),
                                              br(),
                                              fluidRow(column(6, downloadButton(NS(id,'downloadRawShort'), 'Download Selected Raw Data',icon=icon('file-csv'))),
                                                       column(6, downloadButton(NS(id,'downloadRawLong'), 'Download All Listed Raw Data', icon=icon('file-csv')))
                                              ),
                                              br(),
                                              fluidRow(column(9, HTML('<p style="font-size:10px"><i>Note: clicking "Download Selected Raw Data" will download only the "X" and "Y" variables chosen in the boxes above. "Download All Listed Raw Data" will instead download all. The data will be processed according to the selections made in terms of omitting zeroes, administrative level, and grouping.</i></p>')))
                 )
                 ),
                 column(6,
                        plotlyOutput(NS(id,'heatMap'), height="100%"))
                 ),
                 fluidRow(HTML("&nbsp;")),
                 fluidRow(HTML("&nbsp;")),
                 #fluidRow(column(6, uiOutput(NS(id,'indicHeader'))), column(6, uiOutput(NS(id,'corrHeader')))),
                 fluidRow(column(6, uiOutput(NS(id,'indicatorHist')) 
                 ), 
                 column(6, uiOutput(NS(id,'corrHist'))
                 )
                 ),
                 fluidRow(column(6, plotOutput(NS(id,'indicatorMap')) 
                                 #downloadButton("dlindicMap", label="", icon=icon("file-arrow-down"))
                 ), 
                 column(6, plotOutput(NS(id,'corrMap')) 
                        #downloadButton("dlcorrMap", label="", icon=icon("file-arrow-down"))
                 )
                 ),
                 fluidRow(plotOutput(NS(id,'scatterPlot'))),
                 fluidRow(uiOutput(NS(id,'plotInterp')))
),
HTML('</div>')
)
  }
}

#============
#Server
#============
comparisonsServer <- function(id, globals, territory_names) {
  moduleServer(id, function(input, output, session) {

    list2env(globals, environment()) #Unpack the list back into the original data frames/vectors.
    if(any(is.null(globals))) {
      #to do: improve error handling (we don't want the app to crash completely if something isn't present, though)
    } else {
    #Fix names when someone clicks the button
    xname <- reactive({input$xvarSelect}) |> bindEvent(input$submitBtn)
    yname <- reactive({input$yvarSelect}) |> bindEvent(input$submitBtn)
    admMergeVar <- reactive({input$adm_levels }) |> bindEvent(input$submitBtn)
    
    output$yearRadio <- renderUI({
      if(!is.null(year_list)) {
        radioGroupButtons(NS(id, 'yearBtn'), label="Survey Year", choices=year_list, selected=max(year_list), size='sm')
      }
    })
    
  
    #To implement
    #output$adm_subunit <- renderUI({
    #  req(input$disAgg_admin)
    #  if(with(territory_names, exists(input$disAgg_admin))){
    #    selectInput(NS(id, "adminSelect"), sprintf("Select %s", adm_levels$admLabel[adm_levels$admLevel==input$disAgg_admin]), choices=territory_names[[input$disAgg_admin]])
    #  }
    #})
  
    #This also needs to be fixed - some of this stuff should be turfed to the loading files.
  output$groupsBtn <- renderUI({
    req(input$yearBtn, groups_list, input$policiesBox2)
    groups_sub <- groups_list |> filter(level=="All" | level==input$policiesBox2) # fix names 
    yeargroups <- tryCatch(read.csv(sprintf("Data/groups_%s.csv", input$yearBtn), nrows=1), 
                          error=function(e){return(NULL)})
    if(!is.null(yeargroups)){
    groups_sub = groups_sub |> filter(varName %in% yeargroups)
    if(nrow(groups_sub)!=0){
      renderUI(radioButtons(NS(id,"groupsChk"), HTML("<b>Selecting Grouping Variable</b>"), choiceNames=c("None", groups_sub$label), choiceValues=c("", groups_sub$varName)))
    } else {
      renderUI(radioButtons(NS(id, "groupsChk"), HTML("<b>Selecting Grouping Variable</b>"), choiceNames="None Available", choiceValues=""))
    } 
    } else {
      renderUI(radioButtons(NS(id, "groupsChk"), HTML("<b>Selecting Grouping Variable</b>"), choiceNames="None Available", choiceValues=""))
  }
    #radioButtons(NS(id,"groupsChk"), HTML("<b>Selecting Grouping Variable</b>"), choiceNames=c("None", groups_list$label), choiceValues=c("", groups_list$varName))
  }) |> bindCache(input$yearBtn, input$policiesBox2)
  
#Not functional
  #observe({
  #  shinyjs::toggle(id="makeHeatMap", condition=req(input$policiesBox2!="None"))
  #})
  
  
  indics <- reactive({
    if(req(input$policiesBox2)!="None"){
      getIndics(pathway_link, indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
    }
  })
  
  
  #This might be generalizable to reduce code elsewhere.
  #Combine with above?
  heatmap_indics <- reactive({
    #Error handling needed here, too. 
    if(req(input$policiesBox2)!="None") {
    indics_out <- getIndics(pathway_link |> filter(pathwayID!=0), indicator_list, indic_inventory, input$policiesBox2, input$pathwaysIn2, input$yearBtn)
    indics_out <- unlist(indics_out)
    indics_out <- data.frame(shortName=indics_out)
    indics_out <- merge(indics_out, indic_inventory, by="shortName") |> filter(year==input$yearBtn)
    indics_out <- merge(indics_out, indicator_list, by="shortName")
    return(indics_out)
    }
  })
    
  
  #To do: integrate with new data workflow. 
  observeEvent(input$makeHeatMap, {
    if(!is.null(heatmap_indics())){
      data_files <- paste0(heatmap_indics()$file, "_", heatmap_indics()$year) |> unique()
        for(file in data_files){
        infile <- list.files("Data", file, ignore.case=T, full.names=T) #this differs from the other file loading subroutine in getData - should probably make them consistent.
        if(length(infile)!=0){
         temp <- read.csv(infile)
         if(!exists("data_out")){
           data_out <- temp
         } else {
           #temp <- temp |> select(all_of(c(names(temp)[which(!(names(temp) %in% names(data_out)))]))) #Fix for redundant input. (issue: need id variable name)
           #To do: error handling and dealing with non 1:1 merges here.
           data_out <- merge(data_out, temp)
          }
          }
        }
        if(exists("data_out")){
          indic_shortNames <- unlist(heatmap_indics()$shortName, use.names=F)
          data_out <- data_out |> select(any_of(indic_shortNames)) #Won't throw an error if names are missing)
          if(ncol(data_out) < length(indic_shortNames)){
            indics_missing <- indic_shortNames[which(!(indic_shortNames %in% names(data_out)))]
            showNotification(paste("Variable(s)", paste(indics_missing, collapse=", "), "not found in the dataset"), type="warning")
          }
          varnames <- data.frame(shortName=names(data_out))
          varnames <- merge(varnames, indicator_list |> select(shortName, labelName), by="shortName")
          missing_vars <- NULL
          for(currVar in names(data_out)){
            if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
              missing_vars <- c(missing_vars, currVar)
            } else {
              if(!is.numeric(data_out[[currVar]])){
                #data_out <- data_out |> mutate_at(currVar, list(~ recode(., 'None'='0', 'No'='0', 'Yes'='1')))
                data_out[[currVar]] <- as.numeric(data_out[[currVar]])
                if(all(is.na(data_out[[currVar]])) | all(na.omit(data_out[[currVar]]==0))){
                  missing_vars <- c(missing_vars, currVar)
                }
              }
            }
          }
          if(!is.null(missing_vars)){
            data_out <- data_out |> select(!matches(missing_vars))
            showNotification(paste("Variable(s)", paste(missing_vars, collapse = ", "), "were non-numeric and were removed from the dataset"), type="warning")
          }
          output$heatMap <- renderPlotly(corMat(varnames$shortName, varnames$labelName, data_out))
        }
    }
  })
  
  output$dataPathBox <- renderUI({
    if(req(input$policiesBox2!="None") & is.list(pathway_link) & is.list(indicator_list)){
      pickerInput(NS(id, "pathwaysIn2"), "Choose a pathway (optional)", choices=polic_Names[[input$policiesBox2]], options=list(style="selectize-input"), choicesOpt=list(disabled=polic_activ[[input$policiesBox2]]))
    }
  })
  

  
  output$indicsBox <- renderUI({
    req(indics())
    selectInput(NS(id, 'yvarSelect'), HTML("<b>Select Y Variable</b>"), choices=indics()) #, size=length(indics) , selectize=F)) 
  })

  output$corrsBox <- renderUI({
    req(indics())
    selectInput(NS(id,'xvarSelect'), HTML('<b>Select X Variable</b>'), choices=indics()) #, size=length(indics), selectize=F))
  })
  
  aggs_list <- reactive({input$groupsChk}) #ALT Note: Right now this is an unnecessary step, but if we ever end up needing to have multiple disaggregation criteria, it's probably better to do it this way.

  all_data <- reactive({
    req(input$xvarSelect, input$yvarSelect)
    outdata <- getData(indic_inventory, xvars=input$xvarSelect, yvars=input$yvarSelect, indicator_list, aggs_list=aggs_list(), adm_levels=adm_levels, source_call="explorer", drop_0s = input$yChk)
    outdata <- outdata[[admMergeVar()]] |> filter(year %in% input$yearBtn) |> select(-year) #To fix.
    #Move to get data
    if(nzchar(aggs_list())) {
    if(with(outdata, exists(aggs_list()))) { 
      if(!is.factor(outdata[[aggs_list()]])) {
      flevels = str_split(groups_list[which(groups_list$varName==aggs_list()),]$Levels, ",") |> unlist()
      flabels = str_split(groups_list[which(groups_list$varName==aggs_list()),]$Labels, ",") |> unlist()
      outdata[[aggs_list()]] <- factor(outdata[[aggs_list()]], levels=flevels, labels=flabels)
      }
    }
    }
    #Should mean be hard coded here?
    outdata |> select(any_of(c(admMergeVar(), aggs_list(), "shortName", "Mean"))) |> 
      pivot_wider(names_from="shortName", values_from="Mean") |>
      na.omit()
  }) |> 
    bindCache(input$xvarSelect, input$yvarSelect, admMergeVar(), aggs_list(), input$yChk) |> 
    bindEvent(input$submitBtn) 
  
  labs <- reactive({
    list(
    xlab=indicator_list$labelName[indicator_list$shortName==input$xvarSelect],
    ylab=indicator_list$labelName[indicator_list$shortName==input$yvarSelect],
    aggs_lab=if(aggs_list()!="") groups_list$shortName[groups_list$label==aggs_list()] else NULL,
    xAxis = indicator_list$axisName[indicator_list$shortName==input$xvarSelect],
    yAxis = indicator_list$axisName[indicator_list$shortName==input$yvarSelect],
    xTitle = sprintf("Map of %s by %s", indicator_list$labelName[indicator_list$shortName ==input$xvarSelect], str_to_title(admMergeVar())),
    xUnits = indicator_list$units[indicator_list$shortName==input$xvarSelect],
    yTitle = sprintf("Map of %s by %s", indicator_list$labelName[indicator_list$shortName == input$yvarSelect], str_to_title(admMergeVar())),
    yUnits = indicator_list$units[indicator_list$shortName==input$yvarSelect]
    )
  }) |> bindEvent(input$submitBtn)
  

  output$scatterPlot <- renderPlot({
    req(all_data())
    res <- eval(parse_expr(sprintf("with(all_data(), cor.test(%s, %s))", input$xvarSelect, input$yvarSelect)))
    if(is.na(res$p.value)){
      res_out <- ""
      
    } else {
      if(res$p.value <= 0.01){ 
        adj="<span style='color: #44ce1b;'>very high</span>"
      } else if(res$p.value <= 0.05) {
        adj="<span style='color: #bbdb44;'>high</span>"
      } else if(res$p.value <= 0.1) {
        adj="<span style='color: #f7e379;'>moderate</span>"
      }  else if(res$p.value <= 0.2) {
        adj="<span style='color: #f2a134;'>low</span>"
      } else {
        adj = "<span style='color: #e51f1f;'>no</span>"
      }
      
      res_out <- sprintf("<span style='font-size: 20px;'>There is %s%% (%s%% - %s%%) correlation between <span style='color: #0a2167;'><b>%s</b></span> and <br><span style='color: #0a2167;'><b>%s</b></span>. There is %s confidence in this result.</span>", 
                         round(res$estimate[[1]]*100, 1), round(res$conf.int[[1]]*100, 1), round(res$conf.int[[2]]*100, 1),
                         labs()$xlab, labs()$ylab, adj)
    }
    if(!is.null(labs()$aggs_lab)){
      makeScatterGrps(all_data(),input$xvarSelect,input$yvarSelect,aggs_list(),labs()$xlab,labs()$ylab,labs()$aggs_lab, res_out) 
    } else {
      makeScatter(all_data(),input$xvarSelect,input$yvarSelect,labs()$xlab,labs()$ylab, res_out) 
    }
    }) |> bindEvent(input$submitBtn)
  
  #To fix
  bins <- reactive({
    req(all_data())
    min(30, length(unique(all_data()[[admMergeVar()]])))
  })
  
  corrTab <- reactive({
    req(all_data())
    flexTableOut(data=all_data(), adm_level=admMergeVar(), aggs_list= if(aggs_list()=="") NULL else aggs_list(), input$xvarSelect, labs()$xlab, labs()$aggs_lab) #todo: undo hardcoding of mean
  }) |> bindEvent(input$submitBtn) 
  
  indicatorTab <- reactive({
    req(all_data())
    #function(data=all_data(), adm_level=admMergeVar(), aggs_list=NULL, measurevars, lab, groupslab)
    flexTableOut(data=all_data(), adm_level=admMergeVar(), aggs_list=if(aggs_list()=="") NULL else aggs_list(), input$yvarSelect, labs()$ylab, labs()$aggs_lab)
  }) |> bindEvent(input$submitBtn)
  

  
  output$indicatorHist <- renderUI({
    req(indicatorTab())
    navset_card_pill(
    placement="above",
    nav_spacer(),
    nav_panel(title=icon("chart-simple"), 
              renderPlot(indicatorHist())),
    nav_panel(title=icon("table"), renderUI(indicatorTab()[[2]])),
    card_footer(downloadButton(NS(id, "dlIndicHist"), label="", icon=icon("file-arrow-down"))) #this isn't the correct syntax, but the documented way to add a footer doesn't work so we'll just do it this way and eat the warnings.
  )
    }) |> bindEvent(input$submitBtn)
  
  output$corrHist <- renderUI({
    req(corrTab())
    navset_card_pill(
    placement="above",
    nav_spacer(),
    nav_panel(title=icon("chart-simple"), renderPlot(corrHist())),
    nav_panel(title=icon("table"), renderUI(corrTab()[[2]])),
    card_footer(downloadButton(NS(id, "dlCorrHist"), label="", icon=icon("file-arrow-down"))) 
  )
  }) |> bindEvent(input$submitBtn)
  
  #Filename may vary depending on what the user is doing on the input side, to fix.
  output$dlCorrHist <- downloadHandler(
    filename = function(){
      paste0("data-", input$yearBtn, "-", xname(), ".csv")
    },
    content = function(file){
      req(corrTab())
      write.csv(corrTab()[[1]], file, row.names=F)
    }
  )
  
  
  output$dlIndicHist <- downloadHandler(
    filename=function(){
      paste0("data-", input$yearBtn, "-", yname(), ".csv")
    }, 
    content=function(file){
      req(indicatorTab())
      write.csv(indicatorTab()[[1]], file)
    }
  )
  
  
      
  indicatorHist <- reactive({
  makeHist(all_data(), input$xvarSelect, bins(), aggs_list(), labs()$xAxis, labs()$xlab, labs()$aggs_lab)
  }) |> bindEvent(input$submitBtn)
          
  
  corrHist <- reactive({
    makeHist(all_data(), input$yvarSelect, bins(), aggs_list(), labs()$yAxis, labs()$ylab, labs()$aggs_lab)
  }) |> bindEvent(input$submitBtn)
          
  
  output$downloadRawShort <- downloadHandler(
    filename="raw_data_export.csv",
    content=function(file){
      aggs_list = input$groupsChk
      if(aggs_list==""){
        aggs_list <- NULL
      }
      #The indicator_list part of this call is not strictly necessary but I'm keeping it in for clarity (for now)
      rawData <- getData(indic_inventory, xvars=input$xvarSelect, yvars=input$yvarSelect, indicator_list=indicator_list, aggs_list=aggs_list(), adm_levels=adm_levels[adm_levels$admLevel==admMergeVar(),], drop_0s = input$yChk)
      write.csv(rawData[[admMergeVar()]], file, row.names=F)
    }
  )
  
  output$downloadRawLong <- downloadHandler(
    filename="raw_data_export.csv",
    content=function(file){
      aggs_list = input$groupsChk
      if(aggs_list==""){
        aggs_list <- NULL
      }
      rawData <- getData(indic_inventory, xvars=indics(), indicator_list=indicator_list, aggs_list=aggs_list(), adm_levels=adm_levels[adm_levels$admLevel==admMergeVar(),], drop_0s = input$yChk) #Drop 0s won't do anything because we treat it all as xvars
      write.csv(rawData[[admMergeVar()]], file, row.names=F)
    }
  )
  
  
  
  
    }
    
  })
}