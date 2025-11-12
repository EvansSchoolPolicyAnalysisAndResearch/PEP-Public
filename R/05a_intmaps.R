mapUI <- function(id, year_list, adm_levels, territory_names, indicators){
  tagList(
    div(style = "border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin: 10px 0; background-color: #fafafa;",
        #h5("Map 1"),
        # First row of controls
        fluidRow(
          #Might do it thru map?
          column(4, selectInput(NS(id, "adm_level"), "Level:", choices={
            choicelist <- adm_levels$level
            names(choicelist) <- adm_levels$admLabel
          choicelist
          })),
          column(6, uiOutput(NS(id, "indicatorsout"))),
          column(3, selectInput(NS(id, "outcome1"), "Measurement:", 
                                choices = c("Mean", "Total", "Trend"), width = "100%")), #Some way to communicate value for point obs #May need to undo hard coding here.
          conditionalPanel("input.outcome1!='Trend'",
          column(3, selectInput(NS(id, "year1"), "Year:", 
                                choices = year_list, width = "100%"))
        )),
        # Second row of controls
        fluidRow(
          plotlyOutput(NS(id, "map1"))
          #column(4, uiOutput(NS(id, "territorySelect"))) 
                 #selectInput(NS(id, "territory1"), "Territory:", 
                                #choices = territory_names, selected=territory_names[[length(territory_names)]], width = "100%")), #Take last element.
          #column(4, uiOutput(NS(id, "group_ui1"))),
          #column(4, uiOutput(NS(id, "checkbox_ui1"))),
        ),
        fluidRow(
          column(12, 
            uiOutput(NS(id,"hover_info1"))
          )
        )
    )
  )
}

#Experiment to see if this results in easier-to-follow code. Should turf to scripts if it seems better. Not sure how often it'll be reused.
getadmLabel <- function(adm_levels=adm_levels(), target=adm_level()) {
  adm_levels |> 
  filter(admLevel == target) |>
    pull(admLabel) |>
    first()
}

mapServer <- function(id, dfs, shps, adm_levels, indicator_list){
  #adm_levels should already be filtered by the available shapefiles, I think. 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_state1 <- reactiveVal(NULL)
    trace_map1 <- reactiveVal(NULL)
    adm_level <- reactive({
      as.numeric(input$adm_level) #to fix
    })
    
    adm_name <- reactive({
      adm_levels$admLabel[[adm_level()]]
    })
    
    df <- reactive({
      dfs[[paste0("adm", adm_level())]] #also to fix
    })
    
    output$indicatorsout <- renderUI({
      req(df())
      indicators <- df() |> ungroup() |> select(shortName) |> distinct() |> inner_join(indicator_list)
      selectInput(ns("indicator"), "Select Variable:", choices={
        choicelist <- indicators$shortName 
        names(choicelist) <- indicators$label
        choicelist
      })
    })
    
    indicator_name <- reactive({
      indicator_list$label[indicator_list$shortName == input$indicator][1] #Might be simplifiable. 
    })
    
    output$territorySelect <- renderUI({
      #Desired behavior here: If the user selects "county" (i.e. there's a selectable admin level _above_ the currently selected admin level)
      #Pop up with a list of admin units based on the values available in the df and refuse to do anything until the user selects one (so put NA at the top)
      #This gets really messy if someone wants to do more than two admin levels though
      if(adm_level() > min(adm_levels$level)){
        adm_container <- adm_levels$admLabel[[adm_levels()-1]]
        selectInput(ns("territory"), sprintf("Select %s", adm_container), choices=c("", unique(df()[[paste0("adm", adm_level()-1)]])))
      } else {
        return(NULL)
      }
    })
    
    
    #This ends up being some messy reactivity because the graph goes input admin level |> output dataset |> grab available variables from dataset and use that to make another dropdown |> pull in selected variables and stats |> make maps
    #I think it enables us to isolate the (potentially, depending on admin level) computationally intensive pieces, though. 
    
    mapdf <- reactive({
      req(df(), input$indicator, input$outcome1)
      mapdf <- df() |> filter(shortName==input$indicator)
      #adm level filtering here
      
      if(input$indicator!="Trend" & length(unique(df()$year))>1){
          #Need message to user if they request trend but only 1 year is present
        #Also need way to handle if df is grouped w/ a grouping variable (not a current problem because it isn't an option here, but it might be in the future)
        mapdf <- mapdf |> filter(year==input$year1)
      } 
      return(mapdf)
    })

    # Render Map 1
    output$map1 <- renderPlotly({
      req(input$indicator)
      indic <- input$indicator
      
      if(input$outcome1!="Trend") {
      shp <- shps[[adm_level()]] |> left_join(df() |> 
                                              filter(shortName==indic, year==input$year1) |> 
                                              select(matches(c(input$outcome1, adm_level())))) 
      } else {
        minyear <- min(df()$year)
        maxyear <- max(df()$year)
        tempdf <- df() |> 
          filter(shortName==indic, (year==minyear | year==maxyear)) |>
          select(matches(c("Mean", adm_level(), "year"))) |> #Arbitrarily choose means for trend, needs to be made explicit.
          pivot_wider(id_cols=all_of(adm_level()), names_from="year", values_from="Mean", names_glue="{.value}{year}")
        tempdf$Trend <- (tempdf[[paste0("Mean", maxyear)]] - tempdf[[paste0("Mean",minyear)]])/tempdf[[paste0("Mean",minyear)]]  #Eventually "Mean" will be substitutable.
        shp <- shps[[adm_level()]] |> left_join(tempdf)  
      }
     plot_obj <- create_intMap(ns("map1"), shp, paste0("adm", adm_level()), adm_name(), input$outcome1) # To fix.      
     trace_map1(attr(plot_obj, "trace_map"))
      plot_obj
    })

    #Map 1 hover info
    output$hover_info1 <- renderUI({
      hover <- event_data("plotly_hover", source=ns("map1"))
      if (is.null(hover)) {
        return(HTML(paste0("<em>Hover over a ", tolower(adm_name()), " for details</em><br><br><br>")))
      } 
      curve_num <- hover$curveNumber[1]
      if(curve_num <= length(trace_map1())) {
      adm_id <- trace_map1()[[as.character(curve_num)]]
      current_df <- df()
      adm_col <- adm_level()
      hover_df <- current_df[current_df[[adm_col]] == adm_id & 
                               current_df$shortName == input$indicator & 
                               current_df$year == input$year1, ]
      value <- hover_df[[input$outcome1]][1]
      value <- ifelse(is.na(value), "N/A", format(round(value, 1), big.mark = ","))

      lines <- c(
        paste0("<strong>", adm_id, "</strong>"),
        #paste0("<strong>Year:</strong> ", input$year1), #Is year really necessary?
        paste0("<strong>Variable:</strong> ", indicator_name()),
        paste0("<strong>", input$outcome1, ":</strong> ", value)
      )
      return(HTML(paste(lines, collapse = "<br>")))
      }
    })
  })
}

create_intMap <- function(source_name, plot_data, adm_level, adm_name, statistic){ #To fix show colleges.
  # Join data
  plot_data$display_value <- plot_data[[statistic]]
  plot_data <- plot_data |> mutate(display_value=ifelse(
    is.na(display_value), "No data", format(signif(display_value, 4), big.mark=",", trim=T)
  ))
  plot_data$hover_text <- paste0(plot_data[[paste0(adm_level, "_name")]], ": ", plot_data$display_value) #There's probably a better way to do this, too. 
  plot_data$adm_key <- as.character(plot_data[[adm_level]]) #|> #as.character(st_iso)  # Keep ISO for split
  plot_data <- dplyr::arrange(plot_data, !!sym(adm_level))  # Important: consistent ordering
  # Store the trace mapping as an attribute (for hover)
  trace_map <- setNames(plot_data[[adm_level]], 0:(nrow(plot_data)-1))

  if(min(plot_data[[statistic]]) < 0) {
    map_colors <- c("brown4", "#f5f5f5", "#4b2e83")
  } else {
    map_colors <- c("#f5f5f5", "#4b2e83")
  }
  # Create plotly map
  map_out <- plot_ly(source = source_name) %>%
    add_sf(
      data = plot_data,
      split = ~adm_key,  # or ~county_key for state plot
      color = ~get(statistic),
      colors = map_colors,
      stroke = I("#ffffff"),
      span = I(1.5),  # Border thickness
      text = ~hover_text,
      hoverinfo = "text",
      showlegend = FALSE,
      alpha = 1  # Full opacity
    ) %>%
    colorbar(
      title = "Value",
      len = 0.5,        # Length as fraction of plot height (0.5 = 50%)
      thickness = 15,   # Width in pixels
      x = 1.02,         # Position from left edge
      xpad = 10         # Padding from plot
    ) %>%    
    layout(
      hovermode = "closest",
      autosize = TRUE,
      xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE,
                   showticklabels = FALSE, showline = FALSE, fixedrange = FALSE),
      yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE,
                   showticklabels = FALSE, showline = FALSE, fixedrange = FALSE)
    ) %>%
    config(displayModeBar = TRUE)
  
 
  # Store trace mapping as an attribute
  attr(map_out, "trace_map") <- trace_map
  
  return(map_out)
}