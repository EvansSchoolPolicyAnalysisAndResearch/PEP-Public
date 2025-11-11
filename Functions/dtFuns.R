getTrends <- function(data, var, meas) {
  max_year <- max(data$year)
  min_year <- min(data$year)

if(min_year!=max_year){
  diff <- data_out |> pivot_wider(names_from=year, values_from=meas)
  #I think we can assume the data will be reliably ordered by year but we should probably use column names here.
  diff[,4] <- diff[,3]-diff[,2]
  names(diff)[[4]] <- var
  return(diff)
}
  
}

filterFlagTable <- function(dt_out, pathway_link, pathwayTarget, indicator_list){
  if(pathwayTarget!=0){
    indics_out <- pathway_link |> filter(pathwayID==pathwayTarget) |> select(shortName) |> distinct()
    dt_out <- inner_join(dt_out, indics_out, by="shortName")
  } 
  return(dt_out)
}


filterVarTable <- function(dt_out, pathway_link, pathwayTarget, indicator_list, stat){
  if(pathwayTarget!=0){
    indics_out <- pathway_link |> filter(pathwayID==pathwayTarget) |> select(shortName) |> distinct()
    dt_out <- inner_join(dt_out, indics_out, by="shortName")
  } 
  dt_out <- dt_out |> 
    left_join(indicator_list) |>
    select(shortName, labelName, year, units, matches(stat))
  if(stat=="Total"){
    dt_out <- dt_out |> filter(units!="ratio") #Exclude ratios from totals because they're already counted in a different indicator.
  }
  
  for(shortName in unique(dt_out$shortName)){
    poprow <- dt_out[dt_out$shortName==shortName,]
    if(isTRUE(any(poprow$units=="boolean"))) {
      if(stat=="Total") poprow$units <- "Number"
      if(stat=="Mean") poprow$units <- "Percent"
    }
    if(isTRUE(all(poprow$units=="kg")) & isTRUE(any(poprow[[stat]] > 1000))){
      poprow[[stat]] <- poprow[[stat]]/1000
      poprow$units <- "Tonnes"
    } else if(isTRUE(any(poprow[[stat]] > 1000000))) {
      poprow$units <- paste(poprow$units, " (MM)")
      poprow[[stat]] <- poprow[[stat]]/1000000
    }
    dt_out[dt_out$shortName==shortName,] <- poprow
  }
  
  dt_out <- dt_out |> filter(year==min(dt_out$year) | year==max(dt_out$year)) |>
    pivot_wider(id_cols=all_of(c("shortName", "label","units")), names_from="year", values_from=stat, names_glue="{year} {.value}") |>
    rename(Variable=label, Units=units)
  
  
  dt_out <- data.frame(dt_out)
  names(dt_out) <- str_replace_all(names(dt_out), "X", "")
  names(dt_out) <- str_replace_all(names(dt_out), "[.]", " ")
  dt_out$Trend <- signif((dt_out[,5]-dt_out[,4])/dt_out[,4], 4)
  dt_out[,4:5] <- format(signif(dt_out[,4:5], 4), big.mark=',', justify="right", scientific=F, digits=4, nsmall=0, drop0trailing=T)
  
  return(dt_out)
}