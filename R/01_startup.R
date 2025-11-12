suppressWarnings(
  suppressMessages({
    library(shiny)
    library(shinyBS)
    library(tidyr)
    library(shinythemes)
    library(tools)
    library(ggplot2)
    library(dplyr)
    #library(cowplot) #Old dependencies not currently in use; simplifying this list is on the to do list.
    library(stringr)
    library(DT)
    library(glue)
    library(rlang)
    library(shinyWidgets)
    library(sf)
    #library(gridExtra)
    library(ggdist)
    #library(openxlsx)
    library(purrr)
    #library(rintrojs)
    library(corrplot)
    library(plotly)
    library(bslib)
    library(thematic)
    #library(ragg) #for raster output
    library(viridis)
    library(heatmaply)
    library(shinyjs)
    library(reshape2)
    library(ggtext)
    #library(leaflet)
    library(terra)
    library(tidyterra)
    library(flextable)
  }))

#instrument_list <- tryCatch(read.csv("Update/instrument_list.csv"),
#error=function(e){return(NULL)})
import_data <- function(path) {
  df <- tryCatch(read.csv(path),
                 error=function(e){return(NULL)})
  if(is.null(df)) {
    return(NULL)
  } else if(nrow(df)==0) { #if the templates are here
    return(NULL)
  } else {
    return(df)
  }
}

indicator_list <- import_data("Update/indicator_list.csv")
pathway_link <- import_data("Update/pathway_link.csv")
groups_list <- import_data("Update/grouping_vars.csv")
policy_path <- import_data("Update/pathway_table.csv")
ext_sources <- import_data("Update/secondary_sources.csv")
ref_sources <- import_data("Update/evidence_list.csv")
adm_levels <- import_data("Update/adm_levels.csv")



shpfiles <- list.files("Spatial", "\\.shp$")
if(length(shpfiles)>0) { 
  shpnames <- str_extract(shpfiles, "adm[0-9]+")
}

shps <- lapply(shpfiles, FUN=function(x){
  tryCatch(st_read(paste0("Spatial/", x), quiet=T), error=function(e) return(NULL))
})

names(shps) <- shpnames

if(!is.null(adm_levels)){
  adm_levels <- adm_levels |> mutate(level=as.numeric(str_extract(admLevel, "adm([0-9]+)", group=1)))
  #adm_levels[adm_levels$varName=="NA"] <- NA
} else { #Build a generic version from shapefiles if unavailable.
  if(exists("shpnames")) {
    adm_levels <- data.frame(level=seq(1,length(shpnames)), admLevel=shpnames, admLabel=shpnames, shortName=shpnames)
  }
}

if(is.list(indicator_list)){
  colnm_indic <- c("shortName", "category", "label","axis", "units") #Again, only what we minimally need to operate. Note we're moving flags to a new sheet
  colnm_opt <- c("numerator", "denominator", "weight", "caption", "w_lower","w_upper") #Easier to tack these back on if they've been removed rather than continually check for them.
  if(any(!(colnm_indic %in% names(indicator_list)))){
    indicator_list <- NULL
    #To do: error handling
  } else if(any(!colnm_opt %in% names(indicator_list))){
    for(colnm in colnm_opt){
      if(!with(indicator_list, exists(colnm))) {
        indicator_list[[colnm]] <- NA
      }
    }
  }
}

#To do: Add error handling.
dataset_list <- list.files("Data", pattern="*.csv")
dataset_list <- dataset_list[which(!str_detect(dataset_list, " "))]
year_list <- sapply(dataset_list, FUN=function(x){str_extract(x, "[0-9]{4}")}) |> unique() |> na.omit()

if(length(year_list) > 0) {
  if(exists("file_inventory")) rm(file_inventory) #Testing kludge - this isn't loaded from a file so it can linger in the global environment. 
  for(year in year_list){
    names <- lapply(dataset_list[which(str_detect(dataset_list, year))], function(x){
      dat <- read.csv(paste0("Data/",x), nrows=1)
      outdf <- data.frame(shortName=names(dat))
      outdf$file <- str_extract(x, sprintf("([aA-zZ]+)_%s", year), group=1)
      #check for properly named admin levels first, then use the df 
      if(!is.null(adm_levels)){
        adm_merge <- merge(adm_levels, outdf)
        if(nrow(adm_merge > 0)){
          outdf$admLevel <- adm_merge$admLevel[adm_merge$level==max(adm_merge$level)] #Not sure if it's inefficient to have this repeated for every variable but I'm leaving it for now.
        }
      }
      outdf
    })
    names <- do.call("rbind", names)
    names$year <- year
    names <- distinct(names)
    #names <- unlist(names) |> unique()
    if(!exists("file_inventory")){
      file_inventory <- names
    } else {
      file_inventory <- bind_rows(file_inventory, names)
    }
  }
} else {
  year_list <- NULL
}

#File inventory is used in the diagnostics so the user can see all available variables
if(exists("file_inventory") & !is.null(indicator_list)){
  indic_inventory <- inner_join(file_inventory, data.frame(shortName=indicator_list$shortName))
} else {
  indic_inventory <- NULL
}


#Fix logic here.
if(is.list(pathway_link)){
  colnm_link <- c("pathwayID", "goalName","shortName")
  if(all(colnm_link %in% names(pathway_link))){
    pathway_link <- pathway_link |> distinct() #Bad input protection
    goalNames <- str_to_title(unique(pathway_link$goalName))
    indicatorCategories <- pathway_link |> select(goalName, shortName) |> distinct()
  } else {
    goalNames <- NULL
    indicatorCategories <- NULL
  }
} else {
  goalNames <- NULL
  indicatorCategories <- NULL
}

if(is.list(groups_list)){
  colnm_grps <- c("varName","label","shortName","Levels","Labels","level") #need to fix names here
  if(any(!(colnm_grps %in% names(groups_list)))){
    groups_list <- NULL
  }
}

if(is.list(policy_path)){
  colnm_path <- c("pathwayID", "goalName") #Minimum requirement for this to function
  if(any(!(colnm_path %in% names(policy_path)))){
    policy_path <- NULL
    #Error handling here
  }
}

if(!is.list(policy_path)){
  policy_path <- NULL
  polic_Names <- NULL
  polic_activ <- NULL
  pathwaysDT <- NULL
} else {
  long_goalnames <- tryCatch(read.csv("Update/pathway_names.csv"), 
                             error=function(e){return(F)})
  if(is.list(long_goalnames)){
    colnm_goalnames <- c("goalName", "Policy.Goal")
    if(!all(colnm_goalnames %in% names(long_goalnames))){
      long_goalnames <- F
    }
  }
  if(is.list(long_goalnames)){
    long_goalnames <- long_goalnames |> select(all_of(colnm_goalnames)) #Remove extraneous info if it exists
    policy_path <- dplyr::left_join(policy_path, long_goalnames, by="goalName") |> 
      mutate(Policy.Goal=ifelse(is.na(Policy.Goal), goalName, Policy.Goal)) |> #Address empties/non-matches
      filter(!is.na(goalName)) |> #Remove anything still not valid
      relocate(Policy.Goal, .after=goalName) #Cosmetic, I think this gets dropped
  } else {
    policy_path <- policy_path |> mutate(Policy.Goal = goalName) |> filter(!is.na(goalName))
  }
  policy_path[is.na(policy_path[,5]),5] <- "Other" #Referring to the "tool" column by index to allow the name to float. 
  pathwaysDT <- policy_path |> select(-c(pathwayID, goalName))
  pathway_names <- unique(policy_path$Policy.Goal)
  short_Pathways <- unique(policy_path$goalName)
  names(pathwaysDT) <- str_replace_all(names(pathwaysDT), "\\.", " ")
  
  polic_Names <- lapply(short_Pathways, FUN=function(x){
    policy_path_sub <- policy_path |> filter(goalName==x)
    inst_names <- unique(policy_path_sub[,5])
    
    temp_list <- lapply(inst_names, FUN=function(y){
      tempnames <- policy_path_sub[,5][policy_path_sub[,5]==y]
      tempvals <- as.list(policy_path_sub$pathwayID[policy_path_sub[,5]==y])
      
      names(tempvals) <- tempnames
      return(tempvals)
    })
    names(temp_list) <- inst_names
    temp_list <- c(list(`All Instruments`=0), temp_list)
    temp_list <- temp_list[nzchar(names(temp_list))]
    return(temp_list)
  })
  polic_activ <- lapply(short_Pathways, FUN=function(x){
    policy_path_sub <- policy_path |> filter(goalName==x)
    inst_names <- unique(policy_path_sub[,5]) #Doing this the same way as above so that everything lines up
    temp_list <- lapply(inst_names, FUN=function(y){
      tempnames <- policy_path_sub[,5][policy_path_sub[,5]==y]
      tempvals <- policy_path_sub$pathwayID[policy_path_sub[,5]==y]
      return(sapply(tempvals, FUN=function(z){
        nrow(pathway_link |> filter(pathwayID==z)) == 0
      }))
    })
    names(temp_list) <- inst_names
    temp_list <- temp_list[nzchar(names(temp_list))]
    temp_list <- unlist(temp_list)
    temp_list <- c(FALSE, temp_list) # first item
    return(temp_list)
  })
  names(polic_Names) <- short_Pathways
  names(polic_activ) <- short_Pathways
}

#Not currently necessary, to fix
#if(is.list(ref_sources)){
  #names(ref_sources) <- c("Policy Goal", "Citation", "Link") # ALT temp kludge till I can make this more robust
#}

#Not currently necessary, to fix.
#if(is.list(ext_sources)){
#  colnm_ext <- c("Source","Relevant.Variables", "Location")
#  if(any(!(colnm_ext %in% names(ext_sources)))){
#    ext_sources <- NULL
#  } else {
#    if(!interactive()){
#     ext_sources$Location <- sapply(ext_sources$Location, FUN=function(x){as.character(tags$a(x, href=x))})
#    }
#    names(ext_sources) <- str_replace_all(names(ext_sources), "\\.", " ")
#  }
#}

#Might not be necessary?
#Todo: Error handling
if(exists("shps")){
  territory_names <- lapply(names(shps), FUN=function(x){
    territory_names_in <- shps[[x]] |> st_drop_geometry()
    territory_names <- territory_names_in[[x]]
    names(territory_names) <- territory_names_in[[paste0(x, "_name")]]
  })
}

#This is what we want every module to have access to by default - it isn't everything 
#(we have some tables that are being held out because they're not needed for certain modules)
#same with shapefiles
globals <- list(
  adm_levels=adm_levels,
  indicator_list=indicator_list, 
  indic_inventory=indic_inventory,  
  indicatorCategories=indicatorCategories,
  pathway_link=pathway_link,
  polic_Names=polic_Names,
  polic_activ=polic_activ,
  year_list=year_list
) 

#Eventually this will come in on the configs, hard coding for now. 
wins_opts <- list(
  lbound=1,
  ubound=99,
  components=c("num","denom","final"),
  method="Tails", 
  weighted=T
)



#evidence: polic_Names, polic_activ, indicatorCategories, indicator_list, indic_inventory, pathway_link, adm_levels, year_list, shps
#comps:    groups_list, pathway_link, indicator_list, indic_inventory, polic_Names, polic_activ, adm_levels, territory_names

#new diagnostics: file_inventory, indic_inventory, indicator_list, pathway_link, policy_path
#old diagnostics: dataset_list=NULL, indic_inventory, instrument_list=NULL, indicator_list, pathway_link, policy_path


#Outputs after this script runs:
#adm_levels - table of admin levels and reference columns in data (if needed; we prefer just calling everything adm0..admN)
#ext_sources - table of external sources ("Additional Sources for Evaluating Options" tab)
#groups_list - table of grouping variable metadata
#indicator_list - table of measure variable metadata
#indicatorCategories - list for arranging variables in dropdown boxes
#long_goalnames - list of "nice" names for policy table tabs
#pathway_link - connector for policy pathways/tools/instruments to variables
#pathwaysDT - the policy goal data table
#polic_active - which policies have variables?
#polic_Names - list of policies for dropdowns, filtered with polic_active
#shps - shapefiles
#ref_sources - reference for the policy goals table
#territory_names (probably not needed) - list of adm units for dropdowns (to do)
#year_list - list of survey years available. 