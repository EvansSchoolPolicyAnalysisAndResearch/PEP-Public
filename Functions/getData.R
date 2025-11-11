not_all_na <- function(x) any(!is.na(x)) #Helper function

#can delete?
getIndics <- function(pathway_link, indicator_list, indic_inventory, policy, pathway, obsyear, cats=F){
  if((pathway %||% "0")!="0"){ 
    indics_out <- pathway_link %>% filter(goalName==policy, pathwayID==pathway) %>% merge(., indicator_list, by="shortName") 
    
  } else {
    indics_out <- pathway_link %>% filter(goalName==policy) %>% merge(., indicator_list, by="shortName")
  }
  if(cats==T){
    indics_out <- merge(indics_out, indic_inventory %>% filter(year %in% obsyear), by="shortName") %>% select(shortName, category, label) %>% distinct()  %>% arrange(category)
    indics <- lapply(unique(indics_out$category), FUN=function(x){
      sub_indics <- indics_out %>% filter(category==x)
      temp_indics <- as.list(sub_indics$shortName)
      names(temp_indics) <- sub_indics$label
      return(temp_indics)
    })
    names(indics) <- unique(indics_out$category)
  } else {
    indics_out <- merge(indics_out, indic_inventory %>% filter(year %in% obsyear), by="shortName") %>% select(shortName, label) %>% distinct()
    indics <- as.list(indics_out$shortName)
    names(indics) <- indics_out$labelName 
  }
  return(indics)
}



getRawData <- function(files, indicators, aggs_list="year", adm_levels) {
  years <- unique(files$year)
  merge_errors <- data.frame(file=character(), error=character())
  
  #df <- tryCatch(
  df <- do.call("bind_rows", 
   lapply(years, FUN=function(year){
    #to do: add admin level aggregation here. 
    filenames <- files$file[files$year==year] |> unique()
    for(filename in filenames) {
      infile <-  tryCatch(read.csv(sprintf("Data/%s_%s.csv", filename, year)) |> select(any_of(na.omit(c(adm_levels$shortName, indicators$shortName, indicators$denominator, indicators$numerator, indicators$weight)))), #Idk if this is the best way to do this.
                          error=function(e){
                            merge_errors <- merge_errors |> add_row(file=filename, error="Unable to load file or variables were missing")
                          })
      if(is.null(infile)) {
        next
      } else {
        if(!exists("outfile")){
          outfile <- infile
        } else {
          outflag <- F
          #id_vars_sub <- which(names(infile) %in% names(outfile))
          outfile_temp <- tryCatch(full_join(outfile, infile, relationship="one-to-one"), 
                                   warning=function(w) {
                                     merge_errors <- merge_errors |> add_row(file=filename, error="File relationship wasn't one-to-one")
                                     outflag <- T
                                     return(NULL)
                                   },
                                   error=function(e) {
                                     merge_errors <- merge_errors |> add_row(file=filename, error="Unable to execute join")
                                     outflag <- T
                                     return(NULL)
                                   })
          if(!outflag) outfile <- outfile_temp
        }
      }
    }
    if(exists("outfile")) {
      outfile$year <- year
      
      if(!with(outfile, exists("weight"))){
        weights <- tryCatch(read.csv(sprintf("Data/weights_%i.csv", year)),
                            error=function(e){
                              return(NULL)
                            })
        #Better way to do this, maybe?
        if(!is.null(weights)){
          outflag <- F
          mergeNames <- names(outfile)[which(names(outfile) %in% names(weights))] #Slightly more flexible
          if(length(mergeNames)!=0) {
            outfile_temp <- tryCatch(inner_join(outfile, weights, by=mergeNames, relationship="one-to-one"),
                                warning=function(w) {
                                  merge_errors <- merge_errors |> add_row(file="weights", error="File relationship wasn't one-to-one")
                                  outflag <- T
                                  return(NULL)
                                },
                                error=function(e){
                                  merge_errors <- merge_errors |> add_row(file="weights", error="Other error in file join (ID column data types may be mismatched)")
                                  outflag <- T
                                  return(NULL)
                                })
            if(!outflag) outfile <- outfile_temp else outfile$weight <- 1
          } else {
            merge_errors <- merge_errors |> add_row(file="weights", error="Unable to execute join")
            #showNotification("Error in merging weights file: ID column not found. Unweighted averages will be shown.", type="error")
            outfile$weight <- 1
          }
        } else {
          outfile$weight <- 1
        }
      }
      #This is causing problems - need to problem solve.
      if(!all(aggs_list[aggs_list!="year"] %in% names(outfile)) | !all(na.omit(adm_levels$shortName) %in% names(outfile))){
        groups <- tryCatch(read.csv(sprintf("Data/groups_%s.csv", year)),
                           warning=function(w) {
                             aggs_list <- "year"
                             merge_errors |> add_row(file=sprintf("groups_%s", year), error="Grouping file not found")
                             return(NULL)
                           },
                           error=function(e){
                             #showNotification(paste("Grouping file for the survey", survey, "was not found. No groups were applied."))
                             aggs_list <- "year"
                             merge_errors |> add_row(file=sprintf("groups_%s", year), error="Could not load grouping variables file")
                             return(NULL)
                           })
        if(!is.null(groups)){
          mergeNames <- names(outfile)[which(names(outfile) %in% names(groups))] 
          if(length(mergeNames)==0){ 
            merge_errors <- merge_errors |> add_row(file=paste0("groups_", year), error="No ID variables found in groups file")
          } else {
            outfile <- merge(outfile, groups, by=mergeNames)
          }
        }
      }
      outfile
    }
  }) 
  )
  return(list(df=df,
              merge_errors=merge_errors))
}

get_Accessories <- function(vars, indicator_list){ #To do: Consistent naming
  vardf <- data.frame(shortName=vars)
  indicators_filt <- merge(indicator_list, vardf)
  return(na.omit(c(vars, indicators_filt$numerator, indicators_filt$denominator, indicators_filt$weight)))
}

#Just pass the full indicator list without doing any preprocessing, I think.
getData <- function(files, xvars, yvars=NULL, indicator_list=indicator_list,  aggs_list="", filtervar=NULL, adm_levels, drop_0s=F, winsorize=F) { #wins_opts to implement
  #helper function
  summarize_data <- function(adm_vars, data, aggs_list) {
     #Kinda messy, but this function will effectively do nothing if the adm_vars contain the unit of analysis.
    #This will also result in some issues in situations where you want the nationally representative weights only at the national level. TBD.
      data |> group_by(!!!syms(na.omit(c(adm_vars, aggs_list, "shortName")))) |>   #Na omit is awkward but that's how we get the national stats
        summarize(Mean=weighted.mean(value, w=weight, na.rm=T), Total=sum(value*weight, na.rm=T), Obs=sum(!is.na(value)))
  }
  mindicator_list <- c(xvars, yvars)
  mindicators <- merge(c(xvars,yvars), indicator_list)
  #varlist <- c(mindicators$shortName, mindicators$Numerator, mindicators$Denominator) |> na.omit() |> unique()
  aggs_list <- c(aggs_list[nzchar(aggs_list)], "year") 
  years <- files$year %>% unique()
  out_flag <- F
  exit <- F
  
  #add recursive admin loop here.
  raw_data <- getRawData(files, mindicators, aggs_list=aggs_list, adm_levels=adm_levels) #fix mindicators.
  
  if(is.null(raw_data$df)){
    #To do: error handling  
  } else {
    df <- raw_data$df
  
  if((filtervar %||% "") %in% names(df)){
    df <- df %>% filter(!!sym(filtervar)==1) #Keep only yes for everything. 
  }
  #Fix denominator issues. This is super messy.
  varslist_short <- names(df)[which(names(df) %in% unlist(c(xvars,yvars)))]
  if(length(varslist_short)==0){
    #To do: Error handling
    #showNotification(paste("No variables for the selected policy priority were found in", survey))
  } else {
    for(currVar in varslist_short) {
      #Error handling
      if(!is.numeric(df[[currVar]])) df[[currVar]] <- as.numeric(df[[currVar]])
      if(!(currVar %in% indicator_list$shortName) | all(is.na(df[[currVar]]))){
        varslist_short <- varslist_short[-which(varslist_short==currVar)]
        if(!exists("dropped_vars")){
          dropped_vars <- currVar
        } else {
          dropped_vars <- c(dropped_vars, currVar)
        }
      }
    }
    if(length(varslist_short)==0){ 
        showNotification(paste("Error: Data file", file, "is empty or only contains variables not listed in indicators_list"), type="error")
    } else {
      #To do: prevent multiple winsorization on numerator/denominator variables
      #Winsorization here. 
      if(drop_0s==T){
        df <- df %>% filter(!!sym(yvars)!=0)
      }
        if(nrow(df)>0){  
          #Doing this down here to avoid messing up household data export, although hhdata might or might not have denoms at this point. 
          for(currVar in varslist_short){
            denom <- indicators$denominator[indicators$shortName==currVar]
            wt <- indicators$weight[indicators$shortName==currVar]
            df[[paste0("weight.", currVar)]] <- if(any(names(df) %in% denom)) {
              df$weight*df[[denom]] 
            } else if(any(names(df) %in% wt)) {
              df$weight*df[[wt]]
            } else {
              df$weight
            }
            names(df)[names(df)==currVar] <- paste0("value.", currVar)
          }
          df <- df |> mutate(across(starts_with("weight."), ~replace_na(.x,  0)))
          #If denoms are also target variables, they'll be safe behind "value" and "weight"
          df <- df %>% select(-any_of(na.omit(c(mindicators$denominator, "weight")))) %>% 
            pivot_longer(cols=starts_with(c("value", "weight")), names_to=c(".value","shortName"), names_sep="[.]")
          #kludge here?
          #Replace any names in the dataset with their generic admin identifiers. 
          adm_levels <- adm_levels |> dplyr::arrange(level) #Move to startup
          for(i in 1:nrow(adm_levels)){
            varname <- adm_levels$shortName[i]
            adm_level <- adm_levels$admLevel[i]
            if(with(df, exists(varname))){
              names(df)[names(df)==varname] <- na.omit(adm_levels$admLevel[adm_levels$shortName==varname])
            }
            if(with(df, exists(adm_level))) {
              unitid <- adm_levels$admLevel[i]
              maxLevel <- adm_levels$level[i]
            }
          }
          #End kludge
          adm_levels <- adm_levels |> filter(level <= maxLevel)
          #data_out <- adm_levels$admLevel |> map(summarize_data, data=df, aggs_list=aggs_list)
          data_out <- lapply(adm_levels$level, FUN=function(adm){ #This notation needs help
            adm_vars <- adm_levels$admLevel[adm_levels$level<=adm]
            adm_vars[adm_vars=="adm0"] <- NA #Assuming adm0 is always the highest possible and never in the dataset because it's not needed. Need to evaluate this decision.
            summarize_data(adm_vars, df, aggs_list)
          })
          names(data_out) <- adm_levels$admLevel
        }
      }
    }
  }
  
  ##Need to reincorporate this now that data_out is a list.
  if(!exists("droppedVars")){
    droppedVars <- ""
  }
  if(exists("data_out")){
    return(data_out)
  } else {
    return("")
  }
  
}

winsorize <- function(data, lbound, ubound, varlist=list(num=NA, denom=NA, final=NA), components=c("num","denom","final"), method="Tails", weighted=TRUE) {
  #Assume weighting by default, and generate a temporary weight column if weights are not present
  if(with(data, !exists("weight"))){
    data$weight <- 1
  }
  if(nzchar(varlist$denom)){
    data <- data[data[[varlist$denom]]>0,] #NA management. 
  }
  winsvars <- varlist[components]
  winsvars <- winsvars[nzchar(winsvars)]
  quantiles <- c(lbound, ubound, if(method=="Median") 0.5)
  if(length(winsvars!=0)) {
    for(i in seq(1,3)){
      var <- varlist[[i]]
      if(nzchar(var)){
        if(i==3 & nzchar(varlist$num) & nzchar(varlist$denom)){
          data[[varlist$final]] <- data[[varlist$num]]/data[[varlist$denom]]
        }
        
        if(names(varlist)[[i]] %in% components) {
          if(weighted){
            lims <- weighted_quantile(data[[var]], weights=data$weight, probs=quantiles, na.rm=T)
          } else {
            lims <- quantile(data[[var]], probs=quantiles, na.rm=T)
          }
          data[[var]] <- sapply(data[[var]], FUN=function(x){
            switch(method, 
                   Tails=ifelse(x < lims[[1]] & x!=0, lims[[1]], ifelse(x > lims[[2]], lims[[2]], x)),
                   Median=ifelse(x < lims[[1]] & x!=0, lims[[3]], ifelse(x > lims[[2]], lims[[3]], x)),
                   Trim=ifelse(x < lims[[1]] & x!=0, NA, ifelse(x > lims[[2]], NA, x)))
          })
        }
      }
    }
  }
  return(data)
}