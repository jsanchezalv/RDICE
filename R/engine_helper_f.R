
# Global variables for CRAN check -----------------------------------------

if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      c('prevtime', #compute_outputs and compute_outputs_timeseq
        'evttime',
        '.',
        'pat_id',
        'arm',
        'costs',
        'costs_undisc',
        'qalys',
        'qalys_undisc',
        'lys',
        'lys_undisc',
        'out',
        'number_events',
        'evt_arm',
        'input_list_arm')
    )) 
}

# Initial event list --------------------------------------------------------------------------------------------------------------------------------------

#' Execute the initial time to events and separate the events from other inputs that are stored
#'
#' @param arm_name A character string of the name of the intervention
#' @param input_list_arm A list of simulation inputs
#'
#' @return A named vector of initial event times, and a named vector of other inputs to be stored
#'
#' @examples
#' initiate_evt(arm = "int",input_list_arm = input_list_arm)
#'
#' @keywords internal
#' @noRd


initiate_evt <- function(arm_name){
  position <- which(arm_name==names(init_event_list))

  evts_v <- init_event_list[[position]][["evts"]]
  
  othert_v <- init_event_list[[position]][["other_inp"]]
  list2env(mget(c(evts_v,othert_v),ifnotfound=Inf, envir = parent.frame()$env_input_list_arm), envir= parent.frame()$env_input_list_arm) #initialize

  eval(init_event_list[[position]][["expr"]], envir = parent.frame()$env_input_list_arm) #run script
  
  evttime <- lapply(mget(evts_v,ifnotfound=Inf, parent.frame()$env_input_list_arm),unname) #get event times and make sure they are unnamed
  
  time_data <- list(evttime=evttime,
              othertime = if(!is.null(othert_v)){mget(othert_v,ifnotfound=Inf, parent.frame()$env_input_list_arm)} else{NULL}  #get other inputs times
              )
  
  #Event data
  cur_evtlist <- unlist(time_data$evttime)
  
  return(list(cur_evtlist = cur_evtlist, time_data = time_data$othertime))
}



# Get next event ------------------------------------------------------------------------------------------------------------------------------------------

#' Identify which event to process next from a list of events
#'
#' @param evt_list A list of possible events with event times
#'
#' @return Two lists: one containing the name and time of the next event, the other with the remaining events to be processed
#'
#' @examples
#' get_next_evt(evt_list = env_input_list_arm$cur_evtlist)
#'
#' @keywords internal
#' @noRd

get_next_evt <- function(evt_list){                  # This function identifies which event is to be processed next for each patient, depending on intervention

  if (length(evt_list)>0) {
    
    # min_evt <- which.min(evt_list) #old method

    #select the position in the vector that has the minimum time, adding the priority times to make sure to select the right one,
    # 4x slower than old method
    
    min_evt <- which.min(evt_list + parent.frame()$env_input_list_arm$precision_times[names(evt_list)]) 
    cur_evtlist <- list(out = list(evt = names(evt_list[min_evt]), evttime = evt_list[[min_evt]]), evt_list = evt_list[-min_evt])
  } else {
    cur_evtlist <- NULL
  }


  return(cur_evtlist)
}



# Reaction to Event ---------------------------------------------------------------------------------------------------------------------------------------

#' Evaluates the reactions of the event identified by GetNextEvt
#'
#' @param thisevt A two element list containing the first list from GetNextEvt: evt and evttime
#' @param arm A character string of the name of the intervention currently being processed
#' @param env_input_list_arm A list of simulation inputs
#'
#' @return The updated input list with after the reaction to the event is evaluated
#'
#' @examples
#' react_evt(thisevt="evt1",arm="int",env_input_list_arm=env_input_list_arm)
#'
#' @keywords internal
#' @noRd

react_evt <- function(thisevt,arm){      # This function processes the next event (as identified in the GetNextEvt function)
  # Initial set-up --------------------------
  evt <- thisevt$evt                  # Identify event type
  prevtime <- parent.frame()$env_input_list_arm$curtime                 # Identify time of previous event
  curtime <- thisevt$evttime         # Identify time of next event
  
  if (curtime<prevtime) {
    stop("Time of event '", evt,"': ", round(curtime,4), " is smaller than the time of previous event '", if(!is.list(parent.frame()$env_input_list_arm$evt)){parent.frame()$env_input_list_arm$evt}else{NA}, "': ", round(prevtime,4), ". Arm: ", arm, ", id: ", parent.frame()$env_input_list_arm$i)
  }
  
  assign("curtime",curtime, parent.frame()$env_input_list_arm)
  assign("evt",evt, parent.frame()$env_input_list_arm)
  assign("prevtime",prevtime, parent.frame()$env_input_list_arm)
  assign("arm",arm, parent.frame()$env_input_list_arm)
  
  # Create costs and utilities for event --------------------------------------------------
  evt_arm <- paste(evt,arm,sep = "_")
  
  #Reset instantaneous costs/qalys/others

  if(!is.null(parent.frame()$env_input_list_arm$uc_lists$instant_inputs)){
    assign(parent.frame()$env_input_list_arm$uc_lists$instant_inputs,0,parent.frame()$env_input_list_arm)
  }
  
  if(parent.frame()$env_input_list_arm$accum_backwards){
    if(!is.null(parent.frame()$env_input_list_arm$uc_lists$ongoing_inputs)){
      assign(paste0(parent.frame()$env_input_list_arm$uc_lists$ongoing_inputs,"_lastupdate"),0,parent.frame()$env_input_list_arm)
    }
  }
  

  position <- which(evt==names(parent.frame()$env_input_list_arm$evt_react_list))
  pos_l <- length(position)
  if (pos_l==0 | pos_l>1 ) {
    stop("Reaction to event ", evt, " not recognised or more than one reaction found. Make sure that only one reaction has been defined for the event")    
  }
  
  # Evaluate reaction -------------------------------------------------------
  
  
  eval(parent.frame()$env_input_list_arm$evt_react_list[[position]][["react"]], envir = parent.frame()$env_input_list_arm) #run script
  
}


# Get Inputs for Event -----------------------------------------------------------------------------------------------------------------------------------------------

#' Evaluates the cost/utility/cycle unevaluated expressions to be processed by the simulation engine
#'
#' @param x The specific cost/utility and its type (ongoing, instant...) to be used, created through add_cost/add_util
#' @param ifnull Value to be used if the input has not been defined
#' @param type Identifies what type of input is being used. Can be "cost", "util", "cycle_l" (cycle length) and "cycle_starttime" (starting time of the cycle)
#' @param evt_arm_i The event-intervention identifier to understand which specific input to use, separated by an underscore
#' @param input_list_arm_i  A list of simulation inputs
#'
#' @return A numeric vector of evaluated costs/utilities/cycle lengths/starting times for the specific event and intervention defined
#'
#' @examples
#' get_input(x = input_list_arm$uc_lists$cost_ongoing_list,ifnull=0,type="cost",evt_arm_i="evt1_int",input_list_arm_i=input_list_arm)
#'
#' @keywords internal
#' @noRd

get_input <-  function(x,ifnull=0,type,evt_arm_i =evt_arm, input_list_arm_i=input_list_arm){
  out <- NULL
  items <- x[names(x)==evt_arm_i]
  items_l <- length(items)
  if (items_l==0) {
    out <-  c(out,ifnull)
  } else{
    #If length is 1, then don't do loop as it's slow
    if (items_l==1) {
      out <-  c(out,
                if(is.null(items[[1]][[type]])){
                  ifnull
                } else{
                  eval(items[[1]][[type]],input_list_arm_i)
                } #lazy eval will give error on null, so just put 0 in that case
      )
      #otherwise do loop per item
    } else{
      for (i in 1:items_l) {
        out <-  c(out,
                  if(is.null(items[[i]][[type]])){
                    ifnull
                  } else{
                    eval(items[[i]][[type]],input_list_arm_i)
                  } #lazy eval will give error on null, so just put 0 in that case
        )
      }
    }

  }
  return(out)

}


# Helper function to create mean and 95% interval -------------------------

#' Calculate mean and 95% CI from samples
#'
#' @param output_sim The output_psa data frame from the list object returned by `run_sim()`
#' @param element Variable for which mean and 95% CIs are computed (single string)
#' @param round_digit Number of digits to round outputs
#'
#' @return Mean and 95% CI from the PSA samples
#' @importFrom purrr map_dbl
#' @importFrom stats quantile
#'
#' @examples
#' interval_out(output_sim=results$output_sim[[1]],element="costs.",round_digit=3)
#'
#' @keywords internal
#' @noRd

interval_out <- function(output_sim, element,round_digit=2) {
  out <- paste0(apply(do.call(rbind, map(output_sim,element)),2,function(x) format(round(mean(x,na.rm=TRUE),round_digit), big.mark=",", scientific=FALSE)),
                " (",
                apply(do.call(rbind, map(output_sim,element)),2,function(x) format(round(quantile(x,0.025,na.rm=TRUE),round_digit), big.mark=",", scientific=FALSE)) ,
                "; ",
                apply(do.call(rbind, map(output_sim,element)),2,function(x) format(round(quantile(x,0.975,na.rm=TRUE),round_digit), big.mark=",", scientific=FALSE)) ,
                ")"
  )
  return(out)
}

# Helper function to transform the parameters exported in debug -------------------------

#' Helper function to compare environment to cache to check changes
#'
#' @param env Environment to be compared to cache
#' @param clear If TRUE, resets cache
#' @param add If the changes should be evaluated (if FALSE, then it just updates the cache)
#'
#' @return Named list with changes
#'
#'
#' @keywords internal
#' @noRd
log_env <- function(env = NULL, clear = FALSE, add = TRUE) {
    if (clear) {
      # Clear the cache
      .cache <<- list()
    }

    # If an environment is provided, take a snapshot
    if (!is.null(env)) {
      # Capture current environment state
      current_state <- as.list(env, all.names = TRUE)
      
      if(add==TRUE){
        # Get names of current and cached objects
        current_names <- names(current_state)
        cached_names <- names(.cache)
        
        # Identify new objects
        
        
        new_objects <- setdiff(current_names, cached_names)
        
        # Identify modified objects
        common_objects <- intersect(current_names, cached_names)
        
        # Apply `fast_identical` using `mapply` to batch process objects
        modified <- mapply(identical, .cache[common_objects], current_state[common_objects], SIMPLIFY = TRUE)
        modified_objects <- common_objects[!modified]
        
        exclude_objects <- c("evt","start","prevtime","curtime","arm","i")
        
        all_objects <- c(new_objects,modified_objects)
        all_objects <- setdiff(c(new_objects, modified_objects), exclude_objects)
        
        # Store before and after values for modified objects
        loc <- sprintf(
          "Analysis: %s%s%s%s%s%s",
          env$sens,
          if (!is.null(env$simulation)) sprintf("; Sim: %s", env$simulation) else "",
          if (!is.null(env$i)) sprintf("; Patient: %s", env$i) else "",
          if (!is.null(env$arm)) sprintf("; Arm: %s", env$arm) else "",
          if (!is.null(env$evt)) sprintf("; Event: %s", env$evt) else "",
          if (!is.null(env$curtime)) sprintf("; Time: %.3f", env$curtime) else ""
        )
        
        
        if (length(all_objects) == 0) {
          .cache <<- current_state
          log_list <- list(NULL)
          names(log_list) <- loc
          
          return(log_list)
        }
        
        changes <- lapply(all_objects, function(obj) {
          list(prev_value = .cache[[obj]], cur_value = current_state[[obj]])
        })
        names(changes) <- all_objects
        
        log_list <- list(changes)
        names(log_list) <- loc
        
        .cache <<- current_state
        
        return(log_list)
      }
      
      # Update cache to reflect current state
      
    }
    
  }



#Expand events for time frequency approach

#' Helper function to export debug log to a txt file
#' 
#' @param row row to apply function to
#' @param time_points time points to use
#' @param reset_columns Columns that need reset (instantaneous)
#' 
#' @return Expanded datatable
#' 
#' @keywords internal
#' @noRd

expand_event <- function(row, time_points, reset_columns) {
  
  evttime <- row$evttime
  nexttime <- row$nexttime
  
  # Get time_points that fall between evttime and nexttime
  relevant_timepoints <- time_points[time_points > evttime & time_points < nexttime]
  
  # Combine the start (evttime), end (nexttime), and time_points in between
  start_times <- c(evttime, relevant_timepoints)
  end_times <- c(relevant_timepoints, nexttime)

  # Create expanded rows
  expanded <- rbindlist(lapply(seq_along(start_times), function(i) {
    # Copy the row and modify the evttime/nexttime
    new_row <- copy(row)
    new_row$evttime <- start_times[i]
    new_row$nexttime <- end_times[i]
    new_row$time_points <- time_points[i]
    
    # If not the first occurrence, reset specified columns to 0
    if (i > 1) {
      for (col in reset_columns) {
        new_row[[col]] <- 0
        new_row[[paste0(col,"_","undisc")]] <- 0
      }
    }
    
    return(new_row)
  }))
  
  return(expanded)
}


# Helper function to export debug log to a txt file

#' Helper function to export debug log to a txt file
#' 
#' @param log_list name of the debug list object to be exported
#' @param log_name name of the file to be exported to (e.g., "log_list.txt")
#' @param main_byline description
#' 
#' @return None, writes txt called log_name
#' 
#' @keywords internal
#' @noRd

export_log <- function(log_list, log_name, main_byline = FALSE) {
  
  file_conn <- file(log_name)
  
  # Precompute the total number of lines for efficiency
  log_size <- sum(sapply(log_list, function(sublist) {
    length(sublist) * 3 + 2  # Each sublist key has 3 lines (key, prev_value, cur_value) + 2 extra lines for header and blank
  }))
  
  output_lines <- character(log_size)
  
  line_index <- 1
  
  # Loop through the list and construct the data to be written
  for (main_key in names(log_list)) {
    # Handle the main key header
    if (main_byline) {
      header_parts <- unlist(strsplit(main_key, "; "))
      output_lines[line_index] <- paste0(header_parts, collapse = "\n")
    } else {
      output_lines[line_index] <- main_key
    }
    line_index <- line_index + 1
    
    # Handle the second-level key-value pairs (each containing prev_value and cur_value)
    sublist <- log_list[[main_key]]
    for (sub_key in names(sublist)) {
      output_lines[line_index] <- paste0("   ", sub_key)
      line_index <- line_index + 1
      
      # Add prev_value and cur_value indented under the sub_key
      output_lines[line_index] <- paste0("     prev_value = ", sublist[[sub_key]]$prev_value)
      line_index <- line_index + 1
      output_lines[line_index] <- paste0("     cur_value  = ", sublist[[sub_key]]$cur_value)
      line_index <- line_index + 1
    }
    
    # Add a blank line after each block
    output_lines[line_index] <- ""
    line_index <- line_index + 1
  }
  
  writeLines(output_lines, file_conn)
  
  close(file_conn)
}

# Helper function to expand events over time frequency in a fast way for backward accumulation

#'  Helper function to expand events over time frequency in a fast way for backward accumulation
#' 
#' @param data name of the debug list object to be exported
#' @param time_points Time points used for the frequency expansion
#' @param reset_columns Column names to be reset after first occurence, normally instantaneous variables
#' 
#' @return expanded data.table dataset
#' 
#' @keywords internal
#' @noRd
expand_evts_bwd <- function(data, time_points, reset_columns = NULL) {
  # Ensure data is a data.table
  setDT(data)
  
  data[,evt_id := .I]
  
  # Get the relevant time_points for each evttime and prevtime, vectorized
  relevant_timepoints <- lapply(1:nrow(data), function(i) {
    time_points[time_points < data$evttime[i] & time_points > data$prevtime[i]]
  })
  
  # Create start_times and end_times based on prevtime, relevant time_points, and evttime
  start_times <- mapply(c, data$prevtime, relevant_timepoints)
  end_times <- mapply(c, relevant_timepoints,data$evttime)
  
  num_expanded_rows <- lengths(start_times)
  
  # Replicate rows according to the number of expanded time intervals
  expanded_data <- data[rep(seq_len(nrow(data)), num_expanded_rows), ]
  
  # Assign the flattened start_times and end_times back to evttime and prevtime
  expanded_data[, evttime := unlist(end_times)]
  expanded_data[, prevtime := unlist(start_times)]
  
  max_time <- max(time_points)
  # Add the time_points column (rounded up to the nearest time point)
  expanded_data[, time_points := ceiling(evttime)]
  expanded_data[time_points > max_time, time_points := max_time]
  
  # Reset specified columns for all but the first expanded row for each original row
  if (!is.null(reset_columns)) {
    reset_columns_undisc <- paste0(reset_columns, "_undisc")
    columns_to_reset <- c(reset_columns, reset_columns_undisc)
    
    # Create a vector indicating which rows are the last in their expanded series
    is_last_expansion <- rep(FALSE,nrow(expanded_data))
    is_last_expansion[cumsum(num_expanded_rows)] <- TRUE
    
    is_last_expansion <- sequence(num_expanded_rows) == num_expanded_rows
    
    # Reset the specified columns for all non-last expanded rows
    expanded_data[!is_last_expansion, (columns_to_reset) := 0, with = FALSE]
  }
  
  # Make sure prevtime doesn't exceed evttime
  expanded_data[, prevtime := pmin(prevtime, evttime)]
  
  setorder(expanded_data, pat_id, arm, evttime, evt_id)
  
  return(expanded_data)
}

# Helper function to expand events over time frequency in a fast way for forward accumulation

#'  Helper function to expand events over time frequency in a fast way for forward accumulation
#' 
#' @param data name of the debug list object to be exported
#' @param time_points Time points used for the frequency expansion
#' @param reset_columns Column names to be reset after first occurence, normally instantaneous variables
#' 
#' @return expanded data.table dataset
#' 
#' @keywords internal
#' @noRd
expand_evts_fwd <- function(data, time_points, reset_columns = NULL) {
  # Ensure data is a data.table
  setDT(data)
  
  data[,evt_id := .I]
  
  # Get the relevant time_points for each evttime and nexttime, vectorized
  relevant_timepoints <- lapply(1:nrow(data), function(i) {
    time_points[time_points > data$evttime[i] & time_points < data$nexttime[i]]
  })
  
  # Create start_times and end_times based on evttime, relevant time_points, and nexttime
  start_times <- mapply(c, data$evttime, relevant_timepoints)
  end_times <- mapply(c, relevant_timepoints, data$nexttime)
  
  num_expanded_rows <- lengths(start_times)
  
  # Replicate rows according to the number of expanded time intervals
  expanded_data <- data[rep(seq_len(nrow(data)), num_expanded_rows), ]
  
  expanded_data[, evttime     := unlist(start_times)]
  expanded_data[, nexttime    := unlist(end_times)]
  
  max_time <- max(time_points)
  # Add the time_points column (rounded up to the nearest time point)
  expanded_data[, time_points := ceiling(evttime)]
  expanded_data[time_points > max_time, time_points := max_time]

  # Reset specified columns for all but the first expanded row for each original row
  if (!is.null(reset_columns)) {
    reset_columns_undisc <- paste0(reset_columns, "_undisc")
    columns_to_reset <- c(reset_columns, reset_columns_undisc)
    
    # Create a vector indicating which rows are the first in their expanded series
    is_first_expansion <- sequence(num_expanded_rows) == 1
    
    expanded_data[!is_first_expansion, (columns_to_reset) := 0, with = FALSE]
  }
  
  expanded_data[, prevtime := shift(evttime, fill = 0), by = .(pat_id, arm)]
  
  setorder(expanded_data, pat_id, arm, evttime, evt_id)
  
  return(expanded_data)
}


# Compute and Format outputs for specific timepoints -------------------------

#' Compute the discounting and format the outputs from the simulation at specific timepoints
#'
#' @param freq The time frequency used
#' @param patdata_dt The list with the data from the patient-treatment iterations for a single simulation
#' @param input_list The list that contains the main inputs used for the simulation
#' @param prepared_outputs_v Character vector indicating the relevant variables to be exported by type
#' @param data_export_tobesummarized Character vector indicating the relevant variables to be exported by type
#' @param data_export_summarized_nonumeric Character vector indicating the relevant variables to be exported by type
#'
#' @return List with the outputs formatted 
#'
#' @import data.table
#' @importFrom utils tail
#' @importFrom zoo na.locf

#' 
#' @details
#' It computes the discounted and undiscounted lys/costs/qalys at specific timepoints in an aggregated format.
#' 
#' @examples
#' compute_outputs_timseq(patdata_dt=patdata_dt,input_list=input_list, freq = 1)
#'
#' @keywords internal
#' @noRd

compute_outputs_timseq <- function(freq,
                                   patdata_dt,
                                   input_list,
                                   prepared_outputs_v,
                                   data_export_tobesummarized,
                                   data_export_summarized_nonumeric) {
  
  arm_list <- input_list$arm_list
  
  adjusted_to <- floor(max(patdata_dt$evttime)) + (freq - ceiling(max(patdata_dt$evttime)) %% freq) %% freq
  
  if (adjusted_to> max(patdata_dt$evttime)) {
    adjusted_to <- floor(max(patdata_dt$evttime))
  } # if adjusted_to is greater than max_time
  
  time_points <- seq(from=0,to=adjusted_to, by=freq)
  
  if (adjusted_to< max(patdata_dt$evttime)) {
    time_points <- c(time_points, max(patdata_dt$evttime))
  } # if adjusted_to is less than max event time
  
  
  
  time_points_dt <- data.table::data.table(pat_id = rep(rep(unique(patdata_dt$pat_id),each=length(time_points)),length(unique(patdata_dt$arm))),
                                           arm = rep(rep(unique(patdata_dt$arm),each=length(unique(patdata_dt$pat_id))),each=length(unique(time_points))),
                                           time_points = rep(rep(time_points,length(unique(patdata_dt$pat_id))),length(unique(patdata_dt$arm))))
  
  
  if(input_list$accum_backwards){
    final_filtered <- expand_evts_bwd(patdata_dt, time_points, input_list$uc_lists$instant_inputs)
  }else{
    final_filtered <- expand_evts_fwd(patdata_dt, time_points, input_list$uc_lists$instant_inputs)
  }
  

  # Discounting of Outcomes-------------------------------------------------------------
  
  #Discount and undiscount ongoing
  
  for (cat in input_list$uc_lists$ongoing_inputs) {
    
    
    final_filtered[,paste0(cat,"_","undisc") := disc_ongoing_v(lcldr=0,
                                                               lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                                               lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                                               lclval=get(cat))]
    
    final_filtered[,paste0(cat) := disc_ongoing_v(lcldr=input_list$drc,
                                                  lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                                  lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                                  lclval=get(cat))]
    
    if(cat %in% input_list$uc_lists$cost_categories_ongoing){
      final_filtered[, "costs" := costs + get(cat)]
      final_filtered[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% input_list$uc_lists$util_categories_ongoing){
      final_filtered[, "qalys" := qalys+ get(cat)]
      final_filtered[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    
  }
  
  
  #Discount and undiscount instant
  for (cat in input_list$uc_lists$instant_inputs) {
    final_filtered[,paste0(cat,"_","undisc") := disc_instant_v(lcldr=0,
                                                               lclcurtime=evttime,
                                                               lclval=get(cat))]
    
    final_filtered[,paste0(cat) := disc_instant_v(lcldr=input_list$drc,
                                                  lclcurtime=evttime,
                                                  lclval=get(cat))]
    
    if(cat %in% input_list$uc_lists$cost_categories_instant){
      final_filtered[, "costs" := costs + get(cat)]
      final_filtered[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% input_list$uc_lists$util_categories_instant){
      final_filtered[, "qalys" := qalys + get(cat)]
      final_filtered[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    
  }
  
  #Discount and undiscount cycle
  for (cat in input_list$uc_lists$cycle_inputs) {
    final_filtered[,paste0(cat,"_","undisc") := disc_cycle_v(lcldr=0,
                                                             lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                                             cyclelength = get(paste0(cat,"_","cycle_l")),
                                                             lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                                             lclval= get(cat),
                                                             starttime = get(paste0(cat,"_","cycle_starttime")))] 
    
    final_filtered[,paste0(cat) := disc_cycle_v(lcldr=input_list$drc,
                                                lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                                cyclelength = get(paste0(cat,"_","cycle_l")),
                                                lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                                lclval= get(cat),
                                                starttime = get(paste0(cat,"_","cycle_starttime")))]
    
    if(cat %in% input_list$uc_lists$cost_categories_cycle){
      final_filtered[, "costs" := costs + get(cat)]
      final_filtered[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% input_list$uc_lists$util_categories_cycle){
      final_filtered[, "qalys" := qalys + get(cat)]
      final_filtered[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    
  }
  
  
  #Discount and undiscount LYs
  final_filtered[,"lys" := disc_ongoing_v(lcldr=input_list$drq,
                                          lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                          lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                          lclval=1)]
  
  final_filtered[,"lys_undisc" := disc_ongoing_v(lcldr=0,
                                                 lclprvtime=if(input_list$accum_backwards){prevtime}else{evttime},
                                                 lclcurtime=if(input_list$accum_backwards){evttime}else{nexttime},
                                                 lclval=1)]
  
  #Calculate total outcomes
  final_filtered[,"total_costs" := sum(costs),by=.(pat_id,arm)]
  final_filtered[,"total_qalys" := sum(qalys),by=.(pat_id,arm)]
  final_filtered[,"total_lys" := sum(lys),by=.(pat_id,arm)]
  final_filtered[,"total_costs_undisc" := sum(costs_undisc),by=.(pat_id,arm)]
  final_filtered[,"total_qalys_undisc" := sum(qalys_undisc),by=.(pat_id,arm)]
  final_filtered[,"total_lys_undisc" := sum(lys_undisc),by=.(pat_id,arm)]
  
  #Partially order the data
  data.table::setcolorder(final_filtered,   c("evtname","time_points","evttime", "prevtime", "pat_id", "arm",
                                              "total_lys","total_qalys","total_costs",
                                              "total_costs_undisc", "total_qalys_undisc", "total_lys_undisc",
                                              "lys","qalys","costs",
                                              "lys_undisc", "qalys_undisc",  "costs_undisc"))
  
  
  # Organize and create output -----------------------------------------------------------
  
  timed_output <- list()
  
  #Create total outputs and user-defined costs/utilities from IPD
  vector_total_outputs <- c("total_lys","total_qalys","total_costs","total_lys_undisc","total_qalys_undisc","total_costs_undisc")
  vector_total_outputs_search <- c("lys","qalys","costs","lys_undisc","qalys_undisc","costs_undisc")
  
  #Add to final outputs the total outcomes as well as the cost/utility categories totals
  vector_other_outputs <- c(input_list$categories_for_export,prepared_outputs_v)
  
  for (arm_i in arm_list) {
    for (output_i in 1:length(vector_total_outputs)) {
      temp_vec <- final_filtered[arm==arm_i,.(out=sum(get(vector_total_outputs_search[output_i]),na.rm=TRUE)/input_list$npats),by=.(time_points)][,cumsum(out)]
      if(length(time_points)> length(temp_vec)){
        last_value <- tail(temp_vec,1)
        temp_vec <- c(temp_vec,rep(last_value,length(time_points) - length(temp_vec)))
      }
      timed_output[[vector_total_outputs[output_i]]][arm_i] <- list(temp_vec)
    }
    
    for (output_i in vector_other_outputs) {
      temp_vec <- final_filtered[arm==arm_i,.(out=sum(get(output_i),na.rm=TRUE)/input_list$npats),by=.(time_points)][,cumsum(out)]
      if(length(time_points)> length(temp_vec)){
        last_value <- tail(temp_vec,1)
        temp_vec <- c(temp_vec,rep(last_value,length(time_points) - length(temp_vec)))
      }
      timed_output[[output_i]][arm_i]  <- list(temp_vec)
    }
    
    for (output_i in data_export_tobesummarized) {
      #Gets last value from patient and time, removes the accumulation, computes the average over population, then does the cumulative outcome
      temp_vec <- final_filtered[arm==arm_i,.(out=tail(get(output_i)*is.finite(get(output_i)),n=1,na.rm=TRUE)),by=.(time_points,pat_id)][
        ,out:=out-shift(out,fill=0),by=.(pat_id)][
          ,.(out=sum(out,na.rm=TRUE)/(input_list$npats-sum(is.na(out)))),by=.(time_points)][
            ,cumsum(out)]
      
      if(length(time_points)> length(temp_vec)){
        last_value <- tail(temp_vec,1)
        temp_vec <- c(temp_vec,rep(last_value,length(time_points) - length(temp_vec)))
      }
      
      timed_output[[output_i]][arm_i] <- list(temp_vec)
    }
    
    for (output_i in data_export_summarized_nonumeric) {
      #Gets last value 
      temp_vec <- final_filtered[arm==arm_i,.(out=tail(get(output_i),n=1,na.rm=TRUE)),by=.(time_points,pat_id)][,.(out=tail(out,n=1,na.rm=TRUE)),by=.(time_points)]$out
      
      if(length(time_points)> length(temp_vec)){
        last_value <- tail(temp_vec,1)
        temp_vec <- c(temp_vec,rep(last_value,length(time_points) - length(temp_vec)))
      }
      timed_output[[output_i]][arm_i] <- list(temp_vec)
    }
    
  }
  
  final_out_sorted <- names(timed_output)[!names(timed_output) %in% vector_total_outputs]
  order_final_output <- c(vector_total_outputs,final_out_sorted[order(final_out_sorted)])
  timed_output <- c(list(arm_list=arm_list),list(timepoints=time_points),timed_output[order_final_output])
  
  
  
  return(timed_output)
}



# Compute and Format outputs -------------------------

#' Compute the discounting and format the outputs from the simulation
#'
#' @param patdata The list with the data from the patient-treatment iterations for a single simulation
#' @param input_list The list that contains the main inputs used for the simulation
#'
#' @return List with the outputs formatted 
#'
#' @import data.table
#' @importFrom utils tail
#' @importFrom zoo na.locf

#' 
#' @details
#' It computes the discounted and undiscounted lys/costs/qalys. 
#' 
#' It also computes the mean aggregate outcomes per arm for the simulation for numeric values of length 1, discarding NAs and Inf. 
#' Extra data defined by user of length >1 (e.g., matrix) will not be displayed as part of the final IPD data.table and 
#' will instead be reported in the `extradata_raw` list.
#' 
#' Extra data defined by user of length==1 will be integrated in the final IPD data.table. 
#' 
#' For `input_out` items that are of length ==1, using ipd = 2 in `run_sim` will take the last observation per patient.
#' If using ipd = 3, it will return the average across patients of the last observation per patient (if numeric. If not numeric, it will be discarded).
#'
#' @examples
#' compute_outputs(patdata=patdata,input_list=input_list)
#'
#' @keywords internal
#' @noRd

compute_outputs <- function(patdata,env_input_list) {
  arm_list <- env_input_list$arm_list
  simulation <- env_input_list$simulation
  sens <- env_input_list$sens
  n_sim <- env_input_list$n_sim
  npats <- env_input_list$npats
  psa_bool <- env_input_list$psa_bool
  
  patdata_dt <- NULL
  list_patdata <- NULL

  #Split the data as to be exported as a data.table, and the extra data the user described
  data_export_aslist <- env_input_list$input_out[!env_input_list$input_out %in% env_input_list$categories_for_export]
  data_export_summarized_nonumeric <- data_export_aslist
    
  for (arm_i in arm_list) {
    list_patdata <- c(list_patdata,unlist(map(map(patdata,arm_i),"evtlist"), recursive = FALSE))
  }  
  
  rm(patdata)

  #We exclude the extra data the user described that has a length > 1 (e.g., a matrix) from the data.table
  #as there could be matrices or other objects not suitable for data.table
  
  items_length_greater_than_one <- unlist(list_patdata,recursive=FALSE)
  items_length_greater_than_one <- items_length_greater_than_one[names(items_length_greater_than_one) %in% data_export_aslist]
  
  items_length_one_numeric <- unlist(lapply(items_length_greater_than_one, function(x) length(x)==1 & is.numeric(x)))
  items_length_one_numeric <- items_length_one_numeric[items_length_one_numeric==TRUE]
  
  items_length_greater_than_one <- unlist(lapply(items_length_greater_than_one, function(x) length(x)>1))
  items_length_greater_than_one <- items_length_greater_than_one[items_length_greater_than_one==TRUE]
  
  data_export_aslist <- unique(names(items_length_greater_than_one))
  data_export_tobesummarized <- unique(names(items_length_one_numeric))
  data_export_summarized_nonumeric <- data_export_summarized_nonumeric[!data_export_summarized_nonumeric %in% c(data_export_aslist,data_export_tobesummarized)]
  
  if (length(data_export_aslist)>0) {
    list_patdata2 <- lapply(list_patdata,function(x) x[!names(x) %in% data_export_aslist])
  } else{
    list_patdata2 <- list_patdata
  }
  
  patdata_dt <- rbindlist(list(patdata_dt,rbindlist(list_patdata2,fill=TRUE)))
  
  rm(list_patdata2)
  
  #Extract only extra data that the user wants to export
  export_list_ipd <- lapply(list_patdata,function(x) x[data_export_aslist])
  
  
  prepared_outputs_v <- c(
    if(!is.null(env_input_list$uc_lists$ongoing_inputs)){as.vector(outer(env_input_list$uc_lists$ongoing_inputs, c("undisc"), paste, sep="_"))},
    if(!is.null(env_input_list$uc_lists$instant_inputs)){as.vector(outer(env_input_list$uc_lists$instant_inputs, c("undisc"), paste, sep="_"))},
    if(!is.null(env_input_list$uc_lists$cycle_inputs)){as.vector(outer(env_input_list$uc_lists$cycle_inputs, c("undisc"), paste, sep="_"))}
  )
  
  #Use the data.table to initialize values
  patdata_dt[,prevtime:=data.table::shift(evttime,fill=0)]
  patdata_dt[,prevtime:=ifelse(prevtime>evttime,0,prevtime)]
  cols_init <- c("lys",
                 "qalys",
                 "costs",
                 "lys_undisc",
                 "qalys_undisc",
                 "costs_undisc",
                 prepared_outputs_v
                 )
  patdata_dt[,(cols_init):=0]
  

  if(env_input_list$accum_backwards){ #if accumulating backwards, need to rewrite values
  
    for (cat in env_input_list$uc_lists$ongoing_inputs) {
      cat_lastupdate <- paste0(cat, "_lastupdate")
  
      # Calculate last observation index and set the last update flag in one step
      patdata_dt[patdata_dt[, .I[.N], by = .(pat_id, arm)]$V1, (cat_lastupdate) := 1L]
  
      # Keep updated values and fill NA values backwards within each group
      patdata_dt[, (cat) := {
        value_new <- ifelse(get(cat_lastupdate) == 1, get(cat), NA_real_)
        zoo::na.locf(value_new, fromLast = TRUE, na.rm = FALSE)
      }, by = .(pat_id, arm)]
  
      # Remove the last update flag column
      patdata_dt[, (cat_lastupdate) := NULL]
    }
  
  } else{
    patdata_dt[, nexttime:=data.table::shift(evttime,fill=0,n=-1L)]
    patdata_dt[, nexttime := ifelse(nexttime<evttime,evttime,nexttime)]
    }
  
  if(!is.null(env_input_list$timed_freq)){
  timed_outputs <- compute_outputs_timseq(env_input_list$timed_freq,
                                          patdata_dt,
                                          env_input_list,
                                          prepared_outputs_v,
                                          data_export_tobesummarized,
                                          data_export_summarized_nonumeric)
  }
  
  # Discounting of Outcomes-------------------------------------------------------------
  
  #Discount and undiscount ongoing
  
  for (cat in env_input_list$uc_lists$ongoing_inputs) {

    patdata_dt[,paste0(cat,"_","undisc") := disc_ongoing_v(lcldr=0,
                                                                        lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                                                        lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                                                        lclval=get(cat))]
    
    patdata_dt[,paste0(cat) := disc_ongoing_v(lcldr=env_input_list$drc,
                                                                 lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                                                 lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                                                 lclval=get(cat))]

    if(cat %in% env_input_list$uc_lists$cost_categories_ongoing){
      patdata_dt[, "costs" := costs + get(cat)]
      patdata_dt[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% env_input_list$uc_lists$util_categories_ongoing){
      patdata_dt[, "qalys" := qalys+ get(cat)]
      patdata_dt[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    

  }
  
  
  #Discount and undiscount instant
  for (cat in env_input_list$uc_lists$instant_inputs) {
    patdata_dt[,paste0(cat,"_","undisc") := disc_instant_v(lcldr=0,
                                                                        lclcurtime=evttime,
                                                                        lclval=get(cat))]
    
    patdata_dt[,paste0(cat) := disc_instant_v(lcldr=env_input_list$drc,
                                                                 lclcurtime=evttime,
                                                                 lclval=get(cat))]
    
    if(cat %in% env_input_list$uc_lists$cost_categories_instant){
      patdata_dt[, "costs" := costs + get(cat)]
      patdata_dt[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% env_input_list$uc_lists$util_categories_instant){
      patdata_dt[, "qalys" := qalys + get(cat)]
      patdata_dt[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    
  }
  
  #Discount and undiscount cycle
  for (cat in env_input_list$uc_lists$cycle_inputs) {
    patdata_dt[,paste0(cat,"_","undisc") := disc_cycle_v(lcldr=0,
                                                                    lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                                                    cyclelength = get(paste0(cat,"_","cycle_l")),
                                                                    lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                                                    lclval= get(cat),
                                                                    starttime = get(paste0(cat,"_","cycle_starttime")))] 
    
    patdata_dt[,paste0(cat) := disc_cycle_v(lcldr=env_input_list$drc,
                                                             lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                                             cyclelength = get(paste0(cat,"_","cycle_l")),
                                                             lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                                             lclval= get(cat),
                                                             starttime = get(paste0(cat,"_","cycle_starttime")))]
    
    if(cat %in% env_input_list$uc_lists$cost_categories_cycle){
      patdata_dt[, "costs" := costs + get(cat)]
      patdata_dt[, "costs_undisc" := costs_undisc + get(paste0(cat,"_","undisc"))]
    }
    
    if(cat %in% env_input_list$uc_lists$util_categories_cycle){
      patdata_dt[, "qalys" := qalys + get(cat)]
      patdata_dt[, "qalys_undisc" := qalys_undisc + get(paste0(cat,"_","undisc"))]
    }
    
  }
  
  
  #Discount and undiscount LYs
  patdata_dt[,"lys" := disc_ongoing_v(lcldr=env_input_list$drq,
                                      lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                      lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                      lclval=1)]
  
  patdata_dt[,"lys_undisc" := disc_ongoing_v(lcldr=0,
                                             lclprvtime=if(env_input_list$accum_backwards){prevtime}else{evttime},
                                             lclcurtime=if(env_input_list$accum_backwards){evttime}else{nexttime},
                                             lclval=1)]
  
  #Calculate total outcomes
  patdata_dt[,"total_costs" := sum(costs),by=.(pat_id,arm)]
  patdata_dt[,"total_qalys" := sum(qalys),by=.(pat_id,arm)]
  patdata_dt[,"total_lys" := sum(lys),by=.(pat_id,arm)]
  patdata_dt[,"total_costs_undisc" := sum(costs_undisc),by=.(pat_id,arm)]
  patdata_dt[,"total_qalys_undisc" := sum(qalys_undisc),by=.(pat_id,arm)]
  patdata_dt[,"total_lys_undisc" := sum(lys_undisc),by=.(pat_id,arm)]
  
  #Partially order the data
  data.table::setcolorder(patdata_dt,   c("evtname", "evttime", "prevtime", "pat_id", "arm",
                                          "total_lys","total_qalys","total_costs",
                                          "total_costs_undisc", "total_qalys_undisc", "total_lys_undisc",
                                          "lys","qalys","costs",
                                          "lys_undisc", "qalys_undisc",  "costs_undisc"))
  
  # Organize and create output -----------------------------------------------------------
  
  final_output <- list()
  
  #Create total outputs and user-defined costs/utilities from IPD
  vector_total_outputs <- c("total_lys","total_qalys","total_costs","total_lys_undisc","total_qalys_undisc","total_costs_undisc")
  vector_total_outputs_search <- c("lys","qalys","costs","lys_undisc","qalys_undisc","costs_undisc")

  #Add to final outputs the total outcomes as well as the cost/utility categories totals
  vector_other_outputs <- c(env_input_list$categories_for_export,prepared_outputs_v)
  for (arm_i in arm_list) {
    for (output_i in 1:length(vector_total_outputs)) {
      final_output[[vector_total_outputs[output_i]]][arm_i] <- patdata_dt[arm==arm_i,.(out=sum(get(vector_total_outputs_search[output_i]),na.rm=TRUE)),by=.(pat_id)][,mean(out,na.rm=TRUE)]
    }
    for (output_i in vector_other_outputs) {
      final_output[[output_i]][arm_i] <- patdata_dt[arm==arm_i,.(out=sum(get(output_i),na.rm=TRUE)),by=.(pat_id)][,mean(out,na.rm=TRUE)]
    }
    
    for (output_i in data_export_tobesummarized) {
        #Gets last value from patient, then average for numeric
      final_output[[output_i]][arm_i] <- patdata_dt[arm==arm_i,.(out=tail(get(output_i)*is.finite(get(output_i)),n=1,na.rm=TRUE)),by=.(pat_id)][,mean(out,na.rm=TRUE)]
    }
    
    for (output_i in data_export_summarized_nonumeric) {
      #Gets last value 
      final_output[[output_i]][arm_i] <- patdata_dt[arm==arm_i,.(out=tail(get(output_i),n=1,na.rm=TRUE)),by=.(pat_id)][,tail(out,n=1,na.rm=TRUE)]
    }
    
    
    for (output_i in data_export_aslist) {
      #Get last value
      temp <- Filter(function(sublist) sublist[["arm"]] == arm_i, list_patdata)
      final_output[[output_i]][[arm_i]] <- temp[[length(temp)]][[output_i]]
    }
  }
  
  rm(list_patdata)
  
  final_out_sorted <- names(final_output)[!names(final_output) %in% vector_total_outputs]
  order_final_output <- c(vector_total_outputs,final_out_sorted[order(final_out_sorted)])
  final_output <- c(list(arm_list=arm_list),final_output[order_final_output])
  
  #Exports IPD values either fully IPD or aggregated for events
  if (env_input_list$ipd==1) {
    final_output$merged_df <- patdata_dt
    if(length(data_export_aslist)>0){
      final_output$extradata_raw <- export_list_ipd
    }
    
  } else if (env_input_list$ipd==2) {
    #Get names of columns that will be used, 
    other_cols <- c("pat_id", "arm")
    #Columns that will not be summarized (event related columns, time and total_)
    cols_to_rm <- colnames(patdata_dt)[grepl("total_",colnames(patdata_dt)) | colnames(patdata_dt) %in% c("evtname", "evttime", "prevtime")]
    patdata_dt[,number_events:=1]
    #Numeric columns only
    numeric_c <- sapply(patdata_dt,is.numeric)
    #Columns to sum must be in the right list and also be numeric
    cols_to_sum <- colnames(patdata_dt)[!colnames(patdata_dt) %in% c(other_cols,cols_to_rm,data_export_tobesummarized) & numeric_c]
    #Other columns are left as is (takes last value of those)
    cols_to_leave_as_is <- colnames(patdata_dt)[!colnames(patdata_dt) %in% c(other_cols,cols_to_rm) & !colnames(patdata_dt) %in% c(cols_to_sum)]
    
    #Summarize the data as relevant
    patdata_temp <- patdata_dt[, lapply(.SD, sum, na.rm=TRUE), by=other_cols, .SDcols=cols_to_sum] #sum numeric variables in list
    
    if (length(cols_to_leave_as_is!=0)) {
      patdata_temp2 <- patdata_dt[, tail(.SD, 1, na.rm=TRUE), by=other_cols, .SDcols=cols_to_leave_as_is] #get last observation if other
      final_output$merged_df <- merge(patdata_temp,patdata_temp2)
    } else{
      final_output$merged_df <- patdata_temp
    }
    
    colnames(final_output$merged_df)[colnames(final_output$merged_df) %in% c("qalys","costs","lys","qalys_undisc","costs_undisc","lys_undisc")] <- paste0("total_",colnames(final_output$merged_df)[colnames(final_output$merged_df) %in% c("qalys","costs","lys","qalys_undisc","costs_undisc","lys_undisc")])


    if (sens==1 & simulation==1) {
      message("Patient-arm data aggregated across events by selecting the last value for input_out items.")
      }
    
    if(length(data_export_aslist)>0){
      final_output$extradata_raw <- export_list_ipd
    }
  } else{
    if (sens==1 & simulation==1) {
      message("Data aggregated across events and patients by selecting the last value for input_out numeric items and then averaging across patients. Only last value of non-numeric and length > 1 items in simulation is displayed. ")
    }
  }
  
  if(!is.null(env_input_list$timed_freq)){
    final_output$timed_outputs <- timed_outputs
  }
  
  return(final_output)
}


