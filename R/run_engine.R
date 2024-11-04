#' Run the simulation engine without using a parallel engine, facilitating debugging
#'
#' @param arm_list A vector of the names of the interventions evaluated in the simulation
#' @param common_pt_inputs A list of inputs that change across patients but are not affected by the intervention
#' @param unique_pt_inputs A list of inputs that change across each intervention
#' @param env_input_list A list of all other inputs: drc, drq, psa_bool, init_event_list, evt_react_list,
#' uc_lists = list(util_ongoing_list,util_instant_list,util_cycle_list,cost_ongoing_list,cost_instant_list,cost_cycle_list),
#' input_out,ipd,arm_list,simulation,npats,n_sim
#'
#' @return A data frame with the simulation results
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom data.table rbindlist
#' @importFrom progressr progressor
#'
#' @keywords internal


run_engine <- function(arm_list,
                       common_pt_inputs=NULL,
                       unique_pt_inputs=NULL,
                       env_input_list = NULL,
                       log_out = NULL){
  # Initial set-up --------------------------
  arm_list <- arm_list
  simulation <- env_input_list$simulation
  sens <- env_input_list$sens
  n_sim <- env_input_list$n_sim
  npats <- env_input_list$npats
  psa_bool <- env_input_list$psa_bool
  index_log <- env_input_list$index_log 

  #1 Loop per patient ----------------------------------------------------------
  patdata <- vector("list", length=npats) # empty list with npats elements

  temp_log_pt <- list()

    
  pb <- progressr::progressor(50) 
  
  for (i in 1:npats) {
    set.seed(i*simulation)
    if(i %% ceiling(npats / 50) == 0 | i == npats){
      pb(sprintf("Simulation %g", simulation))
      }
    
    #Create empty pat data for each arm
    this_patient <- list()
    
    env_input_list_pt <- new.env(parent = parent.env(env_input_list))
    list2env(as.list(env_input_list, all.names = TRUE), envir = env_input_list_pt)
    
    env_input_list_pt$i <- i
    
    #Extract the inputs that are common for each patient across interventions
    if(env_input_list$debug){
      log_env(env_input_list_pt, add = FALSE)
    }
    eval(common_pt_inputs[[1]]$inputs,envir = env_input_list_pt)
    if(env_input_list$debug){
      log_out[[index_log]] <- log_env(env_input_list_pt)
      index_log <- index_log + 1
    }
    
    #2 Loop per treatment ------------------------------------------------------
    temp_log <- list()
    
    for (arm in arm_list) {
      set.seed(i*simulation)
      # Initialize values to prevent errors
      output_list <- list(curtime = 0)
      
      env_input_list_arm <- new.env(parent = parent.env(env_input_list_pt))
      list2env(as.list(env_input_list_pt, all.names = TRUE), envir = env_input_list_arm)
      
      env_input_list_arm$arm <- arm
      
      
      if(env_input_list$debug){
        log_env(env_input_list_arm, add = FALSE)
      }
      #Extract the inputs that are unique for each patient-intervention
      eval(unique_pt_inputs[[1]]$inputs,envir = env_input_list_arm)
      
      if(env_input_list$debug){
        log_out[[index_log]] <- log_env(env_input_list_arm)
        index_log <- index_log + 1
      }
      
      # Generate event list
      #if noeventlist, then just make start at 0
      
      if (is.null(env_input_list_arm$init_event_list)) {
        env_input_list_arm$evt_list <- list(cur_evtlist = setNames(0,"start"), time_data = NULL)
      } else{
        environment(initiate_evt) <- env_input_list_arm
        evt_list <- initiate_evt(arm)
        
        env_input_list_arm$cur_evtlist <- evt_list[["cur_evtlist"]]
        list2env(evt_list$time_data, envir = env_input_list_arm)
      }
      
      # 3 Loop per event --------------------------------------------------------

      this_patient[[arm]]$evtlist <- NULL
      
      list2env(output_list, envir = env_input_list_arm)

      n_evt <- 0
      while(env_input_list_arm$curtime < Inf){
        
        # Get next event, process, repeat
        output_nxtevt <- get_next_evt(env_input_list_arm$cur_evtlist)
        Evt <- output_nxtevt$out
        env_input_list_arm$cur_evtlist <- output_nxtevt[["evt_list"]]

        n_evt <- n_evt +1


        if (is.null(Evt)==F){  
          #Evalaute event
          environment(react_evt) <- env_input_list_arm
          react_evt(Evt, arm)
          if(env_input_list_arm$debug){
            log_out[[index_log]] <- log_env(env_input_list_arm)
            index_log <- index_log + 1
          }
          
          #Get extra objects to be exported
          
          if(env_input_list$accum_backwards){
          extra_data <- mget(c(env_input_list_arm$input_out,
                               paste0(env_input_list_arm$uc_lists$ongoing_inputs,"_lastupdate",recycle0 = TRUE)
          ), envir = env_input_list_arm)
          }else{
            extra_data <- mget(env_input_list_arm$input_out, envir = env_input_list_arm)
          }
          extra_data <- extra_data[!sapply(extra_data,is.null)]
 
              this_patient[[arm]]$evtlist[[n_evt]] <- c(evtname = Evt$evt ,
                                                        evttime = Evt$evttime,
                                                        pat_id = i,
                                                        arm = arm,
                                                        extra_data
              )
            

        } else {env_input_list_arm$curtime <- Inf} #if no events, stop
        
        
        
      }
      
      
    }

    patdata[[i]] <- this_patient
    
  }
  
  
# Compute outputs ---------------------------------------------------------


  #Compute the outputs and format the data
  final_output <- compute_outputs(patdata, env_input_list)
  
  if(env_input_list$debug){
    final_output$log_list <- log_out
  }
    return(final_output)


}
