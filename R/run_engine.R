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
                            env_input_list = NULL){
  # Initial set-up --------------------------
  arm_list <- arm_list
  simulation <- env_input_list$simulation
  sens <- env_input_list$sens
  n_sim <- env_input_list$n_sim
  npats <- env_input_list$npats
  psa_bool <- env_input_list$psa_bool


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
    
    # env_input_list_pt <- rlang::env_clone(env_input_list, parent.env(env_input_list))
    env_input_list_pt$i <- i
    
    #Extract the inputs that are common for each patient across interventions
    eval(common_pt_inputs[[1]]$inputs,envir = env_input_list_pt)
    
    # if(!is.null(common_pt_inputs)){
    #   for (inp in 1:length(common_pt_inputs)) {
    #     list.common_pt_inputs <- lapply(common_pt_inputs[inp],function(x) eval(x, env_input_list_pt))
    #     #If using pick_eval_v or other expressions, the lists are not deployed, so this is necessary to do so
    #     if(any(is.null(names(list.common_pt_inputs)), names(list.common_pt_inputs)=="") & length(list.common_pt_inputs)==1) {
    #       env_input_list_pt <- c(env_input_list_pt, list.common_pt_inputs[[1]])
    #     } else{
    #     if (!is.null(names(list.common_pt_inputs[[1]]))) {
    #       warning("Item ", names(list.common_pt_inputs), " is named. It is advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model.\n")
    #     }
    #     env_input_list_pt <- c(env_input_list_pt,list.common_pt_inputs)
    #     }
    #   }
    #   
    #   if(env_input_list$debug){ 
    #     names_pt_input <- names(common_pt_inputs)
    #     prev_value <- setNames(vector("list", length(common_pt_inputs)), names_pt_input)
    #     prev_value[names_pt_input] <- env_input_list[names_pt_input]
    #     dump_info <- list(
    #       list(
    #         prev_value = prev_value,
    #         cur_value  = env_input_list_pt[names_pt_input]
    #       )
    #     )
    #     
    #     names(dump_info) <- paste0("Analysis: ", env_input_list_pt$sens,
    #                                "; Sim: ", env_input_list_pt$sim,
    #                                "; Patient: ", env_input_list_pt$i,
    #                                "; Initial Patient Conditions"
    #     )
    #     
    #     temp_log_pt <- c(temp_log_pt,dump_info)
    #   }
    #   
    # }
    # 
    # #Make sure there are no duplicated inputs in the model, if so, take the last one
    # duplic <- duplicated(names(env_input_list_pt),fromLast = T)
    # if (sum(duplic)>0 & i==1 & simulation==1 & sens==1) { warning("Duplicated items detected in the Patient, using the last one added.\n")  }
    # env_input_list_pt <- env_input_list_pt[!duplic]

    #2 Loop per treatment ------------------------------------------------------
    temp_log <- list()
    
    for (arm in arm_list) {
      set.seed(i*simulation)
      # Initialize values to prevent errors
      output_list <- list(curtime = 0)
      
      env_input_list_arm <- new.env(parent = parent.env(env_input_list_pt))
      list2env(as.list(env_input_list_pt, all.names = TRUE), envir = env_input_list_arm)
      
      # env_input_list_arm <- rlang::env_clone(env_input_list_pt, parent.env(env_input_list_pt))
      env_input_list_arm$arm <- arm
      
      #Extract the inputs that are unique for each patient-intervention
      eval(unique_pt_inputs[[1]]$inputs,envir = env_input_list_arm)
      
      # #Extract the inputs that are unique for each patient-intervention
      # env_input_list_arm <- NULL
      # env_input_list_arm <- c(env_input_list_pt,list(arm=arm))
      # 
      # 
      # if(!is.null(unique_pt_inputs)){
      #   for (inp in 1:length(unique_pt_inputs)) {
      #     list.unique_pt_inputs <- lapply(unique_pt_inputs[inp],function(x) eval(x, env_input_list_arm))
      #     #If using pick_eval_v or other expressions, the lists are not deployed, so this is necessary to do so
      #     if(any(is.null(names(list.unique_pt_inputs)), names(list.unique_pt_inputs)=="") & length(list.unique_pt_inputs)==1) {
      #       env_input_list_arm <- c(env_input_list_arm, list.unique_pt_inputs[[1]])
      #     } else{
      #     if ((!is.null(names(list.unique_pt_inputs[[1]]))) & i==1 & simulation==1 & sens==1) {
      #       warning("Item ", names(list.unique_pt_inputs), " is named. It is advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model.\n")
      #     }
      #     env_input_list_arm <- c(env_input_list_arm,list.unique_pt_inputs)
      #     }
      #   }
      #   
      #   if(env_input_list_pt$debug){ 
      #     names_pt_input <- names(unique_pt_inputs)
      #     prev_value <- setNames(vector("list", length(unique_pt_inputs)), names_pt_input)
      #     prev_value[names_pt_input] <- env_input_list_pt[names_pt_input]
      #     dump_info <- list(
      #       list(
      #         prev_value = prev_value,
      #         cur_value  = env_input_list_arm[names_pt_input]
      #       )
      #     )
      # 
      #     names(dump_info) <- paste0("Analysis: ", env_input_list_arm$sens,
      #                                "; Sim: ", env_input_list_arm$sim,
      #                                "; Patient: ", env_input_list_arm$i,
      #                                "; Initial Patient-Arm Conditions"
      #     )
      #     
      #     temp_log <- c(temp_log,dump_info)
      #   }
      # }

      #Make sure there are no duplicated inputs in the model, if so, take the last one
      # duplic <- duplicated(names(env_input_list_arm),fromLast = T)
      # if (sum(duplic)>0 & i==1 & simulation==1 & sens==1) { warning("Duplicated items detected in the Arm, using the last one added.\n")  }
      # env_input_list_arm <- env_input_list_arm[!duplic]

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
      #Main environment of reference is this one
      env_input_list_arm$list_env <- list(list_env = environment())
      
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
      
      temp_log <- c(temp_log,env_input_list_arm$log_list)
    }
    temp_log_pt <- c(temp_log_pt,temp_log)

    patdata[[i]] <- this_patient
    
  }
  
  env_input_list$log_list <- lapply(temp_log_pt,transform_debug)
  
  
# Compute outputs ---------------------------------------------------------


  #Compute the outputs and format the data
  final_output <- compute_outputs(patdata, env_input_list)
  
  if(env_input_list$debug){
    final_output$log_list <- env_input_list$log_list
  }
    return(final_output)


}
