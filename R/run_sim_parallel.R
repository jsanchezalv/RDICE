
# Global variables for CRAN check -----------------------------------------

if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      c('simulation')
    )) 
}

#' Run simulations in parallel mode (at the simulation level)
#'
#' @param arm_list A vector of the names of the interventions evaluated in the simulation
#' @param sensitivity_inputs A list of sensitivity inputs that do not change within a sensitivity in a similar fashion to common_all_inputs, etc
#' @param common_all_inputs A list of inputs common across patients that do not change within a simulation
#' @param common_pt_inputs A list of inputs that change across patients but are not affected by the intervention
#' @param unique_pt_inputs A list of inputs that change across each intervention
#' @param init_event_list A list of initial events and event times. If no initial events are given, a "Start" event at time 0 is created automatically
#' @param evt_react_list A list of event reactions
#' @param util_ongoing_list Vector of QALY named variables that are accrued at an ongoing basis (discounted using drq)
#' @param util_instant_list Vector of QALY named variables that are accrued instantaneously at an event (discounted using drq)
#' @param util_cycle_list Vector of QALY named variables that are accrued in cycles (discounted using drq)
#' @param cost_ongoing_list Vector of cost named variables that are accrued at an ongoing basis (discounted using drc)
#' @param cost_instant_list Vector of cost named variables that are accrued instantaneously at an event (discounted using drc)
#' @param cost_cycle_list Vector of cost named variables that are accrued in cycles (discounted using drc)
#' @param other_ongoing_list Vector of other named variables that are accrued at an ongoing basis (discounted using drq)
#' @param other_instant_list Vector of other named variables that are accrued instantaneously at an event (discounted using drq)
#' @param npats The number of patients to be simulated (it will simulate npats * length(arm_list))
#' @param n_sim The number of simulations to run per sensitivity
#' @param psa_bool A boolean to determine if PSA should be conducted. If n_sim > 1 and psa_bool = FALSE, the differences between simulations will be due to sampling
#' @param sensitivity_bool A boolean to determine if Scenarios/DSA should be conducted. 
#' @param sensitivity_names A vector of scenario/DSA names that can be used to select the right sensitivity (e.g., c("Scenario_1", "Scenario_2")). The parameter "sens_name_used" is created from it which corresponds to the one being used for each iteration.
#' @param n_sensitivity Number of sensitivity analysis (DSA or Scenarios) to run. It will be interacted with sensitivity_names argument if not null (n_sensitivityitivity = n_sensitivity * length(sensitivity_names)). For DSA, it should be as many parameters as there are. For scenario, it should be 1.
#' @param ncores The number of cores to use for parallel computing
#' @param input_out A vector of variables to be returned in the output data frame
#' @param ipd Integer taking value 0 if no IPD data returned, 1 for full IPD data returned, and 2 IPD data but aggregating events
#' @param timed_freq If NULL, it does not produce any timed outputs. Otherwise should be a number (e.g., every 1 year)
#' @param debug If TRUE, will generate a log file
#' @param accum_backwards If TRUE, the ongoing accumulators will count backwards (i.e., the current value is applied until the previous update). If FALSE, the current value is applied between the current event and the next time it is updated.
#'
#' @return A list of lists with the analysis results
#' @importFrom doFuture `%dofuture%`
#' @importFrom future plan
#' @importFrom future multisession
#' @importFrom foreach foreach
#' @importFrom progressr with_progress
#' @importFrom progressr handlers
#' @importFrom progressr handler_txtprogressbar
#'
#' @export
#' 
#' @details This function is slightly different from `run_sim`.
#' `run_sim` allows to run single-core.
#' `run_sim_parallel` allows to use multiple-core at the simulation level,
#' making it more efficient for a large number of simulations relative to `run_sim` (e.g., for  PSA).
#' 
#' Event ties are processed in the order declared within the `init_event_list` argument (`evts` argument within the first sublist of that object).
#' To do so, the program automatically adds a sequence from to 0 to the (number of events - 1) times 1e-10 to add to the event times when selecting the event with minimum time.
#' This time has been selected as it's relatively small yet not so small as to be ignored by which.min (see .Machine for more details)
#'  
#' A list of protected objects that should not be used by the user as input names or in the global environment to avoid the risk of overwriting them is as follows:
#' c("arm", "arm_list", "categories_for_export", "cur_evtlist", "curtime", "evt", "i", "prevtime", "sens", "simulation", "sens_name_used","list_env","uc_lists","npats","ipd").
#' 
#' The engine uses the L'Ecuyer-CMRG for the random number generator.
#' Note that if ncores > 1, then results per simulation will only be exactly replicable if using run_sim_parallel 
#' (as seeds are automatically transformed to be seven integer seeds -i.e, L'Ecuyer-CMRG seeds-)
#' 
#' If no `drc` or `drq` parameters are passed within any of the input lists, these are assigned value 0.03.
#' 
#' Ongoing items will look backward to the last time updated when performing the discounting and accumulation. 
#' This means that the user does not necessarily need to keep updating the value, but only add it when the value 
#' changes looking forward (e.g., o_q = utility at event 1, at event 2 utility does not change, but at event 3 it does, 
#' so we want to make sure to add o_q = utility at event 3 before updating utility. The program will automatically 
#' look back until event 1). Note that in previous versions of the package backward was the default, and now this has switched to forward.
#' 
#' If the `cycle` lists are used, then it is expected the user will declare as well the name of the variable
#'  pasted with `cycle_l` and `cycle_starttime` (e.g., c_default_cycle_l and c_default_cycle_starttime) to 
#'  ensure the discounting can be computed using cycles, with cycle_l being the cycle length, and cycle_starttime 
#'  being the starting time in which the variable started counting.
#' 
#' @examples
#' \dontrun{
#' run_sim_parallel(arm_list=c("int","noint"),
#' common_all_inputs = common_all_inputs,
#' common_pt_inputs = common_pt_inputs,
#' unique_pt_inputs = unique_pt_inputs,
#' init_event_list = init_event_list,
#' evt_react_list = evt_react_list,
#' util_ongoing_list = util_ongoing_list,
#' util_instant_list = util_instant_list,
#' cost_ongoing_list = cost_ongoing_list,
#' cost_instant_list = cost_instant_list,
#' npats = 500,
#' n_sim = 1,
#' psa_bool = FALSE,
#' ncores = future::availableCores(),
#' ipd = 1)
#' }

run_sim_parallel <- function(arm_list=c("int","noint"),
                             sensitivity_inputs=NULL,
                             common_all_inputs=NULL,
                             common_pt_inputs=NULL,
                             unique_pt_inputs=NULL,
                             init_event_list = NULL,
                             evt_react_list = evt_react_list,
                             util_ongoing_list = NULL,
                             util_instant_list = NULL,
                             util_cycle_list = NULL,
                             cost_ongoing_list = NULL,
                             cost_instant_list = NULL,
                             cost_cycle_list = NULL,
                             other_ongoing_list = NULL,
                             other_instant_list = NULL,
                             npats=500,
                             n_sim=1,
                             psa_bool = NULL,
                             sensitivity_bool = FALSE,
                             sensitivity_names = NULL,
                             n_sensitivity = 1,
                             ncores=1,
                             input_out = NULL,
                             ipd = 1,
                             timed_freq = NULL,
                             debug = FALSE,
                             accum_backwards = FALSE){
  
  
  # Set-up basics -----------------------------------------------------------
  
  #Store original rng kind and use L'Ecuyer-CMRG 
  rng_kind_store <- RNGkind()[1]
  RNGkind("L'Ecuyer-CMRG")
  
  #Stop simulation if forbidden objects are used.
  list_forbidden_names <-  c("arm", "arm_list", "categories_for_export", "cur_evtlist", "curtime", "evt", "i", "prevtime", "sens", "simulation", "sens_name_used","list_env","uc_lists","npats","ipd") 
  matched_list_forbidden <- which(list_forbidden_names %in% c(
    ls(parent.frame()),
    names(common_all_inputs),
    names(sensitivity_inputs),
    names(common_pt_inputs),
    names(unique_pt_inputs),
    names(arm_list),
    names(evt_react_list)
  )
  )
  
  if(  length(matched_list_forbidden)>0){
    stop(paste0("Name(s) or object `", list_forbidden_names[matched_list_forbidden],"` defined belong to the list of forbidden names, which can cause issues in the model.
         Please remove or rename those names. See run_sim or run_sim_parallel for a full list of these names.\n"))
  }
  
  
  if(length(names(evt_react_list)) != length(init_event_list[[1]][["evts"]])){
    dif <- setdiff(names(evt_react_list),init_event_list[[1]][["evts"]])
    stop(paste0("Number of events defined in init_event_list first sublist `evts` is not equal to the number of events defined in evt_react_list.
         Please ensure both have equal length, ",dif," is/are missing from one of the lists."))
  }
  
  plan(multisession, workers = ncores)
  
  arm_list <- arm_list #this is done as otherwise there are problems passing arguments from function to function
  
  #get cost/utility categories
  categories_costs_ongoing <- cost_ongoing_list
  categories_costs_instant <- cost_instant_list
  categories_costs_cycle   <- cost_cycle_list
  
  categories_utilities_ongoing <- util_ongoing_list
  categories_utilities_instant <- util_instant_list
  categories_utilities_cycle   <- util_cycle_list
  
  categories_other_ongoing <- other_ongoing_list
  categories_other_instant <- other_instant_list
  
  
  #Remove NULL values
  categories_for_export <- unique(
    c(if(!is.null(categories_costs_ongoing)){categories_costs_ongoing},
      if(!is.null(categories_costs_instant)){categories_costs_instant},
      if(!is.null(categories_costs_cycle)){categories_costs_cycle},
      if(!is.null(categories_utilities_ongoing)){categories_utilities_ongoing},
      if(!is.null(categories_utilities_instant)){categories_utilities_instant},
      if(!is.null(categories_utilities_cycle)){categories_utilities_cycle},
      if(!is.null(categories_other_ongoing)){categories_other_ongoing},
      if(!is.null(categories_other_instant)){categories_other_instant}
    ))
  
  
  output_sim <- list()
  
  log_out <- NULL
  if(debug){
    log_out <- vector(mode="list",length=npats*n_sim*n_sensitivity*50)
    index_log <- 1
  }
  
  start_time <-  proc.time()
  
  # Analysis loop ---------------------------------------------------------
  
  if (is.null(sensitivity_names)) {
    length_sensitivities <- n_sensitivity
  } else{
    length_sensitivities <- n_sensitivity * length(sensitivity_names)
  }
  
  for (sens in 1:length_sensitivities) {
    print(paste0("Analysis number: ",sens))
    start_time_analysis <-  proc.time()
    
    output_sim[[sens]] <- list() #initialize analysis lists
    
    #e.g., if length_sensitivities is 50 (25 param x 2 DSAs) then take at each iteration the divisor to see which name should be applied
    if (!is.null(sensitivity_names)) {
    sens_name_used <- sensitivity_names[ceiling(sens/n_sensitivity)] 
    } else{
    sens_name_used <- ""
    }
    
    
    input_list_sens <- list(
                       psa_bool = psa_bool,
                       init_event_list = init_event_list,
                       precision_times = if(!is.null(init_event_list)){setNames((seq_along(init_event_list[[1]][["evts"]])-1)*1e-10,
                                                                                init_event_list[[1]][["evts"]])},
                       evt_react_list = evt_react_list,
                       uc_lists = list(util_ongoing_list = util_ongoing_list,
                                       util_instant_list = util_instant_list,
                                       util_cycle_list = util_cycle_list,
                                       cost_ongoing_list = cost_ongoing_list,
                                       cost_instant_list = cost_instant_list,
                                       cost_cycle_list = cost_cycle_list,
                                       other_ongoing_list = other_ongoing_list,
                                       other_instant_list = other_instant_list,
                                       ongoing_inputs = c(
                                         if(!is.null(categories_costs_ongoing)){categories_costs_ongoing},
                                         if(!is.null(categories_utilities_ongoing)){categories_utilities_ongoing},
                                         if(!is.null(categories_other_ongoing)){categories_other_ongoing}
                                       ),
                                       instant_inputs = c(
                                         if(!is.null(categories_costs_instant)){categories_costs_instant},
                                         if(!is.null(categories_utilities_instant)){categories_utilities_instant},
                                         if(!is.null(categories_other_instant)){categories_other_instant}
                                       ),
                                       cycle_inputs = c(
                                         if(!is.null(categories_costs_cycle)){categories_costs_cycle},
                                         if(!is.null(categories_utilities_cycle)){categories_utilities_cycle}
                                       ),
                                       cost_categories_ongoing = categories_costs_ongoing,
                                       l_cost_categories_ongoing = length(categories_costs_ongoing),
                                       cost_categories_instant = categories_costs_instant,
                                       l_cost_categories_instant = length(categories_costs_instant),
                                       cost_categories_cycle = categories_costs_cycle,
                                       l_cost_categories_cycle = length(categories_costs_cycle),
                                       util_categories_ongoing = categories_utilities_ongoing,
                                       l_util_categories_ongoing = length(categories_utilities_ongoing),
                                       util_categories_instant = categories_utilities_instant,
                                       l_util_categories_instant = length(categories_utilities_instant),
                                       util_categories_cycle = categories_utilities_cycle,
                                       l_util_categories_cycle = length(categories_utilities_cycle),
                                       other_categories_ongoing = categories_other_ongoing,
                                       l_other_categories_ongoing = length(categories_other_ongoing),
                                       other_categories_instant = categories_other_instant,
                                       l_other_categories_instant = length(categories_other_instant)
                       ),
                       input_out = unique(c(input_out,categories_for_export)),
                       categories_for_export = categories_for_export,
                       ipd = ipd,
                       arm_list = arm_list,
                       npats = npats,
                       n_sim = n_sim,
                       sensitivity_bool = sensitivity_bool,
                       n_sensitivity = n_sensitivity,
                       sens = sens,
                       sensitivity_names = sensitivity_names,
                       sens_name_used = sens_name_used,
                       timed_freq = timed_freq,
                       debug = debug,
                       accum_backwards = accum_backwards,
                       index_log = 1
                      )
    
    set.seed(sens)
    
    # Draw Common parameters  -------------------------------
    sens_input_list <- list2env(input_list_sens)
    
    
    if(sens_input_list$debug){
      log_env(sens_input_list, clear = TRUE, add = FALSE)
    }
    
    eval(sensitivity_inputs[[1]]$inputs,envir = sens_input_list)
    
    if(sens_input_list$debug){
      log_out[[sens_input_list$index_log]] <- log_env(sens_input_list)
      sens_input_list$index_log <- sens_input_list$index_log + 1
    }
    
# Simulation loop ---------------------------------------------------------
    output_sim[[sens]] <- vector("list", length=n_sim) # empty list with n_sim elements
    # Outer loop, repeat for each patient
    exported_items <- unique(c("input_list_sens",ls(.GlobalEnv),ls(parent.env(environment())),ls(environment())))
    options(future.rng.onMisuse = "ignore")
    
    progressr::handlers(progressr::handler_txtprogressbar(width=100))

    progressr::with_progress({  
    output_sim[[sens]] <- foreach(simulation = 1:n_sim,
                         # .options.future = list(seed = TRUE),
                         .options.future = list(packages = .packages()),
                         .options.future = list(globals=structure(TRUE,add = exported_items)),
                         .combine = 'c') %dofuture% {
                           
      RNGkind("L'Ecuyer-CMRG") #repeat this here so parallel::RngStream does not malfunction
                            
      print(paste0("Simulation number: ",simulation))
                           
      start_time_sim <-  proc.time()
                           
                           
      input_list <- c(input_list_sens,
                      simulation = simulation)
      
      set.seed(simulation)
      
      # Draw Common parameters  -------------------------------
      env_input_list <- new.env(parent = parent.env(sens_input_list))
      list2env(as.list(sens_input_list, all.names = TRUE), envir = env_input_list)
      
      eval(common_all_inputs[[1]]$inputs,envir = env_input_list)
      
      if(env_input_list$debug){
        log_out[[env_input_list$index_log]] <- log_env(env_input_list)
        env_input_list$index_log <- env_input_list$index_log + 1
      }
  
      if(is.null(input_list$drc)){input_list$drc <- 0.03}
      if(is.null(input_list$drq)){input_list$drq <- 0.03}
      
      # Run engine ----------------------------------------------------------
  
        final_output <- RDICE:::run_engine(arm_list=arm_list,
                                        common_pt_inputs=common_pt_inputs,
                                        unique_pt_inputs=unique_pt_inputs,
                                        env_input_list = env_input_list,
                                        log_out = log_out)                    # run simulation
      
  
      if (env_input_list$ipd>0) {
  
        final_output$merged_df$simulation <- simulation
        final_output$merged_df$sensitivity <- sens
      }
      
      final_output <- c(list(sensitivity_name = sens_name_used), final_output)
      
      if(env_input_list$debug){
        final_output$log_list <-  unlist(final_output$log_list, recursive = FALSE)
        log_out <- final_output$log_list
      }

      return(list(final_output))
      
      print(paste0("Time to run simulation ", simulation,": ",  round(proc.time()[3]- start_time_sim[3] , 2 ), "s"))
     }
    }, enable=TRUE) 
    

    print(paste0("Time to run analysis ", sens,": ",  round(proc.time()[3]- start_time_analysis[3] , 2 ), "s"))
    
  }
  print(paste0("Total time to run: ",  round(proc.time()[3]- start_time[3] , 2), "s"))
  

  # Export results ----------------------------------------------------------
  
  # if(env_input_list$debug){
  #   export_log(log_out,paste0("log_model_",format(Sys.time(), "%Y_%m_%d_%Hh_%mm_%Ss"),".txt"))
  # }

  results <- output_sim
  
  RNGkind(rng_kind_store)
  
  return(results)

}
