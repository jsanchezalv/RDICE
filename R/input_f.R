
# Global variables for CRAN check -----------------------------------------

if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      c('psa_bool',
        'sens_bool',
        'evt_arm',
        'input_list_arm',
        'new_event_name')
    )) 
}

# Replicate profiles --------------------------------------------------------
#' Replicate profiles data.frame
#'
#' @param profiles data.frame of profiles
#' @param replications integer, final number of observations
#' @param probabilities vector of probabilities with the same length as the number of rows of profiles. Does not need to add up to 1 (are reweighted)
#' @param replacement Boolean whether replacement is used
#' @param seed_used Integer with the seed to be used for consistent results
#'
#' @return Resampled data.frame of profiles
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' replicate_profiles(profiles=data.frame(id=1:100,age=rnorm(100,60,5)),
#' replications=200,probabilities=rep(1,100))
#' }
replicate_profiles <- function(profiles,
                               replications,
                               probabilities = NULL,
                               replacement = TRUE,
                               seed_used = NULL
){
  
  if (!is.null(seed_used)) {
    set.seed(seed_used)
  }
  
  rows_sampled <- sample.int(nrow(profiles),size=replications,prob=probabilities,replace=replacement)
  output <- profiles[rows_sampled,]
  rownames(output) <- NULL  
  
  return(output)
}

# Create indicators for sensitivity/DSA analysis --------------------------------------------------------
#' Creates a vector of indicators (0 and 1) for sensitivity/DSA analysis
#'
#' @param sens current analysis iterator
#' @param n_sensitivity total number of analyses to be run
#' @param elem vector of 0s and 1s of elements to iterate through (1 = parameter is to be included in scenario/DSA)
#' @param n_elem_before Sum of 1s (# of parameters to be included in scenario/DSA) that go before elem
#'
#' @return Numeric vector composed of 0 and 1, where value 1 will be used by `pick_val_v` to pick the corresponding index in its `sens` argument
#' @export
#' 
#' @details
#' n_elem_before is to be used when several indicators want to be used (e.g., for patient level and common level inputs) while facilitating readibility of the code
#'
#' @examples
#' \dontrun{
#'create_indicators(10,20,c(1,1,1,1))
#'create_indicators(7,20,c(1,0,0,1,1,1,0,0,1,1),2)
#' }
create_indicators <- function(sens,n_sensitivity,elem,n_elem_before=0){
  
  if (n_sensitivity<sens) {
    stop("n_sensitivity is smaller than the iterator sens")
  }
  
  if (any(elem>1| elem<0)) {
    stop("One or more elements of elem is >1 or <0. It should be a vector of 0s and 1s.")
  }
  n_elem <- sum(elem)

    if (n_elem_before + n_elem >n_sensitivity) {
    stop("n_sensitivity is smaller than n_elem_before + sum(elem)")
  }

  n_elem_total <- length(elem)
  
  if ((n_elem + n_elem_before <sens) | (n_elem_before >=sens)) { #here fix
    out <- rep(0, n_elem_total)
  } else{
    # which position to use to put the value 1 in indicator
    pos_indicator <-  sens - n_sensitivity*floor((sens-1)/n_sensitivity)  - n_elem_before
    pos_indicator <- which(elem==1)[pos_indicator]
    
    out <- append(rep(0, n_elem_total)[-pos_indicator],1,pos_indicator-1) 
  }
  return(out)
}


# Helper to draw PSA values --------------------------------------------------------
#' Helper function to create a list with random draws or whenever a series of functions needs to be called. Can be implemented within `pick_val_v`.
#'
#' @param f A string or vector of strings with the function to be called, e.g., "rnorm"
#' @param ... parameters to be passed to the function (e.g., if "rnorm", arguments `n`, `mean`, `sd`)
#'
#' @return List with length equal to `f` of parameters called
#' @export
#' 
#' @details
#' This function can be used to pick values for the PSA within `pick_val_v.` 
#' 
#' The function will ignore NA items within the respective parameter (see example below).
#' If an element in f is NA (e.g., a non PSA input) then it will return NA as its value
#' This feature is convenient when mixing distributions with different number of arguments, e.g., `rnorm` and `rgengamma`.
#' 
#' While it's slightly lower than individually calling each function, it makes the code easier to read and more transparent
#'
#' @examples
#' \dontrun{
#' params <- list(
#' param=list("a","b"),
#' dist=list("rlnorm","rnorm"),
#' n=list(4,1),
#' a=list(c(1,2,3,4),1),
#' b=list(c(0.5,0.5,0.5,0.5),0.5),
#' dsa_min=list(c(1,2,3,4),2),
#' dsa_max=list(c(1,2,3,4),3)
#' )
#'pick_psa(params[["dist"]],params[["n"]],params[["a"]],params[["b"]])
#'
#'#It works with functions that require different number of parameters
#'params <- list(
#'  param=list("a","b","c"),
#'  dist=list("rlnorm","rnorm","rgengamma"),
#'  n=list(4,1,1),
#'  a=list(c(1,2,3,4),1,0),
#'  b=list(c(0.5,0.5,0.5,0.5),0.5,1),
#'  c=list(NA,NA,0.2),
#'  dsa_min=list(c(1,2,3,4),2,1),
#'  dsa_max=list(c(1,2,3,4),3,3)
#')
#'
#'pick_psa(params[["dist"]],params[["n"]],params[["a"]],params[["b"]],params[["c"]])
#'
#' #Can be combined with multiple type of functions and distributions if parameters are well located
# params <- list(
# param=list("a","b","c","d"),
# dist=list("rlnorm","rnorm","rgengamma","draw_tte"),
# n=list(4,1,1,1),
# a=list(c(1,2,3,4),1,0,"norm"),
# b=list(c(0.5,0.5,0.5,0.5),0.5,1,1),
# c=list(NA,NA,0.2,0.5), #NA arguments will be ignored
# c=list(NA,NA,NA,NA), #NA arguments will be ignored
# dsa_min=list(c(1,2,3,4),2,1,0),
# dsa_max=list(c(1,2,3,4),3,3,2)
# )
#'
#'#'params <- list(
#' param=list("a","b","c","d"),
#' dist=list("rlnorm","rnorm","rgengamma","draw_tte"),
#' n=list(4,1,1,1),
#' a=list(c(1,2,3,4),1,0,"norm"),
#' b=list(c(0.5,0.5,0.5,0.5),0.5,1,1),
#' c=list(NA,NA,0.2,0.5),
#' c=list(NA,NA,NA,NA), #NA arguments will be ignored
#' dsa_min=list(c(1,2,3,4),2,1,0),
#' dsa_max=list(c(1,2,3,4),3,3,2)
#' )
#'
#'
#'
#' }
#' 
pick_psa <- function(f,...){
  args_called <- list(...)
  sapply(1:length(f), function(x) {
    if(!is.na(f[[x]])){
    args <- lapply(args_called, `[[`, x)
    args <- args[!sapply(args, function(x) any(is.na(x)))]
    do.call(f[[x]], args)
    } else{
      NA
    }
  })
}


# Select which values to apply --------------------------------------------------------
#' Select which values should be applied in the corresponding loop for several values (vector or list).
#'
#' @param base Value if no PSA/DSA/Scenario
#' @param psa Value if PSA
#' @param sens Value if DSA/Scenario
#' @param psa_ind Boolean whether PSA is active
#' @param sens_ind Boolean whether Scenario/DSA is active
#' @param indicator Indicator which checks whether the specific parameter/parameters is/are active in the DSA or Scenario loop 
#' @param indicator_psa Indicator which checks whether the specific parameter/parameters is/are active in the PSA loop.
#'  If NULL, it's assumed to be a vector of 1s of length equal to length(indicator)
#' @param names_out Names to give the output list
#' @param indicator_sens_binary Boolean, TRUE if parameters will be varied fully, FALSE if some elements of the parameters may be changed but not all
#' @param sens_iterator Current iterator number of the DSA/scenario being run, e.g., 5 if it corresponds to the 5th DSA parameter being changed
#' @param distributions List with length equal to length of base where the distributions are stored
#' @param covariances List with length equal to length of base where the variance/covariances are stored (only relevant if multivariate normal are being used)
#'
#' @return List used for the inputs
#' @export
#' 
#' @details
#' This function can be used with vectors or lists, but will always return a list.
#' Lists should be used when correlated variables are introduced to make sure the selector knows how to choose among those
#' This function allows to choose between using an approach where only the full parameters are varied, and an approach where subelements of the parameters can be changed
#'
#' @examples
#' \dontrun{
#' pick_val_v(base = list(0,0),
#'              psa =list(rnorm(1,0,0.1),rnorm(1,0,0.1)),
#'              sens = list(2,3),
#'              psa_ind = FALSE,
#'              sens_ind = TRUE,
#'              indicator=list(1,2),
#'              indicator_sens_binary = FALSE,
#'              sens_iterator = 2,
#'              distributions = list("rnorm","rnorm")
#' )
#' 
#' pick_val_v(base = list(2,3,c(1,2)),
#'              psa =sapply(1:3,
#'                          function(x) eval(call(
#'                            c("rnorm","rnorm","mvrnorm")[[x]],
#'                            1,
#'                            c(2,3,list(c(1,2)))[[x]],
#'                            c(0.1,0.1,list(matrix(c(1,0.1,0.1,1),2,2)))[[x]]
#'                          ))),
#'              sens = list(4,5,c(1.3,2.3)),
#'              psa_ind = FALSE,
#'              sens_ind = TRUE,
#'              indicator=list(1,2,c(3,4)),
#'              names_out=c("util","util2","correlated_vector") ,
#'              indicator_sens_binary = FALSE,
#'              sens_iterator = 4,
#'              distributions = list("rnorm","rnorm","mvrnorm"),
#'              covariances = list(0.1,0.1,matrix(c(1,0.1,0.1,1),2,2))
#' )
#'  
#'} 
pick_val_v <- function(base,
                         psa,
                         sens,
                         psa_ind = psa_bool,
                         sens_ind = sens_bool,
                         indicator,
                         indicator_psa = NULL,
                         names_out=NULL,
                         indicator_sens_binary = TRUE,
                         sens_iterator = NULL,
                         distributions = NULL,
                         covariances = NULL
){
  
  output <- list()
  
  if(indicator_sens_binary == FALSE & sens_ind == TRUE){ #if the indicators for the sensitivity analyses are not 0/1 but instead a list of integers (elements of parameters)
    
    if(is.null(sens_iterator)|is.null(distributions)){
      stop("sens_iterator nor distributions arguments cannot be NULL if indicator_sens_binary argument is FALSE. sens_indicator should take the value of the corresponding DSA/scenario iterator")
    }
    
    if(psa_ind){
      temp_data <- psa #preassign base value
    }else{
      temp_data <- base #preassign base value
    }
    output <- temp_data #preassign base value
    
    
    #Iterate over each parameter, check which elements are active in the current iterator, then check if an adjustment needs to be done 
    for (i in 1:length(base)) {
      ind <- indicator[[i]]==sens_iterator
      
      if(sum(ind)==0){next}
      
      #If not mvrnorm or dirichlet, just update relevant value and leave remaining as they are 
      #Adjustment to mvrnorm or dirichlet is needed only if sum of parameters that need adjustment is < than length of elements in parameter
      if(!distributions[[i]] %in% c("mvrnorm","rdirichlet") | sum(ind)==length(temp_data[[i]])){
        output[[i]][ind] <- sens[[i]][ind] 
        
      } else if(distributions[[i]]=="mvrnorm"){
        #If distribution is mvrnorm, recalculate remaining values using conditional
        output[[i]] <- conditional_mvn(temp_data[[i]], covariances[[i]], which(ind), sens[[i]][ind], full_output = TRUE)[[1]]
        
      } else { 
        #If distribution is dirichlet, recalculate remaining values using conditional
        output[[i]] <- conditional_dirichlet(temp_data[[i]], which(ind), sens[[i]][ind], full_output = TRUE)[[1]]
      }
    }
    
  } else{ #If using old approach of single parameters being changed simultaneously
    
    if ((any(!indicator %in% c(0,1)) & indicator_sens_binary == TRUE) | (!psa_ind %in% c(0,1)) | (!sens_ind %in% c(0,1)) ) {
      stop("Indicator, psa_ind or sens_ind are not FALSE/TRUE (or 0/1)")
    }
    
    len_ind <- length(indicator)
    
    if (is.null(indicator_psa)) {
      indicator_psa <- rep(1,len_ind)
    }else{
      if(len_ind != length(indicator_psa)){
        stop("Length of indicator vector is different than length of indicator_psa")
      }
    }
    
    #if the parameter is out of the dsa/scenario specific iteration, or not in DSA/scenario, use PSA/normal. 
    for (it in 1:len_ind) {
      output[[it]] <-  if (indicator[[it]][1]==0 | sens_ind==F ) { 
        if (psa_ind==T & indicator_psa[it]==1) {
          psa[[it]]
        } else {
          base[[it]]
        }
      } else {#If active, use DSA if DSA and scenario if scenario
        sens[[it]]
      } 
    }
    
  }
  
  if (!is.null(names_out)) {
    names(output) <- names_out
  }
  
  return(output)
} 


# Add item/parameter to list --------------------------------------------------------

#' Defining parameters that may be used in model calculations
#'
#' @param .data Existing data
#' @param ... Items to define for the simulation
#'
#' @return A list of items
#' @export
#'
#' @details
#' The functions to add/modify events/inputs use lists. Whenever several inputs/events are added or modified, it's recommended to group them within one function, as it reduces the computation cost.
#' So rather than use two `add_item` with a list of one element, it's better to group them into a single `add_item` with a list of two elements.
#'
#' Whenever a function is directly implemented which must be evaluated later and that has no object name attached (e.g., `pick_val_v`), 
#' it should be implemented after a first `add_item()` (empty or with content) to avoid confusing the `.data` argument, or wrapping the function within `substitute()`
#'
#' @examples
#' \dontrun{
#' add_item(fl.idfs = 0)
#' add_item(util_idfs = if(psa_bool){rnorm(1,0.8,0.2)} else{0.8}, util.mbc = 0.6, cost_idfs = 2500)
#' common_inputs <- add_item() %>%
#' add_item(pick_val_v(
#'   base      = l_statics[["base"]],
#'   psa       = pick_psa(
#'     l_statics[["function"]],
#'     l_statics[["n"]],
#'     l_statics[["a"]],
#'     l_statics[["b"]]
#'   ),
#'   sens      = l_statics[[sens_name_used]],
#'   psa_ind   = psa_bool,
#'   sens_ind  = sensitivity_bool,
#'   indicator = indicators_statics,
#'   names_out = l_statics[["parameter_name"]]
#' )
#' )
#' }
#'

add_items <- function(input = NULL){
  
  list_item <- list(list(inputs=substitute(input))) 
  
  return(list_item)
}


# Add_reactevt -------------------------------------------------------------------------------------------------------------------------------------------

#' Define the modifications to other events, costs, utilities, or other items affected by the occurrence of the event
#'
#' @param .data Existing data for event reactions
#' @param name_evt Name of the event for which reactions are defined.
#' @param input Expressions that define what happens at the event, using functions as defined in the Details section
#'
#' @export
#'
#' @details
#' There are a series of objects that can be used in this context to help define the event reactions.
#'
#' The following functions may be used to define event reactions within this `add_reactevt()` function:
#' `modify_item()` | Adds & Modifies items/flags/variables for future events
#' `new_event()` | Adds events to the vector of events for that patient
#' `modify_event()` | Modifies existing events by changing their time
#'
#' Apart from the items defined with add_item(), we can also use standard variables that are always defined within the simulation:
#' `curtime` | Current event time (numeric)
#' `prevtime` | Time of the previous event (numeric)
#' `cur_evtlist` | Named vector of events that is yet to happen for that patient (named numeric vector)
#' `evt` | Current event being processed (character)
#' `i` | Patient being iterated (character)
#' `simulation` | Simulation being iterated (numeric)
#'
#' The model will run until `curtime` is set to `Inf`, so the event that terminates the model should modify `curtime` and set it to `Inf`.
#'
#' @examples
#' \dontrun{
#' add_reactevt(name_evt = "start",input = {})
#' add_reactevt(name_evt = "idfs",input = {modify_item(list("fl.idfs"= 0))})
#' }

add_reactevt <- function(.data=NULL,name_evt,input){


  data_list <- .data

  evt_r <- list(list(react=substitute(input)))
  
  names(evt_r) <- paste(name_evt)

  if (is.null(data_list)) {
    data_list <- evt_r
  } else{
    data_list <- append(data_list,evt_r)
  }


  return(data_list)
}



# Add drawing and initial event list -------------------------------------------------------------------------------------------------------------------------------------------

#' Define events and the initial event time
#'
#' @param .data Existing data for initial event times
#' @param arm The intervention for which the events and initial event times are defined
#' @param evts A vector of the names of the events
#' @param other_inp A vector of other input variables that should be saved during the simulation
#' @param input The definition of initial event times for the events listed in the evts argument
#'
#' @return A list of initial events and event times
#' @export
#'
#' @details
#' Events need to be separately defined for each intervention.
#'
#' For each event that is defined in this list, the user needs to add a reaction to the event using the `add_reactevt()` function which will determine what calculations will happen at an event.
#'
#' @examples
#' \dontrun{
#' add_tte(arm="int",evts = c("start","ttot","idfs","os"),
#' input={
#' start <- 0
#' idfs <- draw_tte(1,'lnorm',coef1=2, coef2=0.5)
#' ttot <- min(draw_tte(1,'lnorm',coef1=1, coef2=4),idfs)
#' os <- draw_tte(1,'lnorm',coef1=0.8, coef2=0.2)
#' })
#' }
#'
add_tte <- function(.data=NULL,arm, evts, other_inp = NULL,input){
  data_list <- .data

  for (arm_i in arm) {

    if (!is.character(other_inp) & !is.null(other_inp)) {
      stop("other_inp argument is required to be a character vector or be set to NULL")
    }

    if (!is.character(evts) | length(evts)<2) {
      stop("evts argument in add_tte for the intervention ", arm_i, " is required to be a character vector with length >1")
    }


    evt_l <- list(list(expr=substitute(input),
                       evts = evts,
                       other_inp = other_inp
    ))
    names(evt_l) <- paste(arm_i)

    if (is.null(data_list)) {
      data_list <- evt_l
    } else{
      data_list <- append(data_list,evt_l)
    }

  }

  if (length(names(data_list))!=length(unique(names(data_list)))) {
    stop("More than one initial set of time to events has been defined for an intervention.")
  }

  return(data_list)
}

# Perform luck adjustment when using conditional quantile -------------------------------------------------------------------------------------------------------------------------------

#' Perform luck adjustment
#'
#' @param prevsurv Value of the previous survival
#' @param cursurv Value of the current survival
#' @param luck Luck used to be adjusted (number between 0 and 1)
#' @param condq Conditional quantile approach or standard approach
#'
#' @export
#' 
#' @return Adjusted luck number between 0 and 1
#'
#' @details
#' This function performs the luck adjustment automatically for the user, returning the adjusted luck number.
#' Luck is interpreted in the same fashion as is standard in R (higher luck, higher time to event).
#' 
#' Note that if TTE is predicted using a conditional quantile function (e.g., restricted gompertz, conditional quantile weibull...) `prevsurv` and `cursurv`
#' are the unconditional survival using the "previous" parametrization but at the previous time for `presurv` and at the current time for `cursurv`.
#' For other distributions, `presurv` is the survival up to current time using the previous parametrization, and `cursurv` 
#' is the survival up to current time using the current parametrization.
#' 
#' Note that the advantage of the conditional quantile function is that it does not need the new parametrization to update the luck, 
#' which makes this approach computationally more efficient.
#' This function can also work with vectors, which could allow to update multiple lucks in a single approach, and it can preserve names
#'
#' @examples
#' luck_adj(prevsurv = 0.8,
#'  cursurv = 0.7,
#'  luck = 0.5,
#'  condq = TRUE)
#'  
#' luck_adj(prevsurv = c(1,0.8,0.7),
#'  cursurv = c(0.7,0.6,0.5),
#'  luck = setNames(c(0.5,0.6,0.7),c("A","B","C")),
#'  condq = TRUE)
#'  
#' luck_adj(prevsurv = 0.8,
#'  cursurv = 0.7,
#'  luck = 0.5,
#'  condq = FALSE) #different results
#' 
#' #Unconditional approach, timepoint of change is 25,
#' # parameter goes from 0.02 at time 10 to 0.025 to 0.015 at time 25,
#' #  starting luck is 0.37
#' new_luck <- luck_adj(prevsurv = 1 - pweibull(q=10,3,1/0.02),
#'  cursurv = 1 - pweibull(q=10,3,1/0.025),
#'  luck = 0.37,
#'  condq = FALSE) #time 10 change
#'  
#' new_luck <- luck_adj(prevsurv = 1 - pweibull(q=25,3,1/0.025),
#'  cursurv = 1 - pweibull(q=25,3,1/0.015),
#'  luck = new_luck,
#'  condq = FALSE) #time 25 change
#'  
#' qweibull(new_luck, 3, 1/0.015) #final TTE 
#' 
#' #Conditional quantile approach 
#' new_luck <- luck_adj(prevsurv = 1-pweibull(q=0,3,1/0.02),
#'                       cursurv = 1- pweibull(q=10,3,1/0.02),
#'                       luck = 0.37,
#'                       condq = TRUE) #time 10 change, previous time is 0 so prevsurv will be 1
#' 
#' new_luck <- luck_adj(prevsurv = 1-pweibull(q=10,3,1/0.025),
#'                       cursurv = 1- pweibull(q=25,3,1/0.025),
#'                       luck = new_luck,
#'                       condq = TRUE) #time 25 change
#' 
#' conditional_qweibull(rnd = new_luck,
#'                      shape = 3,
#'                      scale = 1/0.015,
#'                      lower_bound = 25) + 25 #final TTE

luck_adj <- function(prevsurv,cursurv,luck,condq=TRUE){
  
  #If length is 1, go for faster approach, otherwise use vectorial approach
  if(length(prevsurv)==1){
    if(condq==TRUE){
      adj_luck <-  max(1e-5, min(0.99999, if(prevsurv != 0){ 1 - ((1 - luck) * (prevsurv/cursurv)) } else{ luck }))
    }else{
      adj_luck <-  max(1e-5, min(0.99999, if(prevsurv != 0){ 1 - ((1 - luck) * (cursurv/prevsurv)) } else{ luck }))
    }
  } else{
    if(condq==TRUE){
      adj_luck <-  pmax(1e-5, pmin(0.99999, ifelse(prevsurv != 0, 1 - ((1 - luck) * (prevsurv/cursurv)), luck )))
    }else{
      adj_luck <-   pmax(1e-5, pmin(0.99999, ifelse(prevsurv != 0, 1 - ((1 - luck) * (cursurv/prevsurv)), luck )))
    }
  }
  
  #preserve names if existing
  names_luck <- names(luck)
  if(!is.null(names_luck)){
    names(adj_luck) <-names_luck
  }
  
  return(adj_luck)
}

# Continuous and instantaneous discounting ----------------------------------------------------------------------------------------------------------------

#' Calculate discounted costs and qalys between events
#'
#' @param lcldr The discount rate 
#' @param lclprvtime The time of the previous event in the simulation
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The value to be discounted
#'
#' @return Double based on continuous time discounting
#'
#' @examples \dontrun{
#' disc_ongoing(lcldr=0.035,lclprvtime=0.5, lclcurtime=3, lclval=2500)
#' }
#'
#' @export

disc_ongoing <- function(lcldr=0.035, lclprvtime, lclcurtime, lclval){
  
  Instantdr <- log(1+lcldr)
  
  # calculate additional qalys
  if (is.null(lclval)) {
    add <- 0
  } else if(lcldr==0) {
    add <- lclval*(lclcurtime - lclprvtime)
  } else{
    add <- ((lclval)/(0 - Instantdr)) * (exp(lclcurtime * ( 0 - Instantdr)) - exp(lclprvtime * (0 - Instantdr)))
    
  }
  
  return(add)
}


#' Calculate discounted costs and qalys between events for vectors
#'
#' @param lcldr The discount rate 
#' @param lclprvtime The time of the previous event in the simulation
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The value to be discounted
#'
#' @return Double based on continuous time discounting
#'
#' @examples \dontrun{
#' disc_ongoing_v(lcldr=0.035,lclprvtime=0.5, lclcurtime=3, lclval=2500)
#' }
#'
#' @export

disc_ongoing_v <- function(lcldr=0.035, lclprvtime, lclcurtime, lclval){
  
  Instantdr <- log(1+lcldr)
  
  # calculate additional qalys
  if(lcldr==0) {
    add <- lclval*(lclcurtime - lclprvtime)
  } else{
    add <- ((lclval)/(0 - Instantdr)) * (exp(lclcurtime * ( 0 - Instantdr)) - exp(lclprvtime * (0 - Instantdr)))
    
  }
  
  return(add)
}

#' Calculate instantaneous discounted costs or qalys for vectors
#'
#' @param lcldr The discount rate
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The value to be discounted
#'
#' @return Double based on discrete time discounting
#'
#' @examples \dontrun{
#' disc_instant_v(lcldr=0.035, lclcurtime=3, lclval=2500)
#' }
#'
#' @export
#' 
disc_instant_v <- function(lcldr=0.035, lclcurtime, lclval){
  

    addinst <- lclval * ((1+lcldr)^(-lclcurtime))    

  return(addinst)
}

#' Calculate instantaneous discounted costs or qalys
#'
#' @param lcldr The discount rate
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The value to be discounted
#'
#' @return Double based on discrete time discounting
#'
#' @examples \dontrun{
#' disc_instant(lcldr=0.035, lclcurtime=3, lclval=2500)
#' }
#'
#' @export
#' 
disc_instant <- function(lcldr=0.035, lclcurtime, lclval){
  
  if (is.null(lclval)) {
    addinst <- 0
  } else{
    addinst <- lclval * ((1+lcldr)^(-lclcurtime))    
  }# Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  return(addinst)
}

# Cycle discounting -------------------------------------------------------

#' Cycle discounting
#'
#' @param lcldr The discount rate
#' @param lclprvtime The time of the previous event in the simulation
#' @param cyclelength The cycle length
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The  value to be discounted
#' @param starttime The start time for accrual of cycle costs (if not 0)
#'
#' @return Double based on cycle discounting
#'
#' @examples \dontrun{
#' disc_cycle(lcldr=0.035, lclprvtime=0, cyclelength=1/12, lclcurtime=2, lclval=500,starttime=0)
#' }
#'
#' @export

disc_cycle <- function(lcldr=0.035, lclprvtime=0, cyclelength,lclcurtime, lclval,starttime=0){
  
  addcycle <- 0
  
  if (is.null(lclval) ) {
    addcycle <- 0
  } else{
    
    #Note this makes the cycle utilities work weird, so do not use cycle utilities for now!
    for (i in 1:length(lclval)) {
      lclval_i <- lclval[i]
      starttime_i <- starttime[i]
      cyclelength_i <- cyclelength[i]
      
      if (lclval_i==0 ) {} else{
        
        cycle.time.total <- if(starttime_i>= lclcurtime){0}else{seq(from=starttime_i, to = lclcurtime , by= cyclelength_i)} #all cycles that happened until current time of event
        
        #If the cost starts at the selected starttime or at time 0, then include that time, otherwise exclude it
        if (lclprvtime==0) {
          cycle.time <- c(0,cycle.time.total[cycle.time.total >= lclprvtime])  #times at which the cycles take place during this event, put this condition to count also time 0
          n_cycles <- length(unique(cycle.time))
          s <- (1+lcldr)^cyclelength_i -1
          if (lcldr==0) {
            addcycle <- sum(addcycle, lclval_i * n_cycles)
          } else{
            addcycle <- sum(addcycle,lclval_i * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1) )
          }
        } else{
          if (starttime_i ==lclprvtime & lclprvtime==0) {
            cycle.time <- cycle.time.total[cycle.time.total >= lclprvtime]  #times at which the cycles take place during this event, put this condition to count also time of the previous event
          } else{
            cycle.time <- cycle.time.total[cycle.time.total > lclprvtime]  #times at which the cycles take place during this event
          }
          n_cycles_remaining <- length(cycle.time)
          d <- lclprvtime/cyclelength_i
          s <- (1+lcldr)^cyclelength_i -1
          if (lcldr==0) {
            addcycle <- sum(addcycle, lclval_i * n_cycles_remaining)
          } else{
            addcycle <- sum(addcycle, lclval_i * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d)) )
          }
          
        }
        
        #If starting from 0, can be changed substituting interest rate such that s = (1+r)^cyclelength - 1, and using the formula that lclvalq * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1)
        #If starting from time t, then compute transformed time as d = t/cyclelength and use lclvalq * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d-1)), where
        #n_cycles_remaining is the n_cycles - d (so the remaining cycles to be considered), e.g. if 13 cycles (From t=0), and delay 6 periods, then n_cycles_remaining = 7 and d=6
        
      }
      
    }
  }
  return(addcycle)
}

#' Cycle discounting for vectors
#'
#' @param lcldr The discount rate
#' @param lclprvtime The time of the previous event in the simulation
#' @param cyclelength The cycle length
#' @param lclcurtime The time of the current event in the simulation
#' @param lclval The  value to be discounted
#' @param starttime The start time for accrual of cycle costs (if not 0)
#'
#' @return Double based on cycle discounting
#'
#' @examples \dontrun{
#' disc_cycle_v(lcldr=0.035, lclprvtime=0, cyclelength=1/12, lclcurtime=2, lclval=500,starttime=0)
#' }
#'
#' @export

disc_cycle_v <- function(lcldr=0.035, lclprvtime=0, cyclelength,lclcurtime, lclval,starttime=0){
  
  addcycle <- rep(0,length(lclval))
  
  #Note this makes the cycle utilities work weird, so do not use cycle utilities for now!
  for (i in 1:length(lclval)) {
    lclval_i <- lclval[i]
    starttime_i <- starttime[i]
    cyclelength_i <- cyclelength[i]
    lclcurtime_i <- lclcurtime[i]
    lclprvtime_i <- lclprvtime[i]
    
    
    if (lclval_i==0 ) {} else{
      
      cycle.time.total <- if(starttime_i>= lclcurtime_i){0}else{seq(from=starttime_i, to = lclcurtime_i , by= cyclelength_i)} #all cycles that happened until current time of event
      
      #If the cost starts at the selected starttime or at time 0, then include that time, otherwise exclude it
      if (lclprvtime_i==0) {
        cycle.time <- c(0,cycle.time.total[cycle.time.total >= lclprvtime_i])  #times at which the cycles take place during this event, put this condition to count also time 0
        n_cycles <- length(unique(cycle.time))
        s <- (1+lcldr)^cyclelength_i -1
        if (lcldr==0) {
          addcycle[i] <- sum(addcycle[i],lclval_i * n_cycles )
        } else{
          addcycle[i] <- sum(addcycle[i],lclval_i * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1) )
        }
      } else{
        if (starttime_i ==lclprvtime_i & lclprvtime_i==0) {
          cycle.time <- cycle.time.total[cycle.time.total >= lclprvtime_i]  #times at which the cycles take place during this event, put this condition to count also time of the previous event
        } else{
          cycle.time <- cycle.time.total[cycle.time.total > lclprvtime_i]  #times at which the cycles take place during this event
        }
        n_cycles_remaining <- length(cycle.time)
        d <- lclprvtime_i/cyclelength_i
        s <- (1+lcldr)^cyclelength_i -1
        if (lcldr==0) {
          addcycle[i] <- sum(addcycle[i], lclval_i * n_cycles_remaining )
        } else{
          addcycle[i] <- sum(addcycle[i], lclval_i * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d)) )
        }
      }
      
      #If starting from 0, can be changed substituting interest rate such that s = (1+r)^cyclelength - 1, and using the formula that lclvalq * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1)
      #If starting from time t, then compute transformed time as d = t/cyclelength and use lclvalq * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d-1)), where
      #n_cycles_remaining is the n_cycles - d (so the remaining cycles to be considered), e.g. if 13 cycles (From t=0), and delay 6 periods, then n_cycles_remaining = 7 and d=6
    }
    
    
  }
  return(addcycle)
}


#' Reverts list
#'
#' @param ls List
#'
#' @return Reverted list
#'
#' @examples \dontrun{
#' revert_list(ls=list(a=list(ab=1,ac=2),b=list(ab=2,ac=3)))
#' }
#'
#' @export

revert_list <- function(ls) {
  if(!is.list(ls)){stop("Object passed is not a list")}
  
  x <- lapply(ls,`[`, names(ls[[1]]))
  apply(do.call(rbind, x), 2, as.list)
}


#' Creates a random uniform 0-1 for a given random seed substream and updates the value
#'
#' @param n Number of elements
#' @param random_seed Substream to be used
#'
#' @return A vector of uniform values between 0 and 1
#'
#' @examples \dontrun{
#' x <- .Random.seed
#' runif_stream(n=1, x)
#' }
#'
#' @export
runif_stream <- function(n=1,random_seed){
    temp_seed <- .Random.seed
    assign(".Random.seed", random_seed, envir = .GlobalEnv)
    out <- runif(n=n)
    name_temp <- as.character((substitute(random_seed)))
    assign(name_temp, .Random.seed, envir = parent.frame())
    assign(".Random.seed", temp_seed, envir = .GlobalEnv)
  
  return(out)
} 
