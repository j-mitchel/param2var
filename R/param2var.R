

#' This function assigns a comma separated list of variables to global variables
#' and can assign default parameters from any given function to global variables.
#'
#' @param def_fn character The name of a function. The default parameters are extracted
#' from the specified function and set to global variables. Other explicitly set
#' parameters will override these defaults during assignment. (default=NULL)
#' @param rm_vars logical Whether to remove or assign the specified variables and
#' default parameters. Set to TRUE to remove these from the workspace. (default=FALSE)
#' @param ... Any other variables/parameters to be set
#'
#' @return No value is returned. The function is called for it's side effect of
#' assigning and removing global variables.
#' @export
#'
#' @examples
#' # Say you have a function with many parameters that you want to debug, such as the following:
#' my_test_function <- function(p1, p2='foo', p3=10, p4=NULL, p5=12, p6='baz') {
#'      message('This function does nothing')
#' }
#'
#' # Call param2var() to assign specified values to global variables with the same
#' # names as the parameters in the above function. In this example, we are going to
#' # set specific values for the parameters p1 and p2. By setting def_fn=my_test_function,
#' # you automatically assign the rest that function's default parameters to global variables.
#' param2var(def_fn=my_test_function, p1=99, p2='bar')
#'
#' # After debugging, all of the variables we just set can easily be removed by
#' # setting rm_vars=FALSE
#' param2var(def_fn=my_test_function, p1=99, p2='bar', rm_vars=FALSE)
#'
param2var <- function(def_fn=NULL,rm_vars=FALSE,...) {
  myvals <- list(...)
  print(myvals)
  # to assign the default values for the unspecified parameters
  if (!is.null(def_fn)) {
    default_vals <- formals(def_fn) # extract default param values from function
    all_param <- unique(c(names(myvals),names(default_vals))) # get all param names
    all_param <- all_param[all_param!='...'] # remove ellipsis param if present
    myvals <- sapply(all_param, function(p) {
      if (p %in% names(myvals)) {
        return(myvals[[p]])
      } else {
        return(default_vals[[p]])
      }
    }, USE.NAMES = TRUE)
  }

  if (rm_vars) { # if user wants to delete these variables from workspace
    rm(list=names(myvals), envir = .GlobalEnv)
  } else { # otherwise, set the global variables
    vars_missing_vals <- c()
    message('Assigning variables:')
    for (i in 1:length(myvals)) {
      if (class(myvals[[i]])=='name') { # if param has no default or assigned value
        vars_missing_vals <- c(names(myvals)[i], vars_missing_vals)
      } else {
        if (class(myvals[[i]])=='character') {
          message(paste0(names(myvals)[i], ' <- ', "'", myvals[[i]], "'"))
        } else if (is.null(myvals[[i]])) {
          message(paste0(names(myvals)[i], ' <- ', 'NULL'))
        } else {
          message(paste0(names(myvals)[i], ' <- ', myvals[[i]]))
        }
        assign(names(myvals)[i], myvals[[i]], inherits = FALSE, envir = .GlobalEnv)
      }
    }

    if (length(vars_missing_vals) > 0) {
      warning(paste0("No values were assigned for the following parameters without defaults: ",
                     paste(vars_missing_vals, collapse=', ')))
    }
  }
}





