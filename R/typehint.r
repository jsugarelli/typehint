#' @title Package 'typehint'
#'
#'
#'
#' @description
#'
#' Automatically check the data type, number and size of dimensions, and values of
#' function arguments with simple type hint comments in the function code.
#'
#' @details
#'
#' Type hints are special comments with a leading \code{#|} within a
#'   function body indicating the intended nature of the function's arguments in
#'   terms of data types, dimensions and even permitted values. The actual
#'   parameters with which the function is called can be evaluated against these
#'   type hint comments using the \code{check_types()} function.
#'
#' @section Author:
#' Joachim Zuckarelli, joachim@zuckarelli.de, @jsugarelli
#'
#' @family typehint
#' @name typehint
NULL



oneifnull <- function(x) {
  if(is.null(x)) return(1)
  else return(x)
}



prep_msg <- function(templates, msg.index, fun.name, arg.name, arg.val, type.req = "", type.is = "", dimcnt.req = 0, dimcnt.is = 0, dim.no = 0, dim.req = 0, dim.is = 0, dim.comp = "") {
  msg <- stringr::str_replace_all(templates[msg.index], "#type_req", as.character(type.req))
  msg <- stringr::str_replace_all(msg, "#type_is", as.character(type.is))
  if(oneifnull(length(arg.val)) > 1 | prod(oneifnull(dim(arg.val))) > 1) {
      arg.val <- paste0("\n\n", paste0(utils::capture.output(print(arg.val)), collapse = "\n"), "\n\n")
  }
  else {
    if(is.null(arg.val)) {
      arg.val <- "NULL"
    }
    else {
      if(is.na(arg.val)) {
        arg.val <- "NA"
      }
      else {
        if(class(arg.val) == "character") arg.val <- paste0("\"", arg.val, "\"")
      }
    }
  }
  msg <- stringr::str_replace_all(msg, "#argval", paste0(as.character(arg.val), collapse = ", "))
  msg <- stringr::str_replace_all(msg, "#arg", as.character(arg.name))
  msg <- stringr::str_replace_all(msg, "#dimcnt_is", as.character(dimcnt.is))
  msg <- stringr::str_replace_all(msg, "#dimcnt_req", as.character(dimcnt.req))
  msg <- stringr::str_replace_all(msg, "#dimno", as.character(dim.no))
  msg <- stringr::str_replace_all(msg, "#dimcomp", as.character(dim.comp))
  msg <- stringr::str_replace_all(msg, "#dim_req", as.character(dim.req))
  msg <- stringr::str_replace_all(msg, "#dim_is", as.character(dim.is))
  msg <- paste0(stringr::str_replace_all(templates[1], "#fun", as.character(fun.name)), msg,"\n")
  return(msg)
}



get_argchecks <- function(code) {
  argchecks <- list()

  if(NROW(code) > 0) {

    comp.ops <- c(">", ">=", "=", "<", "<=")

    for(i in 1:NROW(code)) {

      # Identify argument name and type
      args <- stringr::str_replace_all(code[i], c("#\\|" = "", "dim\\([^\\)]*\\)" = "", "not\\([^\\)]*\\)" = ""))
      args <- stringr::str_match(args, "\\s*(\\S*)\\s*(\\S*)")

      if(args[1, 1] == "" || args[1, 2] == "") {
        stop(paste0("\nError in \n'", code[i],"'\n. This is not a valid type hint because it lacks either the argument name, the required type, or both.\n"))
      }
      else {

        # Check if argument is already on the arguments check list
        argindex <- 0
        if(length(argchecks) > 0) {
          for(f in 1:length(argchecks)) {
            if(argchecks[[f]]$arg == args[1,2]) {
              argindex <- f
              break
            }
          }
        }
        if(argindex == 0) newarg <- list(arg = args[1,2], class = args[1,3], nots = list())
        else argchecks[[argindex]]$class <- args[1,3]

        # Process dimensions
        dims <- stringr::str_match(code[i], "dim\\(([^\\)]*)\\)")
        if(!is.na(dims[1,2])) {
          if(argindex != 0) argchecks[[argindex]]$dims <- list()
          dims <- stringr::str_match_all(dims[1,2], "((?>>=|<=|=|>|<))?(\\d+)")
          if(!is.na(dims[[1]][1,3])) {
            for(f in 1:NROW(dims[[1]])) {
              if(is.na(dims[[1]][f,2])) comp <- 0
              else {
                if(dims[[1]][f,2] %in% comp.ops) {
                  comp <- switch(dims[[1]][f,2], ">"=1, ">="=2, "="=3, "<"=4, "<="=5)
                }
                else stop("\n'", dims[[1]][f,2], "' is not a valid comparison operator for the specification of an argument's dimension.\n")
              }
              if(argindex == 0) newarg$dims[[length(newarg$dims)+1]] <- list(comp = comp, value = as.expression(dims[[1]][f,3]))
              else argchecks[[argindex]]$dims[[length(argchecks[[argindex]]$dims)+1]] <- list(comp = comp, value = as.expression(dims[[1]][f,3]))
            }
          }
        }

        # Process excluded values
        nots <- stringr::str_match_all(code[i], "not\\(([^\\)]*)\\)")
        notselems <- c()
        if(length(nots[[1]]) > 0) {
          for(f in 1:NROW(nots[[1]])) {
            notselems <- append(notselems, as.expression(parse(text = sapply(stringr::str_split(nots[[1]][f,2], ","), FUN = stringr::str_trim))))
          }
        }
        if(argindex == 0) newarg$nots <- notselems
        else argchecks[[argindex]]$nots <- append(argchecks[[argindex]]$nots, notselems)

        # Build argument checklist
        if(argindex == 0) argchecks[[length(argchecks)+1]] <- newarg
      }

    }
  }
  return(argchecks)
}



#'@title Type hints - Automatic checks of function arguments
#'
#'
#'@description The \code{check_types()} function is used within the body of a
#'  function to evaluate the parameters of a call to that function against the
#'  requirements defined in the type hint comments. See \emph{Details} section
#'  for more information on type hint comments.
#'
#'@param show.msg Indicates if a message is shown whenever a check fails
#'  (default is \code{TRUE}).
#'@param abort Indicates if checks are stopped after the first error occurred
#'  (default is \code{TRUE}), or if all checks are performed.
#'@param messages A vector with four message templates to be used as error
#'  messages. Templates can make use of predefined placeholders to convey
#'  information important for understanding the source of the problem. See below
#'  for a comprehensive discussion of error messages.
#'@param color Standard hex RGB color code of the error messages (default is
#'  \code{"#bd0245"}).
#'
#'
#'@section How do type hints work? Overview.:
#'
#'  Type hints are special comments with a leading \code{#|} within a function
#'  body indicating the intended nature of the function's arguments in terms of
#'  data types, dimensions and even permitted values. The actual parameters with
#'  which the function is called can be evaluated against these type hint
#'  comments using the \code{check_types()} function.
#'
#'  \code{check_types()} returns \code{FALSE} if any of the checks fails.
#'  Checking can be aborted after the first error occurs, or it can be continued
#'  until all checks have been performed. Optionally, the user is shown a
#'  message indicating the nature of the problem with the function arguments.
#'  The messages can be completely customized using placerholder variables for
#'  all kinds of relevant information.
#'
#'@section Type hint comments:
#'
#'  ## Placement of comments
#'
#'  Type hint comments always need to be placed \emph{inside} a function body
#'  and refer to the arguments of that function. They can be placed
#'  \emph{anywhere} in the function body (even after the call of
#'  \code{check_types()}). Type hint comments are regular R comments but start
#'  with \code{#|} (hash plus pipe, without blanks in between). Each function
#'  argument will have its own type hint comment line. Type hint comments can
#'  cover a subset of all arguments, so there can be arguments without any type
#'  hint restrictions.
#'
#'  ## Comment syntax
#'
#'  Type hint comments consist of a data type check and (optionally) dimension
#'  and value checks: \itemize{
#'
#'  \item \bold{Data type check}: The data type checks for the data type of the
#'  argument. At this point, the data type check needs to be the first check in
#'  a type hint comment and can only comprise one permitted data type. The
#'  syntax is \code{argument_name data_type}. A valid type hint
#'  comment consisting only of a data type check could thus look like this:
#'  \code{#| degrees_celsius numeric}.
#'
#'  \item \bold{Dimension check}: The dimension check checks for the number and
#'  size of the dimensions of the argument. It is constructed using the
#'  \code{dim()} function. \code{dim()} takes one parameter per dimension of the
#'  argument. The parameters specify the size of each of the dimensions of the
#'  argument either as specific values or as comparisons. So, the general syntax
#'  is: \code{dim([comparison_operator]dimsize [,[comparison_operator]#'
#'  dim_size]*)}. For example, if the argument (called \code{unemployment}) is
#'  required to be a dataframe with exactly four columns and at least two rows
#'  then the type hint comment would look like this: \code{#| unemployment
#'  data.frame dim(>=2, 4)}. When \code{check_types()} evaluates the parameters
#'  supplied in the function call it looks for the number of dimensions of the
#'  parameter as well as the size of each dimension.
#'
#'  \item \bold{Value check}: The value check evaluates the actual value of the
#'  parameter supplied in the function call and rejects the value if it is on an
#'  exclude list. Such exclude lists are constructed using the \code{not()}
#'  function. The \code{not()} function expects as its arguments the values that
#'  shall not be permitted as parameter values. These values can include
#'  \code{NA} and \code{NULL}. The general syntax of the  \code{not()} function
#'  is: \code{not(excludevalue[,excludevalue]*)}. If we had an argument called
#'  \code{surname} and this argument must not be \code{NA} or\code{""} (empty
#'  character) then the required type hint check would like this: \code{#|
#'  surname character not("", NA)}. There can be multiple \code{not}-lists in
#'  each type hint comment. If the parameter supplied in the function call
#'  consists of, by its nature, multiple elements, like it is the case with
#'  dataframes, list, and matrices, then the value check fails if \emph{any}
#'  element of the parameter provided in the function call is on the exclude
#'  list.}
#'
#'  When formulating \code{dim} or \code{not} restrictions you can use the
#'  values of other parameters of the function call. So, if you have a function
#'  with two arguments, a number of children (\code{num.children}) and a numeric
#'  vector with the ages of these children (\code{age.children}) you can have a
#'  type hint comment for the latter which looks like this: \code{#|
#'  age.children numeric dim(num.children)}.
#'
#'@section If a type hint check fails:
#'
#'  If any of the checks fails \code{check_types()} returns \code{FALSE},
#'  otherwise it returns \code{TRUE}. If \code{show.msg=TRUE} then a message
#'  will be shown in the R console. The messages can be customized using
#'  templates (see next section). Depending on the value of \code{abort} the
#'  checking of parameters is continued (\code{abort=FALSE}) or stopped
#'  immediately (\code{abort=TRUE}), i.e. no further checks are performed after
#'  the first error.
#'
#'@section Type hint output messages:
#'
#'  ## Message templates
#'
#'  The error messages that are shown (if \code{show.msg=TRUE}) when a check
#'  fails are based on templates. The templates are provided to the
#'  \code{check_types()} function via the \code{messages} argument.
#'  \code{messages} is a character vector with exactly four elements, one for
#'  each possible kind of error message; the four types of error messages are:
#'
#'  \itemize{
#'
#'  \item Wrong parameter type
#'
#'  \item Wrong dimension size of parameter
#'
#'  \item Wrong number of dimensions of parameter
#'
#'  \item Parameter value is not permitted
#'
#'  }
#'
#'  The messages provided via the \code{messages} argument are templates that
#'  can use predefined placeholders to convey information relevant for
#'  understanding the problem.
#'
#'  ## Placeholder that can be used in message templates
#'
#'  \itemize{
#'
#'  \item \emph{#fun}: The name of the function of which the parameter values
#'  are to be checked (i.e. the function \code{check_types()} is applied to)
#'
#'  \item \emph{#arg}: The name of the argument
#'
#'  \item \emph{#argval}: The value of the parameter used in the function call
#'
#'  \item \emph{#type_req}: The required type for the argument
#'
#'  \item \emph{#type_is}: The actual type of the parameter used in the function
#'  call
#'
#'  \item \emph{#dimcnt_req}: The required number of dimensions of the argument
#'
#'  \item \emph{#dimcnt_is}: The actual number of dimensions of the parameter
#'  used in the function call
#'
#'  \item \emph{#dim_req}: The required size of the dimension where a dimension
#'  size error occurred
#'
#'  \item \emph{#dim_is}: The actual size of the dimension where a dimension size
#'  error occurred
#'
#'  \item \emph{#dimcomp}: The comparison operator used in combination with
#'  \code{#dim_req}, the required size of the dimension (e.g. the \code{>=} in
#'  \code{>=2}, if this dimension of the argument is to be greater than 1)
#'
#'  \item \emph{#dimno}: The index of the dimension where a dimension size error
#'  occurred
#'
#'  }
#'
#'@family typehint
#'
#' @examples
#'
#' celsius_to_fahrenheit <- function(degrees_celsius) {
#'  #| degrees_celsius numeric dim(1) not(NA, NULL)
#'
#'  if(check_types()) return(degrees_celsius * 9/5 + 32)
#'  else return(NA)
#'
#' }
#'@export
check_types <- function(show.msg = TRUE, abort = TRUE, messages = c("Problem in function '#fun()': ", "Argument '#arg' (#argval) is of class #type_is but needs to be of class #type_req.", "Size of dimension #dimno of argument '#arg' must be #dimcomp#dim_req, but is actually #dim_is.", "Number of dimensions of argument '#arg' must be #dimcnt_req but is actually #dimcnt_is.", "#argval is not a valid value for argument #arg."), color ="#bd0245") {

  function.call <- match.call(definition = sys.function(sys.nframe()-1),
                  call = sys.calls()[[sys.nframe()-1]])
  function.call <- as.list(function.call)
  code <- utils::capture.output(eval(parse(text = function.call[[1]])))
  code <- code[stringr::str_detect(stringr::str_trim(code), "^#\\|")]

  argchecks <- get_argchecks(code)
  #View(argchecks)
  comp.ops <- c(">", ">=", "=", "<", "<=")

  # Check actual parameters against arguments check list
  error = FALSE
  if(length(function.call) > 1 && length(argchecks) > 0) {
    for(i in 2:length(function.call)) {
      argindex = 0
      for(f in 1:length(argchecks)) {
        if(names(function.call)[[i]] == argchecks[[f]]$arg) {
          argindex = f
          break
        }
      }
      if(argindex != 0) {
        arg.val <- eval(function.call[[i]], envir = sys.nframe()-2)
        type.is <- class(arg.val)

        # Check for correct type
        if(type.is != argchecks[[argindex]]$class) {
          error <- TRUE
          if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 2, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = arg.val, type.req = argchecks[[argindex]]$class, type.is = type.is), as = crayon::make_style(color)))
          if(abort) return(!error)
        }

        # Check for correct number and size of dimensions
        if(!is.null(argchecks[[argindex]]$dims)) {
          d <- dim(arg.val)
          if(is.null(d)) d <- length(arg.val)
          if(NROW(d) == length(argchecks[[argindex]]$dims)) {
            for(f in 1:length(argchecks[[argindex]]$dims)) {
              err_index <- 0
              dim.req <- as.numeric(eval(argchecks[[argindex]]$dims[[f]]$value, envir = sys.nframe()-1))
              if(argchecks[[argindex]]$dims[[f]]$comp == 1 && d[f] <= dim.req) err_index <- 1
              if(argchecks[[argindex]]$dims[[f]]$comp == 2 && d[f] < dim.req) err_index <- 2
              if(argchecks[[argindex]]$dims[[f]]$comp %in% c(0,3) && d[f] != dim.req) err_index <- 3
              if(argchecks[[argindex]]$dims[[f]]$comp == 4 && d[f] >= dim.req) err_index <- 4
              if(argchecks[[argindex]]$dims[[f]]$comp == 5 && d[f] > dim.req) err_index <- 5
              if(err_index != 0) {
                error <- TRUE
                if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 3, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = arg.val, dim.no = f, dim.req = dim.req, dim.is = d[f], dim.comp = comp.ops[err_index]), as = crayon::make_style(color)))
                if(abort) return(!error)
              }
            }
          }
          else {
            error <- TRUE
            if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 4, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = arg.val, dimcnt.req = length(argchecks[[argindex]]$dims), dimcnt.is = NROW(d)), as = crayon::make_style(color)))
            if(abort) return(!error)
          }
        }

        # Check for non-valid values
        if(!is.null(argchecks[[argindex]]$nots)) {
          for(f in 1:NROW(argchecks[[argindex]]$nots)) {
            notval <- eval(argchecks[[argindex]]$nots[f], envir = sys.nframe()-1)

            if(class(arg.val) == "list") {
              if(length(rlist::list.search(arg.val, identical(., eval(notval)))) > 0) {
                error <- TRUE
                if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 5, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = notval), as = crayon::make_style(color)))
                if(abort) break
              }
            }
            else {
              if(is.null(notval)) {
                res <- any(is.null(arg.val))
              }
              else {
                if(is.na(notval)) {
                  res <- any(is.na(arg.val), na.rm = TRUE)
                }
                else {
                  res <- any(arg.val == notval, na.rm = TRUE)
                }
              }
              if(res) {
                error <- TRUE
                if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 5, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = notval), as = crayon::make_style(color)))
                if(abort) break
              }
            }
          }
        }
      }
    }
  }

  return(!error)
}



#'@title Type hints - Automatic checks of function arguments
#'
#'
#'@description Prints out the type hint restrictions for a function in the R
#'  console, based on the type hint comments within the function' body.
#'
#'@param fun The function of which the type hint checks will be shown.
#'@param color Color of the output in standard hex RGB format, default is \code{#bd0245}.
#'
#'@family typehint
#'
#' @examples
#'
#' celsius_to_fahrenheit <- function(degrees_celsius) {
#'  #| degrees_celsius numeric dim(1) not(NA, NULL)
#'
#'  if(check_types()) return(degrees_celsius * 9/5 + 32)
#'  else return(NA)
#'
#' }
#'
#' show_typehints(celsius_to_fahrenheit)
#'
#'@export
show_typehints <- function(fun, color ="#bd0245") {

  code <- utils::capture.output(eval(fun))
  code <- code[stringr::str_detect(stringr::str_trim(code), "^#\\|")]
  argchecks <- get_argchecks(code)

  comp.ops <- c(">", ">=", "=", "<", "<=")
  sep.chars <- crayon::silver(" | ")


  if(length(argchecks) > 0) {
    for(i in 1:length(argchecks)) {
      arg <- argchecks[[i]]
      dims <- c()
      if(length(arg$dims) > 0) {
        for(f in 1:length(arg$dims)) {
          if(arg$dims[[f]]$comp == 0) arg$dims[[f]]$comp <- 3
          dims <- append(dims, paste0(stringr::str_replace(comp.ops[arg$dims[[f]]$comp], "^=$", ""), as.character(arg$dims[[f]]$value)))
        }
        dims <- paste0(dims, collapse = crayon::silver(" x "))
      }
      nots <- c()
      if(length(arg$nots) > 0) nots <- paste0(as.character(arg$nots), collapse = ", ")

      outp <- paste0(crayon::bold("Arg"), ": ", arg$arg, sep.chars,
                     crayon::bold("Type"), ": ", arg$class)

      if(length(dims) > 0) outp <- paste0(outp, sep.chars, crayon::bold("Dimensions"), ": ", dims)
      if(length(nots) > 0) outp <- paste0(outp, sep.chars, crayon::bold("Not valid"), ": ", nots)
      outp <- paste0(crayon::style(outp, as = crayon::make_style(color)), "\n")

    }

    cat(outp)
  }
}
