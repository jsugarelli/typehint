test <- function(a,b) {
  #| a data.frame dim(2,2) not(NULL, NA, "b") not(15)
  z<-check_types(abort = FALSE, color = "#00FF00")
  # cat(z)
}


# ........................ Actual code ........................


prep_msg <- function(templates, msg.index, fun.name, arg.name, arg.val, type.req = "", type.is = "", dimcnt.req = 0, dimcnt.is = 0, dim.no = 0, dim.req = 0, dim.is = 0, dim.comp = "") {
  msg <- stringr::str_replace_all(templates[msg.index], "#type_req", as.character(type.req))
  msg <- stringr::str_replace_all(msg, "#type_is", as.character(type.is))
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



check_types <- function(show.msg = TRUE, abort = TRUE, messages = c("Problem in function '#fun()': ", "Argument '#arg' (#argval) is of class #type_is but needs to be of class #type_req.", "Size of dimension #dimno of argument '#arg' must be #dimcomp#dim_req, but is actually #dim_is.", "Number of dimensions of argument '#arg' must be #dimcnt_req but is actually #dimcnt_is.", "#argval is not a valid value for argument #arg."), color ="#bd0245") {
  
  function.call <- match.call(definition = sys.function(sys.nframe()-1),
                  call = sys.calls()[[sys.nframe()-1]])
  function.call <- as.list(function.call)
  code <- capture.output(eval(parse(text = function.call[[1]])))
  code <- code[stringr::str_detect(stringr::str_trim(code), "^#\\|")]
  
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
              if(argindex == 0) newarg$dims[[length(newarg$dims)+1]] = list(comp = comp, value = as.expression(dims[[1]][f,3]))
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
        arg.val <- eval(function.call[[i]], envir = sys.nframe()-1)
        type.is <- class(arg.val)
        
        # Check for correct type 
        if(type.is != argchecks[[argindex]]$class) {
          error <- TRUE
          if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 2, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = arg.val, type.req = argchecks[[argindex]]$class, type.is = type.is), as = crayon::make_style(color)))
          if(abort) return(error)
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
                if(abort) return(error)                
              }
            }
          }
          else {
            error <- TRUE
            if(show.msg) cat(crayon::style(prep_msg(templates = messages, msg.index = 4, fun.name = function.call[[1]], arg.name = argchecks[[argindex]]$arg, arg.val = arg.val, dimcnt.req = length(argchecks[[argindex]]$dims), dimcnt.is = NROW(d)), as = crayon::make_style(color)))
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
  
  return(argchecks)
}
