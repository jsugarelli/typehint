# z1 <- function(x,y) {
#   #| Ein Kommentar f?r typehint
# 
#   
#   z(1,2)
# }
# 
# 
# z <- function(a,b) {
#   x <- match.call(definition = sys.function(sys.nframe()-1),
#                   call = sys.calls()[[sys.nframe()-1]])
#   
#   x<-as.list(x)
#   code <- capture.output(eval(parse(text = x[[1]])))
#   print(code)
#   return(x)
# }




test <- function(a,b) {
  #| a numeric dim(45,<5) not(NULL, NA) not(15)
  #| b numeric
  #---#| arg1 type3 dim(1,1) not(32)
  z<-check_types(abort = FALSE, color = "#00FF00")
  # cat(z)
}


# ........................ Actual code ........................


check_types <- function(show.msg = TRUE, abort = TRUE, messages = c("Problem in function '#fun()': ", "Argument '#arg' (#argval) is of class #type_is but needs to be of class #type_req."), color ="#bd0245") {
  
  function.call <- match.call(definition = sys.function(sys.nframe()-1),
                  call = sys.calls()[[sys.nframe()-1]])
  function.call <- as.list(function.call)
  code <- capture.output(eval(parse(text = function.call[[1]])))
  code <- code[stringr::str_detect(stringr::str_trim(code), "^#\\|")]
  
  argchecks <- list()
  
  if(NROW(code) > 0) {
    
    comp_ops <- c(">", ">=", "=", "<", "<=")
    
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
                if(dims[[1]][f,2] %in% compops) {
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
            notselems <- append(notselems, as.expression(sapply(stringr::str_split(nots[[1]][f,2], ","), FUN = stringr::str_trim)))
          }
        }
        if(argindex == 0) newarg$nots <- notselems
        else argchecks[[argindex]]$nots <- append(argchecks[[argindex]]$nots, notselems)
        
        # Build argument checklist
        if(argindex == 0) argchecks[[length(argchecks)+1]] <- newarg
      }
      
    }
  }
  
  # Check parameter values against arguments check list
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
        if(class(function.call[[i]]) != argchecks[[argindex]]$class) {
          msg <- stringr::str_replace_all(messages[2], "#type_req", argchecks[[argindex]]$class)
          msg <- stringr::str_replace_all(msg, "#type_is", class(function.call[[i]]))
          msg <- stringr::str_replace_all(msg, "#argval", function.call[[i]])
          msg <- stringr::str_replace_all(msg, "#arg", argchecks[[argindex]]$arg)
          msg <- paste0(stringr::str_replace_all(messages[1], "#fun", as.character(function.call[[1]])), msg,"\n")
          error <- TRUE
          if(show.msg) cat(crayon::style(msg, as = crayon::make_style(color)))
          if(abort) return(error)
        }
        if(!is.null(argchecks[[argindex]]$dims)) {
          if(!(class(function.call[[i]]) %in% c("list"))) d <- dims(function.call[[i]])
          else d <- length(function.call[[i]])
          if(NROW(d) == length(argchecks[[argindex]]$dims)) {
            err_index <- 0
            for(f in 1:length(argchecks[[argindex]]$dims)) {
              if(argchecks[[argindex]]$dims[[f]]$comp == 1 && d[f] <= eval(argchecks[[argindex]]$dims[[f]]$value, envir = parent.frame)) {
                err_index = f  
                break
              }
              if(argchecks[[argindex]]$dims[[f]]$comp == 2 && d[f] < eval(argchecks[[argindex]]$dims[[f]]$value, envir = parent.frame)) {
                err_index = f  
                break
              }
              if(argchecks[[argindex]]$dims[[f]]$comp %in% c(0,3) && d[f] != eval(argchecks[[argindex]]$dims[[f]]$value, envir = parent.frame)) {
                err_index = f  
                break
              }
              if(argchecks[[argindex]]$dims[[f]]$comp == 4 && d[f] >= eval(argchecks[[argindex]]$dims[[f]]$value, envir = parent.frame)) {
                err_index = f  
                break
              }
              if(argchecks[[argindex]]$dims[[f]]$comp == 5 && d[f] > eval(argchecks[[argindex]]$dims[[f]]$value, envir = parent.frame)) {
                err_index = f  
                break
              }
              if(err_index != 0) {
                error <- TRUE
                if(show.msg) cat(crayon::style(msg, as = crayon::make_style(color)))
                if(abort) return(error)                
              }
            }
          }
          else {
            # Falsche Zahl von Dimensionen
          }
        }
        # Hier geht's weiter mit den Checks für Argument i gegen argchecks[[argindex]]
      }
    }
  }
  
  return(error)
}
