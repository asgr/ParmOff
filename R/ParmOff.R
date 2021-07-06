ParmOff = function(.func, .args=NULL, .use_args=NULL, .rem_args=NULL, .logged = NULL, .strip = NULL,
                   .quote=TRUE, .envir=parent.frame(), .pass_dots=TRUE, .return='function',
                   ...){
  if(!is.function(.func)){stop('func must be a function!')}

  if(!is.list(.args)){
    .args = as.list(.args)
  }

  if(.return == 'args'){
    input_args = .args
  }

  if(!is.null(.strip)){
    names(.args) = sub(.strip, '', names(.args))
  }

  dots = list(...)
  if(length(dots) > 0){
    if(!is.null(.args)){
      dots = dots[! names(dots) %in% names(.args)]
    }
    .args = c(.args, dots)
  }

  if(!is.null(.logged)){
    temp_logged = .args[.logged]
    if(length(.args[.logged]) > 0){
      .args[.logged] = relist(10^unlist(temp_logged), temp_logged)
    }
  }

  if(!is.null(.use_args) & length(.args) > 1){
    .args = .args[names(.args) %in% .use_args]
  }

  if(!is.null(.rem_args) & length(.args) > 1){
    .args = .args[! names(.args) %in% .rem_args]
  }

  if(length(.args) > 1){
    .func_formals = names(formals(.func))
    if((! '...' %in% .func_formals) | .pass_dots == FALSE){
      .args = .args[names(.args) %in% .func_formals]
    }
  }

  if(.return == 'function'){
    return(do.call(what=.func, args=.args, quote=.quote, envir=.envir))
  }else if(.return == 'args'){
    return(list(current_args = .args, ignore_args = input_args[! names(input_args) %in% names(.args)]))
  }else{
    stop('return must be one of function / args!')
  }
}
