ParmOff = function(.func, .args=NULL, .use_args=NULL, .rem_args=NULL, .quote=TRUE,
                   .envir=parent.frame(), .pass_dots=TRUE, ...){
  if(!is.function(.func)){stop('func must be a function!')}

  dots = list(...)
  if(!is.null(dots)){
    if(!is.null(.args)){
      dots = dots[! names(dots) %in% names(.args)]
    }
    .args = c(.args, dots)
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

  return(do.call(what=.func, args=.args, quote=.quote, envir=.envir))
}
