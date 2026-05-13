ParmOff = function(.func, .args=NULL, .use_args=NULL, .rem_args=NULL, .logged = NULL,
                   .lower = NULL, .upper = NULL, .strip = NULL,
                   .quote=TRUE, .envir=parent.frame(), .pass_dots=TRUE, .return='function',
                   ...){
  if(!is.function(.func)){stop('func must be a function!')}

  if(!is.list(.args)){
    .args = as.list(.args)
  }

  input_args = .args

  if(!is.null(.strip)){
    names(.args) = sub(.strip, '', names(.args))
  }

  dots = list(...)
  if(length(dots) > 0){
    if(length(.args) > 0){
      dots = dots[! names(dots) %in% names(.args)]
    }
    .args = c(.args, dots)
  }

  if(!is.null(.lower)){
    shared = intersect(names(.args), names(.lower))
    if(length(shared) > 0){
      .args[shared] = mapply(function(v, lo) max(v, lo), .args[shared], .lower[shared], SIMPLIFY = FALSE)
    }
  }

  if(!is.null(.upper)){
    shared = intersect(names(.args), names(.upper))
    if(length(shared) > 0){
      .args[shared] = mapply(function(v, hi) min(v, hi), .args[shared], .upper[shared], SIMPLIFY = FALSE)
    }
  }

  if(!is.null(.logged)){
    .args[.logged] = lapply(.args[.logged], function(x) 10^x)
  }

  if(length(.args) > 0){
    arg_names = names(.args)

    if(!is.null(.use_args)){
      .args = .args[arg_names %in% .use_args]
      arg_names = names(.args)
    }

    if(!is.null(.rem_args)){
      .args = .args[! arg_names %in% .rem_args]
      arg_names = names(.args)
    }

    .func_formals = names(formals(.func))
    if(!'...' %in% .func_formals || !.pass_dots){
      .args = .args[arg_names %in% .func_formals]
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
