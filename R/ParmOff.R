ParmOff = function(.func, .args = NULL, .use_args = NULL, .rem_args = NULL, .logged = NULL,
                   .lower = NULL, .upper = NULL, .strip = NULL, .quote = TRUE,
                   .envir = parent.frame(), .pass_dots = TRUE, .return = 'function',
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
    if(length(.args) > 0){
      dots = dots[! names(dots) %in% names(.args)]
    }
    .args = c(.args, dots)
  }

  # arg_names computed after dot-merging so it reflects the full argument set
  arg_names = names(.args)

  if(!is.null(.lower)){
    shared = intersect(arg_names, names(.lower))
    if(length(shared) > 0){
      .args[shared] = Map(pmax, .args[shared], .lower[shared])
    }
  }

  if(!is.null(.upper)){
    shared = intersect(arg_names, names(.upper))
    if(length(shared) > 0){
      .args[shared] = Map(pmin, .args[shared], .upper[shared])
    }
  }

  if(!is.null(.logged)){
    logged_present = intersect(.logged, arg_names)
    if(length(logged_present) > 0){
      .args[logged_present] = lapply(.args[logged_present], function(x) 10^x)
    }
  }

  if(length(.args) > 0){
    if(!is.null(.use_args)){
      .args = .args[arg_names %in% .use_args]
      arg_names = names(.args)
    }

    if(!is.null(.rem_args)){
      .args = .args[! arg_names %in% .rem_args]
      arg_names = names(.args)
    }

    .func_formals = names(formals(.func, envir=.envir))
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
