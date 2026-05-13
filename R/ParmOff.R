ParmOff = function(.func, .args = NULL, .use_args = NULL, .rem_args = NULL,
                   .lower = NULL, .upper = NULL, .logged = NULL, .strip = NULL,
                   .quote = TRUE, .envir = parent.frame(), .pass_dots = TRUE,
                   .return = 'function', .check = TRUE, .bound_raw = TRUE, ...){
  if(.check){
    checkmate::assert_function(.func)
    checkmate::assert(
      checkmate::check_null(.args),
      checkmate::check_list(.args),
      checkmate::check_atomic_vector(.args),
      .var.name = '.args'
    )
    checkmate::assert_character(.use_args, null.ok = TRUE)
    checkmate::assert_character(.rem_args, null.ok = TRUE)
    checkmate::assert_character(.logged, null.ok = TRUE)
    checkmate::assert(
      checkmate::check_null(.lower),
      checkmate::check_numeric(.lower, names = 'unique'),
      checkmate::check_list(.lower, names = 'unique'),
      .var.name = '.lower'
    )
    checkmate::assert(
      checkmate::check_null(.upper),
      checkmate::check_numeric(.upper, names = 'unique'),
      checkmate::check_list(.upper, names = 'unique'),
      .var.name = '.upper'
    )
    checkmate::assert_string(.strip, null.ok = TRUE)
    checkmate::assert_flag(.quote)
    checkmate::assert_environment(.envir)
    checkmate::assert_flag(.pass_dots)
    checkmate::assert_choice(.return, c('function', 'args'))
    checkmate::assert_flag(.bound_raw)
  }
  
  if(!is.list(.args)){
    .args = as.list(.args)
  }

  if(.return == 'args'){
    input_args = .args
  }

  if(!is.null(.strip)){
    names(.args) = sub(.strip, '', names(.args))
  }

  if(!missing(...)){
    dots = list(...)
    if(length(dots) > 0){
      if(length(.args) > 0){
        dots = dots[! names(dots) %in% names(.args)]
      }
      .args = c(.args, dots)
    }
  }

  # arg_names computed after dot-merging so it reflects the full argument set
  arg_names = names(.args)
  
  if(.bound_raw){ #apply bounds before we de_log
    if(!is.null(.lower)){
      shared = arg_names[arg_names %in% names(.lower)]
      if(length(shared) > 0){
        .args[shared] = Map(pmax, .args[shared], .lower[shared])
      }
    }
  
    if(!is.null(.upper)){
      shared = arg_names[arg_names %in% names(.upper)]
      if(length(shared) > 0){
        .args[shared] = Map(pmin, .args[shared], .upper[shared])
      }
    }
  }

  if(!is.null(.logged)){
    logged_present = arg_names[arg_names %in% .logged]
    if(length(logged_present) > 0){
      .args[logged_present] = lapply(.args[logged_present], function(x) 10^x)
    }
  }

  if(!.bound_raw){ #apply bounds after we de_log
    if(!is.null(.lower)){
      shared = arg_names[arg_names %in% names(.lower)]
      if(length(shared) > 0){
        .args[shared] = Map(pmax, .args[shared], .lower[shared])
      }
    }
    
    if(!is.null(.upper)){
      shared = arg_names[arg_names %in% names(.upper)]
      if(length(shared) > 0){
        .args[shared] = Map(pmin, .args[shared], .upper[shared])
      }
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
