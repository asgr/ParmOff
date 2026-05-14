ParmOff = function(.func, .args = NULL, .use_args = NULL, .rem_args = NULL,
                   .lower = NULL, .upper = NULL, .logged = NULL, .strip = NULL,
                   .quote = TRUE, .envir = parent.frame(), .pass_dots = TRUE,
                   .return = 'func', .check = TRUE, .bound_raw = TRUE, ...){
  if(.check){
    assert_function(.func)
    assert(
      check_null(.args),
      check_list(.args),
      check_atomic_vector(.args),
      .var.name = '.args'
    )
    assert_character(.use_args, null.ok = TRUE)
    assert_character(.rem_args, null.ok = TRUE)
    assert_character(.logged, null.ok = TRUE)
    assert(
      check_null(.lower),
      check_numeric(.lower, names = 'unique'),
      check_list(.lower, names = 'unique'),
      .var.name = '.lower'
    )
    assert(
      check_null(.upper),
      check_numeric(.upper, names = 'unique'),
      check_list(.upper, names = 'unique'),
      .var.name = '.upper'
    )
    assert_string(.strip, null.ok = TRUE)
    assert_flag(.quote)
    assert_environment(.envir)
    assert_flag(.pass_dots)
    assert_choice(.return, c('func', 'function', 'args', 'arg', 'func_args', 'func_arg'))
    assert_flag(.bound_raw)
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
        .args[shared] = ParmLimLo(.args[shared], .lower[shared])
      }
    }
  
    if(!is.null(.upper)){
      shared = arg_names[arg_names %in% names(.upper)]
      if(length(shared) > 0){
        .args[shared] = ParmLimHi(.args[shared], .upper[shared])
      }
    }
  }

  
  if(!is.null(.logged)){
    if(is.logical(.logged) && length(.logged) == length(.args)){
      .args[.logged] = lapply(.args[.logged], function(x) 10^x)
    }else if(is.character(.logged) && !is.null(names(.args))){
      logged_present = which(arg_names %in% .logged)
      if(length(logged_present) > 0){
        .args[logged_present] = lapply(.args[logged_present], function(x) 10^x)
      }
    } else {
      warning(".logged must be either a logical vector of length(.args) or a character vector of names")
    }
  }
  
  if(!.bound_raw){ #apply bounds after we de_log
    if(!is.null(.lower)){
      shared = arg_names[arg_names %in% names(.lower)]
      if(length(shared) > 0){
        .args[shared] = ParmLimLo(.args[shared], .lower[shared])
      }
    }
    
    if(!is.null(.upper)){
      shared = arg_names[arg_names %in% names(.upper)]
      if(length(shared) > 0){
        .args[shared] = ParmLimHi(.args[shared], .upper[shared])
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

  if(.return == 'function' | .return=='func'){
    return(do.call(what=.func, args=.args, quote=.quote, envir=.envir))
  }else if(.return == 'args' | .return=='arg'){
    return(list(current_args = .args, ignore_args = input_args[! names(input_args) %in% names(.args)]))
  }else if(.return == 'func_args' | .return=='func_arg'){
    output = list(
      func_out = do.call(what=.func, args=.args, quote=.quote, envir=.envir),
      args_out = list(current_args = .args, ignore_args = input_args[! names(input_args) %in% names(.args)])
    )
  }else{
    stop('return must be one of func / args / func_args!')
  }
}
