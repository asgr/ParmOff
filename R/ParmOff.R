ParmOff = function(.func, .args = NULL, .use_args = NULL, .rem_args = NULL,
                   .lower = NULL, .upper = NULL, .logged = NULL, .strip = NULL,
                   .quote = TRUE, .envir = parent.frame(), .pass_dots = TRUE,
                   .return = 'func', .check = TRUE, .bound_raw = TRUE, .log_type = 'log10',
                   .clash = 'first', .verbose = FALSE, ...){
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
    assert(
      check_null(.logged),
      check_character(.logged),
      check_logical(.logged),
      .var.name = '.logged'
    )
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
    assert_choice(.log_type, c('log10', 'ln', 'log2'))
    assert_choice(.clash, c('first', 'last', 'nothing'))
    assert_flag(.verbose)
  }

  if(!is.list(.args)){
    .args = as.list(.args)
  }

  if(.verbose || .return %in% c('args', 'arg', 'func_args', 'func_arg')){
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
      .args = ParmLimLo(.args, as.list(.lower), verbose = .verbose)
    }

    if(!is.null(.upper)){
      .args = ParmLimHi(.args, as.list(.upper), verbose = .verbose)
    }
  }

  if(!is.null(.logged)){ #do any unlogging
    if(is.logical(.logged) && length(.logged) != length(.args)){
      stop('.logged logical vector must be the same length as .args (got ', length(.logged), ' vs ', length(.args), ')')
    }
    .args = ParmUnLog(.args, .logged, log_type = .log_type, verbose = .verbose)
  }

  if(!.bound_raw){ #apply bounds after we de_log
    if(!is.null(.lower)){
      .args = ParmLimLo(.args, as.list(.lower), verbose = .verbose)
    }

    if(!is.null(.upper)){
      .args = ParmLimHi(.args, as.list(.upper), verbose = .verbose)
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
      arg_names = names(.args)
    }

    if(anyDuplicated(arg_names) && .clash != 'nothing'){
      if(.clash == 'first'){
        fromLast = FALSE
      }else if(.clash == 'last'){
        fromLast = TRUE
      }else{
        stop('.clash must be one of first / last / nothing')
      }
      .args = .args[!duplicated(arg_names, fromLast=fromLast)]
      arg_names = names(.args)
    }
  }

  if(.verbose){
    message('Used arguments:\n', paste(arg_names, collapse=' '),
            '\n\nIgnored arguments:\n', paste(names(input_args)[! names(input_args) %in% arg_names], collapse=' '))
  }

  if(.return == 'function' | .return=='func'){
    return(do.call(what=.func, args=.args, quote=.quote, envir=.envir))
  }else if(.return == 'args' | .return=='arg'){
    return(list(current_args = .args, ignore_args = input_args[! names(input_args) %in% arg_names]))
  }else if(.return == 'func_args' | .return=='func_arg'){
    output = list(
      func_out = do.call(what=.func, args=.args, quote=.quote, envir=.envir),
      args_out = list(current_args = .args, ignore_args = input_args[! names(input_args) %in% arg_names])
    )
  }else{
    stop('return must be one of func / args / func_args!')
  }
}
