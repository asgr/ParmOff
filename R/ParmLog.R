.ParmLogApply = function(x, logged, log_fun) {
  if (is.null(logged)) {
    return(x)
  }

  if (is.logical(logged)) {
    if (length(logged) != length(x)) {
      stop('Length of logical vector \'logged\' must match length of \'x\' (expected ', length(x), ', got ', length(logged), ')')
    }
    x[logged] = lapply(x[logged], log_fun)
  } else if (is.character(logged) && !is.null(names(x))) {
    sel = names(x) %in% logged
    if (any(sel)) {
      x[sel] = lapply(x[sel], log_fun)
    }
  }
  return(x)
}

.ParmLogVerbose = function(before, after, logged, operation) {
  if (is.logical(logged)) {
    sel_idx = which(logged)
    nms = names(before)
    for (i in sel_idx) {
      nm = if (!is.null(nms)) nms[i] else paste0('[', i, ']')
      bef_sel = before[[i]]
      if (.is_printable(bef_sel)) {
        message(operation, " applied to '", nm, "'\n  before: ", .format_val(bef_sel),
                '\n  after:  ', .format_val(after[[i]]))
      }
    }
  } else if (is.character(logged) && !is.null(names(before))) {
    sel = names(before)[names(before) %in% logged]
    for (nm in sel) {
      bef_sel = before[[nm]]
      if (.is_printable(bef_sel)) {
        message(operation, " applied to '", nm, "'\n  before: ", .format_val(bef_sel),
                '\n  after:  ', .format_val(after[[nm]]))
      }
    }
  }
  invisible(NULL)
}

.invert_log_type = function(log_type){
  switch(log_type,
         'log10' = '10^x',
         'ln'    = 'exp(x)',
         'log2'  = '2^x'
  )
}

ParmLog = function(x, logged, log_type = 'log10', verbose = FALSE) {
  log_fun = switch(log_type,
    'log10' = log10,
    'ln'    = log,
    'log2'  = log2,
    stop("log_type must be 'log10', 'ln' or 'log2'")
  )
  result = .ParmLogApply(x, logged, log_fun)
  if (verbose) .ParmLogVerbose(x, result, logged, paste0('ParmLog (', log_type, ')'))
  result
}

ParmUnLog = function(x, logged, log_type = 'log10', verbose = FALSE) {
  log_fun = switch(log_type,
    'log10' = function(v) 10^v,
    'ln'    = exp,
    'log2'  = function(v) 2^v,
    stop("log_type must be 'log10', 'ln' or 'log2'")
  )
  result = .ParmLogApply(x, logged, log_fun)
  if (verbose) .ParmLogVerbose(x, result, logged, paste0('ParmUnLog (', .invert_log_type(log_type), ')'))
  result
}
