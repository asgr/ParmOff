.ParmLogApply = function(x, logged, log_fun) {
  if (is.null(logged)) {
    return(x)
  }

  if (is.logical(logged)) {
    x[logged] = lapply(x[logged], log_fun)
  } else if (is.character(logged) && !is.null(names(x))) {
    sel = names(x) %in% logged
    if (any(sel)) {
      x[sel] = lapply(x[sel], log_fun)
    }
  }

  x
}

ParmLog = function(x, logged, log_type = 'log10') {
  log_fun = switch(log_type,
    'log10' = log10,
    'ln'    = log,
    'log2'  = log2,
    stop("log_type must be 'log10', 'ln' or 'log2'")
  )
  .ParmLogApply(x, logged, log_fun)
}

ParmUnLog = function(x, logged, log_type = 'log10') {
  log_fun = switch(log_type,
    'log10' = function(v) 10^v,
    'ln'    = exp,
    'log2'  = function(v) 2^v,
    stop("log_type must be 'log10', 'ln' or 'log2'")
  )
  .ParmLogApply(x, logged, log_fun)
}
