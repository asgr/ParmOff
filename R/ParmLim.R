ParmLimRecur = function(x, bound, lim_fun){
  if(is.null(bound)){
    return(x)
  }

  if(is.list(x)){
    out = x
    x_names = names(x)
    bound_names = if(is.list(bound)) names(bound) else NULL

    for(i in seq_along(x)){
      child_bound = bound

      if(is.list(bound)){
        if(!is.null(x_names) && !is.null(bound_names) && x_names[i] %in% bound_names){
          child_bound = bound[[x_names[i]]]
        }else if(length(bound) == 1){
          child_bound = bound[[1]]
        }else if(i <= length(bound) && is.null(bound_names)){
          child_bound = bound[[i]]
        }else{
          child_bound = NULL
        }
      }

      out[[i]] = ParmLimRecur(x[[i]], child_bound, lim_fun)
    }

    return(out)
  }

  lim_fun(x, bound)
}

ParmLimLo = function(x, lower){
  ParmLimRecur(x, lower, pmax)
}

ParmLimHi = function(x, upper){
  ParmLimRecur(x, upper, pmin)
}

ParmLimBoth = function(x, lower, upper){
  ParmLimHi(ParmLimLo(x, lower), upper)
}
