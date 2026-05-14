ParmLimLo = function(x, lower){
  if(is.list(x)){
    return(mapply(pmax, x, lower, SIMPLIFY = FALSE))
  }else{
    return(pmax(x, lower))
  }
}

ParmLimHi = function(x, upper){
  if(is.list(x)){
    return(mapply(pmin, x, upper, SIMPLIFY = FALSE))
  }else{
    return(pmin(x, upper))
  }
}

ParmLimBoth = function(x, lower, upper){
  if(is.list(x)){
    return(mapply(function(a, l, u) pmax(pmin(a, u), l),
                  x, lower, upper, SIMPLIFY = FALSE))
  }else{
    return(pmax(pmin(x, upper), lower))
  }
}
