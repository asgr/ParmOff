.ParmLimRecur = function(x, bound, lim_fun) {
  
  # Nothing to do
  if (is.null(bound)) return(x)
  
  # --- Case 1: x is a list → recurse ---
  if (is.list(x)) {
    
    x_names <- names(x)
    b_names <- names(bound)
    
    
    out <- lapply(seq_along(x), function(i) {
      
      # Determine child bound
      child_bound <- NULL
      
      if (is.list(bound)) {
        
        if (!is.null(b_names) && !is.null(x_names) && x_names[i] %in% b_names) {
          child_bound <- bound[[x_names[i]]]
          
        } else if (is.null(b_names)) {
          if (length(bound) == 1) {
            child_bound <- bound[[1]]
          } else if (i <= length(bound)) {
            child_bound <- bound[[i]]
          }
        }
        
      } else {
        child_bound <- bound
      }
      
      .ParmLimRecur(x[[i]], child_bound, lim_fun)
    })
    
    names(out) <- x_names
    return(out)
  }
  
  # --- Case 2: x is atomic ---
  
  # Reject list bounds at leaf level
  if (is.list(bound)) return(x)
  
  # If bound is named → align by name
  if (!is.null(names(bound)) && !is.null(names(x))) {
    
    b <- bound[names(x)]
    
    # Only apply where bound exists
    idx <- !is.na(b)
    
    out <- x
    out[idx] <- lim_fun(x[idx], b[idx])
    
    return(out)
  }
  
  # Scalar or unnamed vector → direct apply
  lim_fun(x, bound)
}


ParmLimLo = function(x, lower){
  .ParmLimRecur(x, lower, pmax)
}

ParmLimHi = function(x, upper){
  .ParmLimRecur(x, upper, pmin)
}

ParmLimBoth = function(x, lower, upper){
  ParmLimHi(ParmLimLo(x, lower), upper)
}
