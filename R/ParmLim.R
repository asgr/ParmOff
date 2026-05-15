# ---------------------------------------------------------------------------
# Internal verbose helpers (shared by ParmLim and ParmLog)
# ---------------------------------------------------------------------------

.is_printable = function(x) {
  if (is.matrix(x)) return(nrow(x) <= 10 && ncol(x) <= 10)
  is.atomic(x) && length(x) <= 20
}

.format_val = function(x) {
  if (is.matrix(x)) {
    rows = apply(format(x), 1, paste, collapse = ' ')
    paste0('[\n    ', paste(rows, collapse = '\n    '), '\n  ]')
  } else if (!is.null(names(x))) {
    paste(names(x), format(x), sep = '=', collapse = ', ')
  } else {
    paste(format(x), collapse = ', ')
  }
}

.ParmLimVerbose = function(before, after, operation) {
  if (is.list(before)) {
    nms = names(before)
    if (is.null(nms)) return(invisible(NULL))
    for (nm in nms) {
      b = before[[nm]]
      a = after[[nm]]
      if (!identical(b, a) && .is_printable(b)) {
        message(operation, " imposed on '", nm, "'\n  before: ", .format_val(b),
                '\n  after:  ', .format_val(a))
      }
    }
  } else {
    if (!identical(before, after) && .is_printable(before)) {
      message(operation, " imposed\n  before: ", .format_val(before),
              '\n  after:  ', .format_val(after))
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------

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


ParmLimLo = function(x, lower, verbose = FALSE) {
  result = .ParmLimRecur(x, lower, pmax)
  if (verbose) .ParmLimVerbose(x, result, 'Lower limit')
  result
}

ParmLimHi = function(x, upper, verbose = FALSE) {
  result = .ParmLimRecur(x, upper, pmin)
  if (verbose) .ParmLimVerbose(x, result, 'Upper limit')
  result
}

ParmLimBoth = function(x, lower, upper, verbose = FALSE) {
  ParmLimHi(ParmLimLo(x, lower, verbose = verbose), upper, verbose = verbose)
}
