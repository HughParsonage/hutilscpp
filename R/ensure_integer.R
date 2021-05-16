

ensure_integer <- function(x, on_failure = c("error", "warn")) {
  if (is.integer(x)) {
    return(x)
  }
  if (is.double(x)) {
    # 0 if all ok, otherwise position
    int_status <- .Call("Cwhich_isnt_int", x, PACKAGE = packageName())
    if (int_status) {
      switch(on_failure[1],
             error = stop("`", vname(x), "` was not integerish at position ", 
                          int_status, "."),
             warn = warning("`", vname(x), "` was not integerish at position ", 
                            int_status, "."))
      
    }
    return(as.integer(x))
  }
  stop("`", vname(x), "` was type ", typeof(x), ", but must be integer.")
}


