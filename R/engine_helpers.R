# Level 2 function (functions called by level 1 functions) definitions ---------


.scholidonline_get_dispatcher <- function(
    name
) {
  
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    rlang::abort(
      message = "`name` must be a single, non-missing character string."
    )
  }
  
  fun <- get0(
    x = name,
    mode = "function",
    inherits = TRUE
  )
  
  if (is.null(fun)) {
    rlang::abort(
      message = paste0(
        "Missing implementation: ",
        name,
        "()."
      )
    )
  }
  
  fun
}


# Level 3 function (functions called by level 2 functions) definitions ---------


.scholidonline_as_logical_scalar <- function(
    x
) {
  
  if (!is.logical(x) || length(x) != 1L) {
    rlang::abort(
      message = "Provider implementation must return a single logical value."
    )
  }
  
  x
}


.scholidonline_as_character_scalar <- function(
    x
) {
  
  if (!is.character(x) || length(x) != 1L) {
    rlang::abort(
      message = "Provider implementation must return a single character value."
    )
  }
  
  x
}