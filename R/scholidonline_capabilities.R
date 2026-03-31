#' Supported scholidonline capabilities
#'
#' @description
#' Return a summary of the capabilities supported by the scholidonline package.
#'
#' The returned table describes, for each supported identifier type:
#' - which single-identifier operations are available
#'   (`exists`, `links`, `meta`),
#' - which identifier conversions are available,
#' - which providers support each capability, and
#' - which provider is used by default when `provider = "auto"`.
#'
#' This function is useful for discovering what scholidonline can do for a
#' given identifier type or conversion pair.
#'
#' @return A data.frame with one row per supported capability and the
#'   following columns:
#' \itemize{
#'   \item `type`: source identifier type
#'   \item `operation`: operation name (`exists`, `links`, `meta`,
#'     or `convert`)
#'   \item `target`: target identifier type for conversion operations,
#'    otherwise `NA`
#'   \item `providers`: comma-separated names of providers supporting the
#'    capability
#'   \item `default_provider`: default provider used when
#'     `provider = "auto"`
#' }
#'
#' @examples
#' caps <- scholidonline_capabilities()
#'
#' subset(caps, type == "pmid" & operation == "convert")
#'
#' subset(caps, type == "doi" & target == "pmcid")
#'
#' @export
scholidonline_capabilities <- function() {
  reg <- .scholidonline_registry()
  
  rows <- list()
  
  for (type in names(reg)) {
    entry <- reg[[type]]
    
    for (operation in c("exists", "links", "meta")) {
      op_block <- entry[[operation]]
      
      if (is.null(op_block)) {
        next
      }
      
      rows[[length(rows) + 1L]] <- data.frame(
        type = type,
        operation = operation,
        target = NA_character_,
        providers = paste(op_block$providers, collapse = ", "),
        default_provider = op_block$default_provider,
        stringsAsFactors = FALSE
      )
    }
    
    convert_block <- entry$convert
    
    if (is.null(convert_block) || !length(convert_block)) {
      next
    }
    
    for (target in names(convert_block)) {
      conv_block <- convert_block[[target]]
      
      rows[[length(rows) + 1L]] <- data.frame(
        type = type,
        operation = "convert",
        target = target,
        providers = paste(conv_block$providers, collapse = ", "),
        default_provider = conv_block$default_provider,
        stringsAsFactors = FALSE
      )
    }
  }
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}