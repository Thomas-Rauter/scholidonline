#' Retrieve scholarly metadata
#'
#' @description
#' Retrieve structured metadata for scholarly identifiers from external
#' registries.
#'
#' @details
#' `id_metadata()` is vectorized over `x` and returns a data.frame with one row
#' per input identifier.
#' 
#' For providers that support batch lookup, such as arXiv, multiple identifiers
#' may be resolved using a single provider request. This does not change the
#' public return shape: the output still contains one row per input identifier.
#'
#' The function returns a harmonized cross-provider data.frame with columns
#' `title`, `year`, `container`, `doi`, `pmid`, `pmcid`, and `url`. For
#' bibliographic identifiers, `container` is typically a journal or source
#' title and linked DOI/PMID/PMCID fields may be populated. For other types,
#' the same columns are reused with type-appropriate meaning (for example,
#' protein name and organism for UniProt, organization name and country for
#' ROR, or accession title and organism for NCBI accessions). Bibliographic
#' link columns are `NA` when not applicable.
#'
#' For NCBI accession types such as BioProject, `title` is the registry's
#' short project or record title from Entrez ESummary, not the full description
#' shown on the NCBI website. Use `url` for the complete record.
#'
#' `type` must be a single value or `"auto"`. For mixed identifier columns,
#' omit `type` or use `type = "auto"` so each element is classified
#' separately.
#'
#' @param x A character vector of identifiers.
#' @param type A single identifier type string, or `"auto"` to infer the type
#'   for each element of `x`. See `scholidonline_types()` for supported values.
#' @param provider A single provider string specifying which online service to
#'   use. Use `"auto"` to use the default provider for the resolved identifier
#'   type. In most cases, `"auto"` is appropriate.
#' @param fields An optional character vector naming the columns to return. If
#'   `NULL`, all default columns are returned. Unknown field names are ignored.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings and messages where possible.
#'
#' @return A data.frame with one row per input identifier. By default, the
#'   returned columns are `input`, `type`, `provider`, `title`, `year`,
#'   `container`, `doi`, `pmid`, `pmcid`, and `url`. Inputs that cannot be
#'   identified, normalized, or resolved are returned as rows with missing
#'   metadata fields.
#'
#' @examples
#' \donttest{
#'   out <- id_metadata("10.1038/nature12373", type = "doi")
#'   knitr::kable(out)
#'   out <- id_metadata(c("31452104", "PMC6821181"))
#'   knitr::kable(out)
#'   out <- id_metadata(
#'     "10.1038/nature12373",
#'      fields = c("title", "year", "doi")
#'      )
#'   knitr::kable(out)
#' }
#'
#' @export
id_metadata <- function(
    x,
    type = c("auto", scholidonline_types()),
    provider = c("auto", .scholidonline_providers()),
    fields = NULL,
    ...,
    quiet = FALSE
){
  .scholidonline_check_x(x)
  type <- match.arg(type)
  provider <- match.arg(provider)
  .scholidonline_check_type_provider(
    type = type,
    provider = provider
  )
  .scholidonline_check_quiet(quiet)
  
  prepared <- .scholidonline_prepare_inputs(
    x = x,
    type = type
  )

  base_df <- data.frame(
    input = x,
    type = prepared$type_vec,
    provider = NA_character_,
    title = NA_character_,
    year = NA_integer_,
    container = NA_character_,
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!any(prepared$ok)) {
    return(base_df)
  }

  x_ok <- prepared$x_norm[prepared$ok]
  type_ok <- prepared$type_vec[prepared$ok]
  
  res <- .scholidonline_run_unary(
    x = x_ok,
    operation = "meta",
    type = type_ok,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  for (j in seq_along(res)) {
    df <- res[[j]]

    if (is.null(df) || nrow(df) == 0L) {
      next
    }

    idx <- prepared$ok_idx[j]
    base_df$provider[idx] <- df$provider[1]
    base_df$title[idx] <- df$title[1]
    base_df$year[idx] <- df$year[1]
    base_df$container[idx] <- df$container[1]
    base_df$doi[idx] <- df$doi[1]
    base_df$pmid[idx] <- df$pmid[1]
    base_df$pmcid[idx] <- df$pmcid[1]
    base_df$url[idx] <- df$url[1]
  }
  
  if (!is.null(fields)) {
    keep <- fields[fields %in% names(base_df)]
    base_df <- base_df[, keep, drop = FALSE]
  }
  
  base_df
}