# ROR provider helpers


#' Build a ROR API URL for a normalized identifier
#'
#' @param x A single normalized ROR iD string.
#'
#' @return A URL string.
#'
#' @noRd
.ror_api_url <- function(x) {
  paste0(
    "https://api.ror.org/v2/organizations/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Fetch JSON for a ROR organization record
#'
#' @param x A single normalized ROR iD string.
#' @param quiet Logical.
#' @param silent_404 Logical.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.ror_fetch_organization_json <- function(
    x,
    quiet,
    silent_404 = TRUE
) {
  .scholidonline_http_get_json(
    url = .ror_api_url(x),
    quiet = quiet,
    provider_label = "ROR",
    silent_404 = silent_404
  )
}


#' Extract the preferred display name from a ROR organization record
#'
#' @param obj Parsed ROR organization JSON.
#'
#' @return A single character string.
#'
#' @noRd
.ror_display_name <- function(obj) {
  names <- obj$names

  if (is.null(names) || length(names) == 0L) {
    return(NA_character_)
  }

  for (entry in names) {
    types <- entry$types %||% character()

    if ("ror_display" %in% types) {
      value <- entry$value

      if (!is.null(value) && nzchar(value)) {
        return(as.character(value))
      }
    }
  }

  for (entry in names) {
    types <- entry$types %||% character()

    if ("label" %in% types) {
      value <- entry$value

      if (!is.null(value) && nzchar(value)) {
        return(as.character(value))
      }
    }
  }

  value <- names[[1]]$value

  if (is.null(value) || !nzchar(value)) {
    NA_character_
  } else {
    as.character(value)
  }
}


#' Extract a country name from a ROR organization record
#'
#' @param obj Parsed ROR organization JSON.
#'
#' @return A single character string.
#'
#' @noRd
.ror_country_name <- function(obj) {
  locations <- obj$locations

  if (is.null(locations) || length(locations) == 0L) {
    return(NA_character_)
  }

  for (location in locations) {
    details <- location$geonames_details

    if (
      !is.null(details) &&
        !is.null(details$country_name) &&
        nzchar(details$country_name)
    ) {
      return(as.character(details$country_name))
    }
  }

  NA_character_
}


#' Build a canonical ROR landing-page URL
#'
#' @param x A single normalized ROR iD string.
#' @param obj Parsed ROR organization JSON.
#'
#' @return A single URL string.
#'
#' @noRd
.ror_record_url <- function(x, obj) {
  id <- obj$id %||% NA_character_

  if (!is.na(id) && nzchar(id)) {
    return(as.character(id))
  }

  paste0("https://ror.org/", x)
}
