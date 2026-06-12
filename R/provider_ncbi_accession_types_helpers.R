# NCBI accession helpers for bioproject, refseq, sra, and assembly


#' Map a BioProject accession to an Entrez database name
#'
#' @param x A single normalized BioProject accession string.
#'
#' @return `"bioproject"`, or `NULL` when unsupported.
#'
#' @noRd
.ncbi_bioproject_entrez_db <- function(x) {
  if (grepl("^(PRJNA|PRJEB|PRJDB|PRJDA|PRJEA)[0-9]{2,}$", x, ignore.case = TRUE)) {
    "bioproject"
  } else {
    NULL
  }
}


#' Map a RefSeq accession to an Entrez database name
#'
#' @param x A single normalized RefSeq accession string.
#'
#' @return `"nuccore"`, `"protein"`, or `NULL` when unsupported.
#'
#' @noRd
.ncbi_refseq_entrez_db <- function(x) {
  prefix <- toupper(substr(x, 1L, regexpr("_", x, fixed = TRUE) - 1L))

  if (prefix %in% c("NP", "XP", "YP", "WP", "AP")) {
    "protein"
  } else if (
    prefix %in% c(
      "AC", "NC", "NG", "NM", "NR", "NT", "NW", "NZ", "XM", "XR"
    )
  ) {
    "nuccore"
  } else {
    NULL
  }
}


#' Map an SRA accession to an Entrez database name
#'
#' @param x A single normalized SRA accession string.
#'
#' @return `"sra"`, or `NULL` when unsupported.
#'
#' @noRd
.ncbi_sra_entrez_db <- function(x) {
  if (grepl("^[SED]R[RXSP][0-9]{5,}$", x, ignore.case = TRUE)) {
    "sra"
  } else {
    NULL
  }
}


#' Map a genome assembly accession to an Entrez database name
#'
#' @param x A single normalized assembly accession string.
#'
#' @return `"assembly"`, or `NULL` when unsupported.
#'
#' @noRd
.ncbi_assembly_entrez_db <- function(x) {
  if (grepl("^GC[AF]_[0-9]{9}\\.[0-9]+$", x, ignore.case = TRUE)) {
    "assembly"
  } else {
    NULL
  }
}


#' Fetch an ESummary response for a typed NCBI accession
#'
#' @param entrez_db_fn Function mapping `x` to an Entrez database name.
#' @param x A single normalized accession string.
#' @param quiet Logical.
#' @param ... Passed to `.scholidonline_esummary_entrez()`.
#'
#' @return Parsed JSON, or `NULL` when routing or the request fails.
#'
#' @noRd
.ncbi_accession_type_fetch_esummary <- function(
    entrez_db_fn,
    x,
    quiet,
    ...
) {
  db <- entrez_db_fn(x)

  if (is.null(db)) {
    return(NULL)
  }

  .ncbi_accession_fetch_esummary(
    db = db,
    x = x,
    ...,
    quiet = quiet
  )
}


#' Extract an organism label from a typed accession ESummary record
#'
#' @param rec A single ESummary record list.
#'
#' @return A single character string.
#'
#' @noRd
.ncbi_accession_organism_from_record <- function(rec) {
  if (is.null(rec)) {
    return(NA_character_)
  }

  organism <- rec$organism

  if (is.list(organism) && !is.null(organism$organismname)) {
    organism <- organism$organismname
  }

  organism <- organism %||%
    rec$speciesname %||%
    rec$taxon %||%
    rec$organism_name %||%
    NA_character_

  if (is.null(organism) || !nzchar(organism)) {
    NA_character_
  } else {
    as.character(organism)
  }
}


#' Extract a year from the first matching record field
#'
#' @param rec A single ESummary record list.
#' @param fields Character vector of candidate field names.
#'
#' @return An integer year, or `NA_integer_`.
#'
#' @noRd
.ncbi_accession_year_from_record_fields <- function(rec, fields) {
  if (is.null(rec)) {
    return(NA_integer_)
  }

  for (field in fields) {
    value <- rec[[field]]

    if (is.null(value) || is.na(value) || !nzchar(as.character(value))) {
      next
    }

    year <- .ncbi_accession_year_from_value(value)

    if (!is.na(year)) {
      return(year)
    }
  }

  NA_integer_
}


#' Extract a title from preferred typed record fields
#'
#' @param rec A single ESummary record list.
#' @param fields Character vector of candidate field names.
#'
#' @return A single character string.
#'
#' @noRd
.ncbi_accession_title_from_record_fields <- function(rec, fields) {
  if (is.null(rec)) {
    return(NA_character_)
  }

  for (field in fields) {
    value <- rec[[field]]

    if (!is.null(value) && nzchar(as.character(value))) {
      return(as.character(value))
    }
  }

  .ncbi_accession_title_from_record(rec)
}


#' Build the canonical BioProject record URL
#'
#' @param x A single normalized BioProject accession string.
#'
#' @return A single URL string.
#'
#' @noRd
.ncbi_bioproject_record_url <- function(x) {
  paste0(
    "https://www.ncbi.nlm.nih.gov/bioproject/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Build the canonical RefSeq record URL
#'
#' @param x A single normalized RefSeq accession string.
#'
#' @return A single URL string.
#'
#' @noRd
.ncbi_refseq_record_url <- function(x) {
  path <- if (identical(.ncbi_refseq_entrez_db(x), "protein")) {
    "protein"
  } else {
    "nuccore"
  }

  paste0(
    "https://www.ncbi.nlm.nih.gov/",
    path,
    "/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Build the canonical SRA record URL
#'
#' @param x A single normalized SRA accession string.
#'
#' @return A single URL string.
#'
#' @noRd
.ncbi_sra_record_url <- function(x) {
  paste0(
    "https://www.ncbi.nlm.nih.gov/sra/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Build the canonical assembly record URL
#'
#' @param x A single normalized assembly accession string.
#'
#' @return A single URL string.
#'
#' @noRd
.ncbi_assembly_record_url <- function(x) {
  paste0(
    "https://www.ncbi.nlm.nih.gov/assembly/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Shared existence implementation for typed NCBI accessions
#'
#' @param x A single normalized accession string.
#' @param entrez_db_fn Function mapping `x` to an Entrez database name.
#' @param ... Passed to `.scholidonline_esummary_entrez()`.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.ncbi_accession_type_exists_ncbi <- function(
    x,
    entrez_db_fn,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  js <- .ncbi_accession_type_fetch_esummary(
    entrez_db_fn = entrez_db_fn,
    x = x,
    ...,
    quiet = quiet
  )

  if (is.null(js) && is.null(entrez_db_fn(x))) {
    return(NA)
  }

  .ncbi_accession_exists_from_esummary(
    js = js,
    x = x
  )
}


#' Shared metadata implementation for typed NCBI accessions
#'
#' @param x A single normalized accession string.
#' @param entrez_db_fn Function mapping `x` to an Entrez database name.
#' @param url_fn Function building the canonical record URL from `x`.
#' @param title_fields Character vector of preferred title fields.
#' @param year_fields Character vector of preferred date fields.
#' @param ... Passed to `.scholidonline_esummary_entrez()`.
#' @param quiet Logical.
#'
#' @return A data.frame containing metadata for the accession.
#'
#' @noRd
.ncbi_accession_type_meta_ncbi <- function(
    x,
    entrez_db_fn,
    url_fn,
    title_fields,
    year_fields,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (is.null(entrez_db_fn(x))) {
    return(data.frame())
  }

  js <- .ncbi_accession_type_fetch_esummary(
    entrez_db_fn = entrez_db_fn,
    x = x,
    ...,
    quiet = quiet
  )

  if (is.null(js)) {
    return(data.frame())
  }

  rec <- .ncbi_accession_record_from_esummary(
    js = js,
    x = x
  )

  if (is.null(rec)) {
    return(data.frame())
  }

  .ncbi_accession_meta_frame(
    title = .ncbi_accession_title_from_record_fields(
      rec = rec,
      fields = title_fields
    ),
    year = .ncbi_accession_year_from_record_fields(
      rec = rec,
      fields = year_fields
    ),
    container = .ncbi_accession_organism_from_record(rec),
    url = url_fn(x)
  )
}
