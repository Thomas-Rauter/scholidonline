#' NCBI: check whether a BioProject accession exists
#'
#' @param x A single, normalized BioProject accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_bioproject_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_exists_ncbi(
    x = x,
    entrez_db_fn = .ncbi_bioproject_entrez_db,
    ...,
    quiet = quiet
  )
}


#' NCBI: retrieve metadata for a BioProject accession
#'
#' @param x A single, normalized BioProject accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A data.frame containing metadata for the BioProject accession.
#'
#' @noRd
.meta_bioproject_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_meta_ncbi(
    x = x,
    entrez_db_fn = .ncbi_bioproject_entrez_db,
    url_fn = .ncbi_bioproject_record_url,
    title_fields = c(
      "project_title",
      "projecttitle",
      "title"
    ),
    year_fields = c(
      "registrationdate",
      "registration_date",
      "date"
    ),
    ...,
    quiet = quiet
  )
}


#' NCBI: check whether a RefSeq accession exists
#'
#' @param x A single, normalized RefSeq accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_refseq_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_exists_ncbi(
    x = x,
    entrez_db_fn = .ncbi_refseq_entrez_db,
    ...,
    quiet = quiet
  )
}


#' NCBI: retrieve metadata for a RefSeq accession
#'
#' @param x A single, normalized RefSeq accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A data.frame containing metadata for the RefSeq accession.
#'
#' @noRd
.meta_refseq_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_meta_ncbi(
    x = x,
    entrez_db_fn = .ncbi_refseq_entrez_db,
    url_fn = .ncbi_refseq_record_url,
    title_fields = c("title", "caption"),
    year_fields = c("createdate", "updatedate", "date"),
    ...,
    quiet = quiet
  )
}


#' NCBI: check whether an SRA accession exists
#'
#' @param x A single, normalized SRA accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_sra_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_exists_ncbi(
    x = x,
    entrez_db_fn = .ncbi_sra_entrez_db,
    ...,
    quiet = quiet
  )
}


#' NCBI: retrieve metadata for an SRA accession
#'
#' @param x A single, normalized SRA accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A data.frame containing metadata for the SRA accession.
#'
#' @noRd
.meta_sra_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (is.null(.ncbi_sra_entrez_db(x))) {
    return(data.frame())
  }

  js <- .ncbi_accession_type_fetch_esummary(
    entrez_db_fn = .ncbi_sra_entrez_db,
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

  title <- .ncbi_accession_title_from_record_fields(
    rec = rec,
    fields = c("title", "caption")
  )

  if (is.na(title) && !is.null(rec$expxml) && nzchar(rec$expxml)) {
    title <- sub(
      ".*<Title>([^<]+)</Title>.*",
      "\\1",
      rec$expxml,
      perl = TRUE
    )

    if (identical(title, rec$expxml)) {
      title <- NA_character_
    }
  }

  .ncbi_accession_meta_frame(
    title = title,
    year = .ncbi_accession_year_from_record_fields(
      rec = rec,
      fields = c("publicationdate", "createdate", "loaddate")
    ),
    container = .ncbi_accession_organism_from_record(rec),
    url = .ncbi_sra_record_url(x)
  )
}


#' NCBI: check whether a genome assembly accession exists
#'
#' @param x A single, normalized assembly accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_assembly_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_exists_ncbi(
    x = x,
    entrez_db_fn = .ncbi_assembly_entrez_db,
    ...,
    quiet = quiet
  )
}


#' NCBI: retrieve metadata for a genome assembly accession
#'
#' @param x A single, normalized assembly accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A data.frame containing metadata for the assembly accession.
#'
#' @noRd
.meta_assembly_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .ncbi_accession_type_meta_ncbi(
    x = x,
    entrez_db_fn = .ncbi_assembly_entrez_db,
    url_fn = .ncbi_assembly_record_url,
    title_fields = c("assemblyname", "asmname", "title"),
    year_fields = c("submissiondate", "asmdate", "sequencedate"),
    ...,
    quiet = quiet
  )
}
