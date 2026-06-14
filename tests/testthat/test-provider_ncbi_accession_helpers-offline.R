esummary_entrez_bindings <- function(result = list(ok = TRUE)) {
  calls <- new.env(parent = emptyenv())
  calls$url <- NULL
  calls$query <- NULL
  calls$quiet <- NULL
  calls$rate_limit_quiet <- NULL

  list(
    bindings = list(
      .scholidonline_req_json = function(url, query, quiet) {
        calls$url <- url
        calls$query <- query
        calls$quiet <- quiet
        result
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        calls$rate_limit_quiet <- quiet
        invisible(NULL)
      },
      .package = "scholidonline"
    ),
    calls = calls
  )
}


testthat::test_that(
  ".scholidonline_esummary_entrez() builds the expected ESummary request",
  {
    mock <- esummary_entrez_bindings()

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .scholidonline_esummary_entrez(
      db = "sra",
      id = "SRR1234567",
      quiet = TRUE
    )

    testthat::expect_identical(out, list(ok = TRUE))
    testthat::expect_identical(
      mock$calls$url,
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
    )
    testthat::expect_identical(
      mock$calls$query,
      list(
        db = "sra",
        id = "SRR1234567",
        retmode = "json"
      )
    )
    testthat::expect_true(mock$calls$quiet)
    testthat::expect_true(mock$calls$rate_limit_quiet)
  }
)


testthat::test_that(
  ".scholidonline_esummary_entrez() forwards extra query parameters",
  {
    mock <- esummary_entrez_bindings()

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    .scholidonline_esummary_entrez(
      db = "bioproject",
      id = c("PRJNA257197", "PRJEB12345"),
      tool = "scholidonline",
      quiet = FALSE
    )

    testthat::expect_identical(
      mock$calls$query$db,
      "bioproject"
    )
    testthat::expect_identical(
      mock$calls$query$id,
      c("PRJNA257197", "PRJEB12345")
    )
    testthat::expect_identical(
      mock$calls$query$tool,
      "scholidonline"
    )
    testthat::expect_false(mock$calls$quiet)
  }
)


testthat::test_that(
  "Entrez ESummary and ESearch helpers validate db and id arguments",
  {
    testthat::expect_error(
      .scholidonline_esummary_entrez(db = character(), id = "SRR1"),
      "`db` must be a single, non-missing character string.",
      fixed = TRUE
    )
    testthat::expect_error(
      .scholidonline_esummary_entrez(db = "sra", id = character()),
      "`id` must be a character vector.",
      fixed = TRUE
    )
    testthat::expect_error(
      .scholidonline_esearch_entrez(db = NA_character_, term = "SRR1"),
      "`db` must be a single, non-missing character string.",
      fixed = TRUE
    )
    testthat::expect_error(
      .scholidonline_esearch_entrez(db = "sra", term = character()),
      "`term` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


esearch_entrez_bindings <- function(result = list(ok = TRUE)) {
  calls <- new.env(parent = emptyenv())
  calls$url <- NULL
  calls$query <- NULL

  list(
    bindings = list(
      .scholidonline_req_json = function(url, query, quiet) {
        calls$url <- url
        calls$query <- query
        result
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      },
      .package = "scholidonline"
    ),
    calls = calls
  )
}


accession_fetch_bindings <- function(
    direct_result = list(result = list(uids = character())),
    search_ids = character(),
    summary_result = NULL
) {
  calls <- new.env(parent = emptyenv())
  calls$esummary <- list()
  calls$esearch <- NULL

  list(
    bindings = list(
      .scholidonline_esummary_entrez = function(db, id, ..., quiet = FALSE) {
        calls$esummary[[length(calls$esummary) + 1L]] <- list(
          db = db,
          id = id,
          quiet = quiet
        )

        if (length(calls$esummary) == 1L) {
          direct_result
        } else {
          summary_result
        }
      },
      .scholidonline_esearch_entrez = function(db, term, ..., quiet = FALSE) {
        calls$esearch <- list(
          db = db,
          term = term,
          quiet = quiet
        )
        list(
          esearchresult = list(
            idlist = search_ids
          )
        )
      },
      .package = "scholidonline"
    ),
    calls = calls
  )
}


testthat::test_that(
  ".scholidonline_esearch_entrez() builds the expected ESearch request",
  {
    mock <- esearch_entrez_bindings()

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .scholidonline_esearch_entrez(
      db = "sra",
      term = "SRR1234567",
      quiet = TRUE
    )

    testthat::expect_identical(out, list(ok = TRUE))
    testthat::expect_identical(
      mock$calls$url,
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
    )
    testthat::expect_identical(
      mock$calls$query,
      list(
        db = "sra",
        term = "SRR1234567",
        retmode = "json"
      )
    )
  }
)


testthat::test_that(
  ".ncbi_accession_esearch_term() adds Entrez field tags when required",
  {
    testthat::expect_identical(
      .ncbi_accession_esearch_term("bioproject", "PRJNA257197"),
      "PRJNA257197"
    )
    testthat::expect_identical(
      .ncbi_accession_esearch_term("gds", "GSE2553"),
      "GSE2553[Accession]"
    )
    testthat::expect_identical(
      .ncbi_accession_esearch_term("sra", "SRR390728"),
      "SRR390728[Accession]"
    )
    testthat::expect_identical(
      .ncbi_accession_esearch_term(
        "assembly",
        "GCA_000001405.28"
      ),
      "GCA_000001405.28[Assembly]"
    )
  }
)


testthat::test_that(
  ".ncbi_accession_record_matches_id() matches database-specific accession fields",
  {
    testthat::expect_true(
      .ncbi_accession_record_matches_id(
        list(project_acc = "PRJNA257197"),
        "PRJNA257197"
      )
    )
    testthat::expect_true(
      .ncbi_accession_record_matches_id(
        list(
          assemblyaccession = "GCF_000001405.39",
          fti_genbank = "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/000/001/405/GCA_000001405.28_GRCh38.p13"
        ),
        "GCA_000001405.28"
      )
    )
    testthat::expect_true(
      .ncbi_accession_record_matches_id(
        list(
          runs = "<Run acc=\"SRR390728\" total_spots=\"1\" />"
        ),
        "SRR390728"
      )
    )
  }
)


testthat::test_that(
  ".ncbi_accession_fetch_esummary() falls back to ESearch for typed accessions",
  {
    summary_result <- list(
      result = list(
        uids = "300298",
        `300298` = list(
          uid = "300298",
          accession = "PRJNA257197",
          project_title = "Example BioProject"
        )
      )
    )
    mock <- accession_fetch_bindings(
      search_ids = "300298",
      summary_result = summary_result
    )

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .ncbi_accession_fetch_esummary(
      db = "bioproject",
      x = "PRJNA257197",
      quiet = TRUE
    )

    testthat::expect_length(mock$calls$esummary, 2L)
    testthat::expect_identical(mock$calls$esummary[[1L]]$id, "PRJNA257197")
    testthat::expect_identical(mock$calls$esearch$db, "bioproject")
    testthat::expect_identical(mock$calls$esearch$term, "PRJNA257197")
    testthat::expect_identical(mock$calls$esummary[[2L]]$id, "300298")
    testthat::expect_identical(
      .ncbi_accession_record_from_esummary(out, "PRJNA257197")$project_title,
      "Example BioProject"
    )
  }
)


testthat::test_that(
  ".ncbi_accession_fetch_esummary() skips ESearch when direct ESummary resolves",
  {
    direct_result <- list(
      result = list(
        uids = "NM_000546.6",
        `NM_000546.6` = list(
          uid = "NM_000546.6",
          accession = "NM_000546.6",
          title = "Example transcript"
        )
      )
    )
    mock <- accession_fetch_bindings(
      direct_result = direct_result,
      summary_result = direct_result
    )

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .ncbi_accession_fetch_esummary(
      db = "nuccore",
      x = "NM_000546.6",
      quiet = TRUE
    )

    testthat::expect_length(mock$calls$esummary, 1L)
    testthat::expect_null(mock$calls$esearch)
    testthat::expect_identical(
      .ncbi_accession_record_from_esummary(out, "NM_000546.6")$title,
      "Example transcript"
    )
  }
)


testthat::test_that(
  ".ncbi_accession_record_from_esummary() resolves direct keyed records",
  {
    js <- list(
      result = list(
        uids = "GSE2553",
        GSE2553 = list(
          uid = "GSE2553",
          accession = "GSE2553",
          title = "Example series"
        )
      )
    )

    rec <- .ncbi_accession_record_from_esummary(js, "GSE2553")

    testthat::expect_identical(rec$title, "Example series")
  }
)


testthat::test_that(
  ".ncbi_accession_record_from_esummary() resolves UID-keyed records",
  {
    js <- list(
      result = list(
        uids = "12345",
        `12345` = list(
          uid = "12345",
          accession = "SRR1234567",
          title = "Example run"
        )
      )
    )

    rec <- .ncbi_accession_record_from_esummary(js, "SRR1234567")

    testthat::expect_identical(rec$title, "Example run")
  }
)


testthat::test_that(
  ".ncbi_accession_record_from_esummary() selects the matching BioProject UID",
  {
    js <- list(
      result = list(
        uids = list("300298", "257197"),
        `300298` = list(
          uid = "300298",
          project_acc = "PRJNA300298"
        ),
        `257197` = list(
          uid = "257197",
          project_acc = "PRJNA257197",
          project_title = "Example BioProject"
        )
      )
    )

    rec <- .ncbi_accession_record_from_esummary(js, "PRJNA257197")

    testthat::expect_identical(rec$project_acc, "PRJNA257197")
    testthat::expect_identical(rec$project_title, "Example BioProject")
  }
)


testthat::test_that(
  ".ncbi_accession_exists_from_esummary() returns TRUE for resolved records",
  {
    js <- list(
      result = list(
        uids = "SRR1234567",
        SRR1234567 = list(
          uid = "SRR1234567",
          accession = "SRR1234567",
          title = "Example run"
        )
      )
    )

    testthat::expect_true(
      .ncbi_accession_exists_from_esummary(js, "SRR1234567")
    )
  }
)


testthat::test_that(
  ".ncbi_accession_exists_from_esummary() returns FALSE for empty uids",
  {
    js <- list(
      result = list(
        uids = character()
      )
    )

    testthat::expect_false(
      .ncbi_accession_exists_from_esummary(js, "SRR1234567")
    )
  }
)


testthat::test_that(
  ".ncbi_accession_exists_from_esummary() returns FALSE for record errors",
  {
    js <- list(
      result = list(
        uids = "SRR1234567",
        SRR1234567 = list(
          uid = "SRR1234567",
          error = "ID not found"
        )
      )
    )

    testthat::expect_false(
      .ncbi_accession_exists_from_esummary(js, "SRR1234567")
    )
  }
)


testthat::test_that(
  ".ncbi_accession_exists_from_esummary() returns NA on missing result block",
  {
    testthat::expect_identical(
      .ncbi_accession_exists_from_esummary(NULL, "SRR1234567"),
      NA
    )
    testthat::expect_identical(
      .ncbi_accession_exists_from_esummary(list(result = NULL), "SRR1234567"),
      NA
    )
  }
)


testthat::test_that(
  ".ncbi_accession_exists_from_esummary() returns FALSE when uids omit the accession",
  {
    js <- list(
      result = list(
        uids = "SRR9999999"
      )
    )

    testthat::expect_false(
      .ncbi_accession_exists_from_esummary(js, "SRR1234567")
    )
  }
)


testthat::test_that(
  ".ncbi_accession_title_from_record() prefers title-like fields",
  {
    testthat::expect_identical(
      .ncbi_accession_title_from_record(
        list(title = "Primary title", caption = "Caption")
      ),
      "Primary title"
    )
    testthat::expect_identical(
      .ncbi_accession_title_from_record(
        list(caption = "Caption only")
      ),
      "Caption only"
    )
    testthat::expect_true(
      is.na(.ncbi_accession_title_from_record(list()))
    )
  }
)


testthat::test_that(
  ".ncbi_accession_year_from_value() extracts a four-digit year",
  {
    testthat::expect_identical(
      .ncbi_accession_year_from_value("2021/03/15"),
      2021L
    )
    testthat::expect_identical(
      .ncbi_accession_year_from_value("2021"),
      2021L
    )
    testthat::expect_true(
      is.na(.ncbi_accession_year_from_value("n/a"))
    )
  }
)


testthat::test_that(
  ".ncbi_accession_meta_frame() returns harmonized accession metadata",
  {
    out <- .ncbi_accession_meta_frame(
      title = "Example accession",
      year = 2020L,
      container = "Homo sapiens",
      url = "https://www.ncbi.nlm.nih.gov/sra/SRR1234567"
    )

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_identical(out$title, "Example accession")
    testthat::expect_identical(out$year, 2020L)
    testthat::expect_identical(out$container, "Homo sapiens")
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/sra/SRR1234567"
    )
    testthat::expect_identical(out$provider, "ncbi")
  }
)
