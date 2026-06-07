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
