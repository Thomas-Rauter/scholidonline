empty_df <- function() {
  data.frame(stringsAsFactors = FALSE)
}


scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


geo_esummary_bindings <- function(result = NULL) {
  calls <- new.env(parent = emptyenv())
  calls$db <- NULL
  calls$id <- NULL
  calls$quiet <- NULL

  list(
    bindings = list(
      .scholidonline_esummary_entrez = function(
          db,
          id,
          ...,
          quiet = FALSE
      ) {
        calls$db <- db
        calls$id <- id
        calls$quiet <- quiet
        result
      },
      .package = "scholidonline"
    ),
    calls = calls
  )
}


geo_gse_record_json <- function() {
  list(
    result = list(
      uids = "GSE2553",
      GSE2553 = list(
        uid = "GSE2553",
        accession = "GSE2553",
        title = "Example series",
        taxon = "Homo sapiens",
        pdat = "2005/01/01"
      )
    )
  )
}


geo_gds_record_json <- function() {
  list(
    result = list(
      uids = "GDS505",
      GDS505 = list(
        uid = "GDS505",
        accession = "GDS505",
        title = "Example dataset",
        taxon = "Mus musculus",
        pdat = "2004/06/15"
      )
    )
  )
}


testthat::test_that(
  ".ncbi_geo_entrez_db() routes GEO prefixes to the expected databases",
  {
    testthat::expect_identical(.ncbi_geo_entrez_db("GSE2553"), "geo")
    testthat::expect_identical(.ncbi_geo_entrez_db("GSM313800"), "geo")
    testthat::expect_identical(.ncbi_geo_entrez_db("GPL96"), "geo")
    testthat::expect_identical(.ncbi_geo_entrez_db("GDS505"), "gds")
    testthat::expect_null(.ncbi_geo_entrez_db("XXX123"))
  }
)


testthat::test_that(
  ".ncbi_geo_fetch_esummary() uses geo for series accessions",
  {
    mock <- geo_esummary_bindings(result = geo_gse_record_json())

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .ncbi_geo_fetch_esummary("GSE2553", quiet = TRUE)

    testthat::expect_identical(mock$calls$db, "geo")
    testthat::expect_identical(mock$calls$id, "GSE2553")
    testthat::expect_identical(out, geo_gse_record_json())
  }
)


testthat::test_that(
  ".ncbi_geo_fetch_esummary() uses gds for dataset accessions",
  {
    mock <- geo_esummary_bindings(result = geo_gds_record_json())

    do.call(
      testthat::local_mocked_bindings,
      mock$bindings
    )

    out <- .ncbi_geo_fetch_esummary("GDS505", quiet = TRUE)

    testthat::expect_identical(mock$calls$db, "gds")
    testthat::expect_identical(mock$calls$id, "GDS505")
    testthat::expect_identical(out, geo_gds_record_json())
  }
)


testthat::test_that(
  ".exists_geo_ncbi() returns TRUE for resolved GEO records",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      geo_esummary_bindings(result = geo_gse_record_json())$bindings
    )

    testthat::expect_true(
      .exists_geo_ncbi("GSE2553", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_geo_ncbi() returns FALSE for empty uids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      geo_esummary_bindings(
        result = list(result = list(uids = character()))
      )$bindings
    )

    testthat::expect_false(
      .exists_geo_ncbi("GSE2553", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_geo_ncbi() returns NA for unsupported GEO prefixes",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .exists_geo_ncbi("XXX123", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_geo_ncbi() returns NA on NULL ESummary response",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      geo_esummary_bindings(result = NULL)$bindings
    )

    testthat::expect_identical(
      .exists_geo_ncbi("GSE2553", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".meta_geo_ncbi() returns harmonized metadata for a series accession",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      geo_esummary_bindings(result = geo_gse_record_json())$bindings
    )

    out <- .meta_geo_ncbi("GSE2553", quiet = TRUE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_identical(out$title, "Example series")
    testthat::expect_identical(out$year, 2005L)
    testthat::expect_identical(out$container, "Homo sapiens")
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2553"
    )
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_geo_ncbi() returns harmonized metadata for a dataset accession",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      geo_esummary_bindings(result = geo_gds_record_json())$bindings
    )

    out <- .meta_geo_ncbi("GDS505", quiet = TRUE)

    testthat::expect_identical(out$title, "Example dataset")
    testthat::expect_identical(out$year, 2004L)
    testthat::expect_identical(out$container, "Mus musculus")
  }
)


testthat::test_that(
  ".meta_geo_ncbi() returns empty data.frame for unsupported prefixes",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_identical(
      .meta_geo_ncbi("XXX123", quiet = TRUE),
      empty_df()
    )
  }
)
