scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_geo() uses ncbi provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_geo_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "GSE2553")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_true(
      .exists_geo(
        x = "GSE2553",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_geo() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .exists_geo(
        x = "GSE2553",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)


testthat::test_that(
  ".meta_geo() dispatches explicit ncbi provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_geo_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "GDS505")
          data.frame(
            title = "Example dataset",
            year = 2004L,
            container = "Mus musculus",
            doi = NA_character_,
            pmid = NA_character_,
            pmcid = NA_character_,
            url = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GDS505",
            provider = "ncbi",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )

    out <- .meta_geo(
      x = "GDS505",
      provider = "ncbi",
      quiet = TRUE
    )

    testthat::expect_identical(out$title, "Example dataset")
    testthat::expect_identical(out$container, "Mus musculus")
  }
)


testthat::test_that(
  ".meta_geo() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .meta_geo(
        x = "GDS505",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)
