scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_openalex() uses openalex provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_openalex_openalex = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "W2741809807")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_true(
      .exists_openalex(
        x = "W2741809807",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_openalex() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .exists_openalex(
        x = "W2741809807",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)


testthat::test_that(
  ".links_openalex() dispatches explicit openalex provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .links_openalex_openalex = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "W2741809807")
          data.frame(
            linked_type = "doi",
            linked_value = "10.1000/a",
            provider = "openalex",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )

    out <- .links_openalex(
      x = "W2741809807",
      provider = "openalex",
      quiet = TRUE
    )

    testthat::expect_identical(out$linked_value, "10.1000/a")
  }
)


testthat::test_that(
  ".meta_openalex() uses openalex provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_openalex_openalex = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "W2741809807")
          data.frame(
            title = "Sample",
            year = 2013L,
            container = "Nature",
            doi = "10.1000/a",
            pmid = "123",
            pmcid = "PMC1",
            url = "https://openalex.org/W2741809807",
            provider = "openalex",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )

    out <- .meta_openalex(
      x = "W2741809807",
      provider = "auto",
      quiet = TRUE
    )

    testthat::expect_identical(out$title, "Sample")
  }
)


testthat::test_that(
  ".meta_openalex() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .meta_openalex(
        x = "W2741809807",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)


testthat::test_that(
  ".links_openalex() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .links_openalex(
        x = "W2741809807",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)


testthat::test_that(
  ".convert_openalex_to_doi() dispatches explicit openalex provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .convert_openalex_to_doi_openalex = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "W2741809807")
          "10.1000/a"
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_identical(
      .convert_openalex_to_doi(
        x = "W2741809807",
        from = "openalex",
        to = "doi",
        provider = "openalex",
        quiet = TRUE
      ),
      "10.1000/a"
    )
  }
)


testthat::test_that(
  ".convert_openalex_to_pmid() uses openalex provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .convert_openalex_to_pmid_openalex = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "W2741809807")
          "24136969"
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_identical(
      .convert_openalex_to_pmid(
        x = "W2741809807",
        from = "openalex",
        to = "pmid",
        provider = "auto",
        quiet = TRUE
      ),
      "24136969"
    )
  }
)
