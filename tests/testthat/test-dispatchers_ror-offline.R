scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_ror() uses ror provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_ror_ror = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "01an7q238")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_true(
      .exists_ror(
        x = "01an7q238",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_ror() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .exists_ror(
        x = "01an7q238",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)


testthat::test_that(
  ".meta_ror() dispatches explicit ror provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_ror_ror = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "01an7q238")
          data.frame(
            title = "University of Bern",
            year = NA_integer_,
            container = "Switzerland",
            doi = NA_character_,
            pmid = NA_character_,
            pmcid = NA_character_,
            url = "https://ror.org/01an7q238",
            provider = "ror",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )

    out <- .meta_ror(
      x = "01an7q238",
      provider = "ror",
      quiet = TRUE
    )

    testthat::expect_identical(out$title, "University of Bern")
    testthat::expect_identical(out$container, "Switzerland")
  }
)


testthat::test_that(
  ".meta_ror() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .meta_ror(
        x = "01an7q238",
        provider = "crossref",
        quiet = TRUE
      ),
      "Unknown provider: crossref"
    )
  }
)
