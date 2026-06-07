scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_uniprot() uses uniprot provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_uniprot_uniprot = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "P04637")
          testthat::expect_true(quiet)
          TRUE
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_true(
      .exists_uniprot(
        x = "P04637",
        provider = "auto",
        quiet = TRUE
      )
    )
  }
)


testthat::test_that(
  ".exists_uniprot() errors on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    testthat::expect_error(
      .exists_uniprot(
        x = "P04637",
        provider = "ncbi",
        quiet = TRUE
      ),
      "Unknown provider: ncbi"
    )
  }
)


testthat::test_that(
  ".meta_uniprot() dispatches explicit uniprot provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_uniprot_uniprot = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "P04637")
          data.frame(
            title = "Cellular tumor antigen p53",
            year = 1987L,
            container = "Homo sapiens",
            doi = NA_character_,
            pmid = NA_character_,
            pmcid = NA_character_,
            url = "https://www.uniprot.org/uniprotkb/P04637",
            provider = "uniprot",
            stringsAsFactors = FALSE
          )
        },
        .package = "scholidonline"
      )
    )

    out <- .meta_uniprot(
      x = "P04637",
      provider = "uniprot",
      quiet = TRUE
    )

    testthat::expect_identical(out$title, "Cellular tumor antigen p53")
    testthat::expect_identical(out$container, "Homo sapiens")
  }
)
