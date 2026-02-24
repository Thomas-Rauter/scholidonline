testthat::test_that(
  "id_exists (online): DOI exists via registry",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    out <- scholidonline::id_exists(
      x        = "10.1038/s41586-020-2649-2",
      type     = "doi",
      provider = "auto"
    )

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 1L)
    testthat::expect_identical(out, TRUE)
  }
)

testthat::test_that(
  "id_exists (online): DOI non-existence returns FALSE",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    out <- scholidonline::id_exists(
      x        = "10.9999/this-doi-should-not-exist-xyz",
      type     = "doi",
      provider = "doi.org"
    )

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 1L)

    # If the network errors, your implementation might return NA.
    # For a true registry "not found", expect FALSE.
    testthat::expect_true(isFALSE(out) || is.na(out))
  }
)

testthat::test_that(
  "id_exists (online): ORCID exists",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    out <- scholidonline::id_exists(
      x        = "0000-0002-1825-0097",
      type     = "orcid",
      provider = "orcid"
    )

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 1L)

    # This ORCID is commonly used in examples; it should exist.
    testthat::expect_identical(out, TRUE)
  }
)
