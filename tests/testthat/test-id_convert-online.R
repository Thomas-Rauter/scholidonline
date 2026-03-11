testthat::test_that(
  "id_convert (online): PMID -> DOI returns a DOI-like string or NA",
  {
    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    # A well-known PubMed ID (example). If provider coverage changes,
    # conversion may yield NA; that is still acceptable behavior.
    out <- scholidonline::id_convert(
      x        = "31452104",
      from     = "pmid",
      to       = "doi",
      provider = "auto"
    )

    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)

    # If resolved, it should look like a DOI.
    if (!is.na(out)) {
      testthat::expect_true(
        grepl("^10\\.", out)
      )
    }
  }
)

testthat::test_that(
  "id_convert (online): DOI -> PMID returns digits or NA",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    out <- scholidonline::id_convert(
      x        = "10.1038/s41586-020-2649-2",
      from     = "doi",
      to       = "pmid",
      provider = "auto"
    )

    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)

    if (!is.na(out)) {
      testthat::expect_true(
        grepl("^\\d+$", out)
      )
    }
  }
)

testthat::test_that(
  "id_convert (online): PMCID -> PMID returns digits or NA",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    # This example may or may not exist over time; allow NA.
    out <- scholidonline::id_convert(
      x        = "PMC6184899",
      from     = "pmcid",
      to       = "pmid",
      provider = "auto"
    )

    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)

    if (!is.na(out)) {
      testthat::expect_true(
        grepl("^\\d+$", out)
      )
    }
  }
)
