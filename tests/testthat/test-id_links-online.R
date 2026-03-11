testthat::test_that(
  "id_links (online): DOI yields at least one link or 0 rows",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    df <- scholidonline::id_links(
      x        = "10.1038/s41586-020-2649-2",
      type     = "doi",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")

    testthat::expect_true(
      all(
        c(
          "input",
          "input_type",
          "linked_type",
          "linked_value",
          "provider"
        ) %in% names(df)
      )
    )

    # Accept either: no links found, or at least one returned link.
    testthat::expect_true(nrow(df) >= 0L)

    if (nrow(df) > 0L) {
      testthat::expect_true(
        all(df$input == "10.1038/s41586-020-2649-2")
      )
      testthat::expect_true(
        all(nzchar(df$linked_type))
      )
      testthat::expect_true(
        all(nzchar(df$linked_value))
      )
    }
  }
)

testthat::test_that(
  "id_links (online): PMID yields some links or 0 rows",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    df <- scholidonline::id_links(
      x        = "31452104",
      type     = "pmid",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")

    testthat::expect_true(
      all(
        c(
          "input",
          "input_type",
          "linked_type",
          "linked_value",
          "provider"
        ) %in% names(df)
      )
    )

    testthat::expect_true(nrow(df) >= 0L)

    if (nrow(df) > 0L) {
      testthat::expect_true(
        all(df$input == "31452104")
      )
      testthat::expect_true(
        all(nzchar(df$linked_type))
      )
      testthat::expect_true(
        all(nzchar(df$linked_value))
      )
    }
  }
)

testthat::test_that(
  "id_links (online): ORCID yields links or 0 rows",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    df <- scholidonline::id_links(
      x        = "0000-0002-1825-0097",
      type     = "orcid",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")

    testthat::expect_true(
      all(
        c(
          "input",
          "input_type",
          "linked_type",
          "linked_value",
          "provider"
        ) %in% names(df)
      )
    )

    testthat::expect_true(nrow(df) >= 0L)

    if (nrow(df) > 0L) {
      testthat::expect_true(
        all(df$input == "0000-0002-1825-0097")
      )
      testthat::expect_true(
        all(nzchar(df$linked_type))
      )
      testthat::expect_true(
        all(nzchar(df$linked_value))
      )
    }
  }
)
