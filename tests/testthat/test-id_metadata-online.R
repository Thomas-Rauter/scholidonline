testthat::test_that(
  "id_metadata (online): DOI returns structured data",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    df <- scholidonline::id_metadata(
      x        = "10.1038/s41586-020-2649-2",
      type     = "doi",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_equal(nrow(df), 1L)

    testthat::expect_true(
      all(
        c(
          "input",
          "type",
          "provider",
          "title",
          "year"
        ) %in% names(df)
      )
    )

    testthat::expect_identical(
      df$input[1],
      "10.1038/s41586-020-2649-2"
    )

    testthat::expect_identical(df$type[1], "doi")

    testthat::expect_true(
      is.na(df$title[1]) || nzchar(df$title[1])
    )
  }
)

testthat::test_that(
  "id_metadata (online): PMID returns structured data",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    df <- scholidonline::id_metadata(
      x        = "31452104",
      type     = "pmid",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_equal(nrow(df), 1L)

    testthat::expect_true(
      all(
        c(
          "input",
          "type",
          "provider"
        ) %in% names(df)
      )
    )

    testthat::expect_identical(df$type[1], "pmid")
  }
)

testthat::test_that(
  "id_metadata (online): vectorized input binds rows",
  {

    testthat::skip_on_cran()
    testthat::skip_if_offline()

    run <- Sys.getenv("SCHOLIDONLINE_RUN_ONLINE_TESTS", "false")
    testthat::skip_if_not(
      identical(tolower(run), "true")
    )

    x <- c(
      "10.1038/s41586-020-2649-2",
      "10.1126/science.169.3946.635"
    )

    df <- scholidonline::id_metadata(
      x        = x,
      type     = "doi",
      provider = "auto"
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_equal(nrow(df), length(x))

    testthat::expect_true(
      all(df$input %in% x)
    )
  }
)
