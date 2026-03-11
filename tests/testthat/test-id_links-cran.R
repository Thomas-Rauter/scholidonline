testthat::test_that(
  "id_links (CRAN/offline): returns data.frame with required columns",
  {

    stubs <- new.env(parent = baseenv())

    stubs$links_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      data.frame(
        linked_type  = "url",
        linked_value = paste0("https://doi.org/", x),
        provider     = "stub",
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    df <- scholidonline::id_links(
      x    = "10.1000/182",
      type = "doi"
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

    testthat::expect_equal(nrow(df), 1L)
    testthat::expect_identical(df$input[1], "10.1000/182")
    testthat::expect_identical(df$input_type[1], "doi")
    testthat::expect_identical(df$linked_type[1], "url")
    testthat::expect_identical(df$provider[1], "stub")
  }
)

testthat::test_that(
  "id_links (CRAN/offline): type = NULL infers and unknown yields 0 rows",
  {

    stubs <- new.env(parent = baseenv())

    stubs$links_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      data.frame(
        linked_type  = "url",
        linked_value = paste0("https://doi.org/", x),
        provider     = "stub",
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c(
      "https://doi.org/10.1000/182",
      "not an id",
      NA
    )

    df <- scholidonline::id_links(
      x    = x,
      type = NULL
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

    testthat::expect_equal(
      sum(df$input == "10.1000/182", na.rm = TRUE),
      1L
    )
  }
)

testthat::test_that(
  "id_links (CRAN/offline): vectorized input yields multiple rows",
  {

    stubs <- new.env(parent = baseenv())

    stubs$links_pmid <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      data.frame(
        linked_type  = c("url", "doi"),
        linked_value = c(
          paste0("https://pubmed.ncbi.nlm.nih.gov/", x, "/"),
          paste0("10.1000/", x)
        ),
        provider = "stub",
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    df <- scholidonline::id_links(
      x    = c("12345678", "87654321"),
      type = "pmid"
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_equal(nrow(df), 4L)

    testthat::expect_true(
      all(df$input %in% c("12345678", "87654321"))
    )
  }
)

testthat::test_that(
  "id_links (CRAN/offline): errors if implementation missing",
  {

    testthat::expect_error(
      scholidonline::id_links(
        x    = "0317-8471",
        type = "issn"
      ),
      "Missing implementation",
      fixed = FALSE
    )
  }
)
