testthat::test_that(
  "id_metadata (CRAN/offline): returns data.frame with core columns",
  {

    stubs <- new.env(parent = baseenv())

    stubs$metadata_doi <- function(
    x,
    provider = "auto",
    fields   = NULL,
    ...,
    quiet    = FALSE
    ) {
      data.frame(
        provider  = "stub",
        title     = "A Title",
        year      = "2020",
        authors   = "Doe, J.",
        container = "Journal",
        doi       = x,
        pmid      = NA_character_,
        pmcid     = NA_character_,
        url       = paste0("https://doi.org/", x),
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    df <- scholidonline::id_metadata(
      x    = "10.1000/182",
      type = "doi"
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
          "year",
          "authors",
          "container"
        ) %in% names(df)
      )
    )

    testthat::expect_identical(df$input[1], "10.1000/182")
    testthat::expect_identical(df$type[1], "doi")
    testthat::expect_identical(df$provider[1], "stub")
  }
)

testthat::test_that(
  "id_metadata (CRAN/offline): vectorized input binds rows",
  {

    stubs <- new.env(parent = baseenv())

    stubs$metadata_doi <- function(
    x,
    provider = "auto",
    fields   = NULL,
    ...,
    quiet    = FALSE
    ) {
      data.frame(
        provider  = "stub",
        title     = paste("Title", x),
        year      = "2020",
        authors   = "Doe, J.",
        container = "Journal",
        doi       = x,
        pmid      = NA_character_,
        pmcid     = NA_character_,
        url       = paste0("https://doi.org/", x),
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c("10.1000/1", "10.1000/2")

    df <- scholidonline::id_metadata(
      x    = x,
      type = "doi"
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_equal(nrow(df), 2L)

    testthat::expect_true(
      all(df$input %in% x)
    )
  }
)

testthat::test_that(
  "id_metadata (CRAN/offline): type = NULL infers and unknown skipped",
  {

    stubs <- new.env(parent = baseenv())

    stubs$metadata_doi <- function(
    x,
    provider = "auto",
    fields   = NULL,
    ...,
    quiet    = FALSE
    ) {
      data.frame(
        provider  = "stub",
        title     = "A Title",
        year      = "2020",
        authors   = "Doe, J.",
        container = "Journal",
        doi       = x,
        pmid      = NA_character_,
        pmcid     = NA_character_,
        url       = paste0("https://doi.org/", x),
        stringsAsFactors = FALSE
      )
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c(
      "https://doi.org/10.1000/182",
      "not an id"
    )

    df <- scholidonline::id_metadata(
      x    = x,
      type = NULL
    )

    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_true(
      any(df$input == "10.1000/182")
    )
  }
)

testthat::test_that(
  "id_metadata (CRAN/offline): errors if implementation missing",
  {

    testthat::expect_error(
      scholidonline::id_metadata(
        x    = "0317-8471",
        type = "issn"
      ),
      "Missing implementation",
      fixed = FALSE
    )
  }
)
