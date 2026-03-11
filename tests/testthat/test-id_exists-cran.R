testthat::test_that(
  "id_exists (CRAN/offline): returns logical and handles NA",
  {

    stubs <- new.env(parent = baseenv())

    stubs$exists_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      isTRUE(grepl("^10\\.1000/", x))
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c("10.1000/182", "10.9999/xyz", NA)

    out <- scholidonline::id_exists(
      x    = x,
      type = "doi"
    )

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, length(x))
    testthat::expect_identical(out[1], TRUE)
    testthat::expect_identical(out[2], FALSE)
    testthat::expect_true(is.na(out[3]))
  }
)

testthat::test_that(
  "id_exists (CRAN/offline): infers type and unknown yields NA",
  {

    stubs <- new.env(parent = baseenv())

    stubs$exists_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      isTRUE(grepl("^10\\.1000/", x))
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c(
      "https://doi.org/10.1000/182",
      "not an id"
    )

    out <- scholidonline::id_exists(
      x    = x,
      type = NULL
    )

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 2L)
    testthat::expect_identical(out[1], TRUE)
    testthat::expect_true(is.na(out[2]))
  }
)

testthat::test_that(
  "id_exists (CRAN/offline): errors on invalid type",
  {

    testthat::expect_error(
      scholidonline::id_exists(
        x    = "10.1000/182",
        type = "notatype"
      ),
      "Unknown `type`",
      fixed = FALSE
    )
  }
)

testthat::test_that(
  "id_exists (CRAN/offline): errors if implementation missing",
  {

    testthat::expect_error(
      scholidonline::id_exists(
        x    = "0317-8471",
        type = "issn"
      ),
      "Missing implementation",
      fixed = FALSE
    )
  }
)
