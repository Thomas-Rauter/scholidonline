testthat::test_that(
  "id_convert (CRAN/offline): converts PMID -> DOI with declared types",
  {

    stubs <- new.env(parent = baseenv())

    stubs$convert_pmid_to_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      paste0("10.1000/", x)
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    out <- scholidonline::id_convert(
      x    = "12345678",
      from = "pmid",
      to   = "doi"
    )

    testthat::expect_type(out, "character")
    testthat::expect_length(out, 1L)
    testthat::expect_identical(out, "10.1000/12345678")
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): from = NULL infers type per element",
  {

    stubs <- new.env(parent = baseenv())

    stubs$convert_pmid_to_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      paste0("10.1000/", x)
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    x <- c(
      "PMID: 12345678",
      "not an id",
      NA
    )

    out <- scholidonline::id_convert(
      x    = x,
      from = NULL,
      to   = "doi"
    )

    testthat::expect_type(out, "character")
    testthat::expect_length(out, length(x))
    testthat::expect_identical(out[1], "10.1000/12345678")
    testthat::expect_true(is.na(out[2]))
    testthat::expect_true(is.na(out[3]))
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): errors on invalid `to` type",
  {

    testthat::expect_error(
      scholidonline::id_convert(
        x    = "12345678",
        from = "pmid",
        to   = "notatype"
      ),
      "Unknown `to`",
      fixed = FALSE
    )
  }
)

testthat::test_that(
  "id_convert (CRAN/offline): errors if implementation missing",
  {

    stubs <- new.env(parent = baseenv())

    stubs$convert_pmid_to_doi <- function(
    x,
    provider = "auto",
    ...,
    quiet = FALSE
    ) {
      paste0("10.1000/", x)
    }

    attach(stubs, name = "scholidonline_stubs")
    on.exit(detach("scholidonline_stubs"), add = TRUE)

    testthat::expect_error(
      scholidonline::id_convert(
        x    = "12345678",
        from = "pmid",
        to   = "pmcid"
      ),
      "Missing implementation",
      fixed = FALSE
    )
  }
)
