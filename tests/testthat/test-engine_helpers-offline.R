testthat::test_that(
  ".scholidonline_get_dispatcher() returns dispatcher function",
  {
    out <- scholidonline:::.scholidonline_get_dispatcher(
      name = ".scholidonline_get_dispatcher"
    )
    
    testthat::expect_true(is.function(out))
    testthat::expect_identical(
      out,
      scholidonline:::.scholidonline_get_dispatcher
    )
  }
)


testthat::test_that(
  ".scholidonline_get_dispatcher() errors for non-character input",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_get_dispatcher(
        name = 1
      ),
      "`name` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_get_dispatcher() errors for character vector length 0",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_get_dispatcher(
        name = character()
      ),
      "`name` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_get_dispatcher() errors for character vector length > 1",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_get_dispatcher(
        name = c("a", "b")
      ),
      "`name` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_get_dispatcher() errors for NA_character_",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_get_dispatcher(
        name = NA_character_
      ),
      "`name` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_get_dispatcher() errors for missing implementation",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_get_dispatcher(
        name = ".scholidonline_not_a_real_dispatcher"
      ),
      "Missing implementation: .scholidonline_not_a_real_dispatcher().",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_as_logical_scalar() returns TRUE unchanged",
  {
    out <- scholidonline:::.scholidonline_as_logical_scalar(
      x = TRUE
    )
    
    testthat::expect_identical(out, TRUE)
  }
)


testthat::test_that(
  ".scholidonline_as_logical_scalar() returns FALSE unchanged",
  {
    out <- scholidonline:::.scholidonline_as_logical_scalar(
      x = FALSE
    )
    
    testthat::expect_identical(out, FALSE)
  }
)


testthat::test_that(
  ".scholidonline_as_logical_scalar() returns NA unchanged",
  {
    out <- scholidonline:::.scholidonline_as_logical_scalar(
      x = NA
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".scholidonline_as_logical_scalar() errors for logical vector",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_as_logical_scalar(
        x = c(TRUE, FALSE)
      ),
      "Provider implementation must return a single logical value.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_as_logical_scalar() errors for non-logical input",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_as_logical_scalar(
        x = "TRUE"
      ),
      "Provider implementation must return a single logical value.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_as_character_scalar() returns scalar unchanged",
  {
    out <- scholidonline:::.scholidonline_as_character_scalar(
      x = "doi"
    )
    
    testthat::expect_identical(out, "doi")
  }
)


testthat::test_that(
  ".scholidonline_as_character_scalar() returns NA_character_ unchanged",
  {
    out <- scholidonline:::.scholidonline_as_character_scalar(
      x = NA_character_
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)


testthat::test_that(
  ".scholidonline_as_character_scalar() errors for character vector",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_as_character_scalar(
        x = c("doi", "pmid")
      ),
      "Provider implementation must return a single character value.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_as_character_scalar() errors for non-character input",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_as_character_scalar(
        x = 1L
      ),
      "Provider implementation must return a single character value.",
      fixed = TRUE
    )
  }
)