testthat::test_that(
  "id_exists (CRAN/offline): checks DOI existence with declared type",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(operation, "exists")
        testthat::expect_identical(type, "doi")
        testthat::expect_identical(provider, "auto")
        testthat::expect_identical(quiet, FALSE)
        list(TRUE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = "10.1000/182",
      type = "doi"
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 1L)
    testthat::expect_identical(out, TRUE)
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): vectorizes over declared type input",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(operation, "exists")
        testthat::expect_identical(type, rep("doi", 2L))
        list(TRUE, FALSE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c("10.1000/182", "10.9999/nope"),
      type = "doi"
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 2L)
    testthat::expect_identical(out, c(TRUE, FALSE))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): infers type per element when type is NULL",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(operation, "exists")
        testthat::expect_identical(
          type,
          c("doi", "pmid", "pmcid")
        )
        list(TRUE, TRUE, FALSE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c(
        "10.1000/182",
        "12345678",
        "PMC12345"
      ),
      type = NULL
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(out, c(TRUE, TRUE, FALSE))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): unsupported inferred types yield NA",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(x, c("10.1000/182", "12345678"))
        testthat::expect_identical(type, c("doi", "pmid"))
        list(TRUE, FALSE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c(
        "10.1000/182",
        "12345678",
        "not-an-id"
      ),
      type = NULL
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_length(out, 3L)
    testthat::expect_identical(out, c(TRUE, FALSE, NA))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): NA inputs yield NA outputs",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(x, "10.1000/182")
        testthat::expect_identical(type, "doi")
        list(TRUE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c(NA, "10.1000/182"),
      type = c("doi")[1]
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(out, c(NA, TRUE))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): normalization failures yield NA",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(x, "10.1000/182")
        testthat::expect_identical(type, "doi")
        list(TRUE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c("10.1000/182", "not-a-doi"),
      type = "doi"
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(out, c(TRUE, NA))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): returns all NA when nothing is classifiable",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(...) {
        testthat::fail(
          "Unary engine should not be called when no inputs are usable."
          )
      },
      .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = c("not-an-id", NA),
      type = NULL
    )
    
    testthat::expect_type(out, "logical")
    testthat::expect_identical(out, c(NA, NA))
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): passes quiet through to unary engine",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_true(quiet)
        list(TRUE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = "10.1000/182",
      type = "doi",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, TRUE)
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): passes explicit provider through to unary engine",
  {
    testthat::local_mocked_bindings(
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_identical(provider, "crossref")
        list(TRUE)
      },
    .package = "scholidonline"
    )
    
    out <- scholidonline::id_exists(
      x = "10.1000/182",
      type = "doi",
      provider = "crossref"
    )
    
    testthat::expect_identical(out, TRUE)
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): errors on invalid type",
  {
    testthat::expect_error(
      scholidonline::id_exists(
        x = "10.1000/182",
        type = "notatype"
      ),
      "'arg' should be one of",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): errors on invalid quiet",
  {
    testthat::expect_error(
      scholidonline::id_exists(
        x = "10.1000/182",
        type = "doi",
        quiet = NA
      ),
      "quiet",
      ignore.case = TRUE
    )
  }
)


testthat::test_that(
  "id_exists (CRAN/offline): errors on invalid provider before dispatch",
  {
    testthat::expect_error(
      scholidonline::id_exists(
        x = "10.1000/182",
        type = "doi",
        provider = "notaprovider"
      ),
      "'arg' should be one of",
      ignore.case = TRUE
    )
  }
)