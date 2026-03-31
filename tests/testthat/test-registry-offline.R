testthat::test_that(
  ".scholidonline_exists_meta() returns metadata for supported type",
  {
    reg <- list(
      doi = list(
        exists = list(
          providers = c("crossref", "doi.org"),
          default_provider = "crossref"
        )
      )
    )
    
    bindings <- list(
      .scholidonline_match_type = function(x, arg = "x") {
        testthat::expect_identical(arg, "type")
        "doi"
      },
      .scholidonline_registry = function() {
        reg
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_exists_meta(
      type = "doi"
    )
    
    testthat::expect_identical(
      out,
      list(
        providers = c("crossref", "doi.org"),
        default_provider = "crossref"
      )
    )
  }
)


testthat::test_that(
  ".scholidonline_exists_meta() errors when exists is unsupported",
  {
    reg <- list(
      orcid = list(
        exists = NULL
      )
    )
    
    bindings <- list(
      .scholidonline_match_type = function(x, arg = "x") {
        testthat::expect_identical(arg, "type")
        "orcid"
      },
      .scholidonline_registry = function() {
        reg
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    testthat::expect_error(
      scholidonline:::.scholidonline_exists_meta(
        type = "orcid"
      ),
      "Existence checking is not supported for `orcid`.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_conversion_meta() returns metadata for supported pair",
  {
    reg <- list(
      pmid = list(
        convert = list(
          doi = list(
            providers = c("ncbi", "epmc"),
            default_provider = "ncbi"
          )
        )
      )
    )
    
    seen <- new.env(parent = emptyenv())
    seen$args <- character()
    
    bindings <- list(
      .scholidonline_match_type = function(x, arg = "x") {
        seen$args <- c(seen$args, arg)
        x
      },
      .scholidonline_registry = function() {
        reg
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_conversion_meta(
      from = "pmid",
      to = "doi"
    )
    
    testthat::expect_identical(seen$args, c("from", "to"))
    testthat::expect_identical(
      out,
      list(
        providers = c("ncbi", "epmc"),
        default_provider = "ncbi"
      )
    )
  }
)


testthat::test_that(
  ".scholidonline_conversion_meta() errors for unsupported pair",
  {
    reg <- list(
      pmid = list(
        convert = list(
          doi = NULL
        )
      )
    )
    
    bindings <- list(
      .scholidonline_match_type = function(x, arg = "x") {
        x
      },
      .scholidonline_registry = function() {
        reg
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    testthat::expect_error(
      scholidonline:::.scholidonline_conversion_meta(
        from = "pmid",
        to = "doi"
      ),
      "Unsupported conversion: pmid -> doi.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_match_type() returns matched type unchanged",
  {
    bindings <- list(
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid")
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_match_type(
      x = "doi"
    )
    
    testthat::expect_identical(out, "doi")
  }
)


testthat::test_that(
  ".scholidonline_match_type() supports partial matching via match.arg()",
  {
    bindings <- list(
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid")
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_match_type(
      x = "pmid"
    )
    
    testthat::expect_identical(out, "pmid")
  }
)


testthat::test_that(
  ".scholidonline_match_type() errors for non-character input",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_match_type(
        x = 1,
        arg = "type"
      ),
      "`type` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_match_type() errors for zero-length character",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_match_type(
        x = character(),
        arg = "type"
      ),
      "`type` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_match_type() errors for length > 1 character",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_match_type(
        x = c("doi", "pmid"),
        arg = "type"
      ),
      "`type` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_match_type() errors for missing character input",
  {
    testthat::expect_error(
      scholidonline:::.scholidonline_match_type(
        x = NA_character_,
        arg = "type"
      ),
      "`type` must be a single, non-missing character string.",
      fixed = TRUE
    )
  }
)


testthat::test_that(
  ".scholidonline_match_type() errors for unsupported type",
  {
    bindings <- list(
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid")
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    testthat::expect_error(
      scholidonline:::.scholidonline_match_type(
        x = "isbn",
        arg = "type"
      ),
      "'arg' should be one of",
      fixed = TRUE
    )
  }
)