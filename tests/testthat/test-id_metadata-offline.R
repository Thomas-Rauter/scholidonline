testthat::test_that("id_metadata() rejects invalid x", {
  testthat::expect_error(
    id_metadata(NULL),
    regexp = "x"
  )
})


testthat::test_that("id_metadata() rejects invalid type", {
  testthat::expect_error(
    id_metadata("10.1038/nature12373", type = "banana"),
    regexp = "arg"
  )
})


testthat::test_that("id_metadata() rejects invalid provider", {
  testthat::expect_error(
    id_metadata("10.1038/nature12373", provider = "banana"),
    regexp = "arg"
  )
})


testthat::test_that("id_metadata() rejects invalid quiet", {
  testthat::expect_error(
    id_metadata("10.1038/nature12373", quiet = "no"),
    regexp = "quiet"
  )
})


testthat::test_that("id_metadata() returns NA row for invalid identifier", {
  out <- id_metadata("not_an_identifier")
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(
    names(out),
    c(
      "input",
      "type",
      "provider",
      "title",
      "year",
      "container",
      "doi",
      "pmid",
      "pmcid",
      "url"
    )
  )
  testthat::expect_equal(out$input, "not_an_identifier")
  testthat::expect_true(is.na(out$type))
  testthat::expect_true(is.na(out$provider))
  testthat::expect_true(is.na(out$title))
  testthat::expect_true(is.na(out$year))
  testthat::expect_true(is.na(out$container))
  testthat::expect_true(is.na(out$doi))
  testthat::expect_true(is.na(out$pmid))
  testthat::expect_true(is.na(out$pmcid))
  testthat::expect_true(is.na(out$url))
})


testthat::test_that("id_metadata() uses explicit type and normalizes x", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
    x,
    operation,
    type,
    provider,
    ...,
    quiet
    ) {
      seen$x <- x
      seen$operation <- operation
      seen$type <- type
      seen$provider <- provider
      seen$quiet <- quiet
      
      list(
        data.frame(
          title = "Title A",
          year = 2020L,
          container = "Journal A",
          doi = "10.1000/a",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/a",
          provider = "crossref",
          stringsAsFactors = FALSE
        ),
        data.frame(
          title = "Title B",
          year = 2021L,
          container = "Journal B",
          doi = "10.1000/b",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/b",
          provider = "crossref",
          stringsAsFactors = FALSE
        )
      )
    }
  )
  
  out <- id_metadata(
    x = c("10.1000/A", "10.1000/B"),
    type = "doi",
    provider = "crossref",
    quiet = TRUE
  )
  
  testthat::expect_equal(
    seen$x,
    c("10.1000/A", "10.1000/B")
  )
  testthat::expect_equal(seen$operation, "meta")
  testthat::expect_equal(seen$type, c("doi", "doi"))
  testthat::expect_equal(seen$provider, "crossref")
  testthat::expect_true(isTRUE(seen$quiet))
  
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(
    out$input,
    c("10.1000/A", "10.1000/B")
  )
  testthat::expect_equal(out$type, c("doi", "doi"))
  testthat::expect_equal(
    out$provider,
    c("crossref", "crossref")
  )
  testthat::expect_equal(out$title, c("Title A", "Title B"))
  testthat::expect_equal(out$year, c(2020L, 2021L))
})


testthat::test_that("id_metadata() auto-detects type per element", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
      seen$x <- x
      seen$type <- type
      
      list(
        data.frame(
          title = "DOI title",
          year = 2020L,
          container = "Journal A",
          doi = "10.1000/a",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/a",
          provider = "crossref",
          stringsAsFactors = FALSE
        ),
        data.frame(
          title = "PMID title",
          year = 2021L,
          container = "Journal B",
          doi = NA_character_,
          pmid = "31452104",
          pmcid = NA_character_,
          url = "https://example.org/b",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      )
    }
  )
  
  out <- id_metadata(c("10.1000/A", "31452104"))
  
  testthat::expect_equal(
    seen$x,
    c("10.1000/A", "31452104")
  )
  testthat::expect_equal(seen$type, c("doi", "pmid"))
  
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$type, c("doi", "pmid"))
  testthat::expect_equal(
    out$provider,
    c("crossref", "ncbi")
  )
  testthat::expect_equal(
    out$title,
    c("DOI title", "PMID title")
  )
})


testthat::test_that("id_metadata() keeps unresolved valid inputs as NA rows", {
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
      list(
        data.frame(),
        data.frame(
          title = "Resolved",
          year = 2022L,
          container = "Journal X",
          doi = "10.1000/b",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/x",
          provider = "crossref",
          stringsAsFactors = FALSE
        )
      )
    }
  )
  
  out <- id_metadata(
    x = c("10.1000/A", "10.1000/B"),
    type = "doi"
  )
  
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(is.na(out$provider[1]))
  testthat::expect_true(is.na(out$title[1]))
  testthat::expect_equal(out$provider[2], "crossref")
  testthat::expect_equal(out$title[2], "Resolved")
})


testthat::test_that("id_metadata() supports field selection", {
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
      list(
        data.frame(
          title = "Resolved",
          year = 2022L,
          container = "Journal X",
          doi = "10.1000/x",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/x",
          provider = "crossref",
          stringsAsFactors = FALSE
        )
      )
    }
  )
  
  out <- id_metadata(
    x = "10.1000/X",
    type = "doi",
    fields = c("input", "title", "doi")
  )
  
  testthat::expect_equal(
    names(out),
    c("input", "title", "doi")
  )
  testthat::expect_equal(out$input, "10.1000/X")
  testthat::expect_equal(out$title, "Resolved")
  testthat::expect_equal(out$doi, "10.1000/x")
})


testthat::test_that("id_metadata() ignores unknown requested fields", {
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
      list(
        data.frame(
          title = "Resolved",
          year = 2022L,
          container = "Journal X",
          doi = "10.1000/x",
          pmid = NA_character_,
          pmcid = NA_character_,
          url = "https://example.org/x",
          provider = "crossref",
          stringsAsFactors = FALSE
        )
      )
    }
  )
  
  out <- id_metadata(
    x = "10.1000/X",
    type = "doi",
    fields = c("input", "title", "banana")
  )
  
  testthat::expect_equal(
    names(out),
    c("input", "title")
  )
})


testthat::test_that("id_metadata() preserves input order", {
  testthat::local_mocked_bindings(
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
      lapply(
        X = x,
        FUN = function(xi) {
          data.frame(
            title = paste0("title::", xi),
            year = 2024L,
            container = "Journal",
            doi = xi,
            pmid = NA_character_,
            pmcid = NA_character_,
            url = paste0("https://example.org/", xi),
            provider = "crossref",
            stringsAsFactors = FALSE
          )
        }
      )
    }
  )
  
  out <- id_metadata(
    x = c("10.1000/Z", "10.1000/A", "10.1000/M"),
    type = "doi"
  )
  
  testthat::expect_equal(
    out$input,
    c("10.1000/Z", "10.1000/A", "10.1000/M")
  )
  testthat::expect_equal(
    out$title,
    c(
      "title::10.1000/Z",
      "title::10.1000/A",
      "title::10.1000/M"
    )
  )
})