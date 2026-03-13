test_that("id_links() errors for invalid x", {
  expect_error(
    id_links(x = 1),
    class = "rlang_error"
  )
})

test_that("id_links() returns typed zero-row data.frame when no inputs are usable", {
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) invisible(NULL),
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    detect_scholid_type = function(x) c(NA_character_, "isbn"),
    normalize_scholid = function(x, type) NA_character_,
    .package = "scholid"
  )
  
  out <- id_links(
    x = c(NA_character_, "9781234567890"),
    type = "auto"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_identical(nrow(out), 0L)
})

test_that("id_links() infers types, normalizes inputs, and delegates only valid elements", {
  captured <- new.env(parent = emptyenv())
  
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) invisible(NULL),
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .scholidonline_run_unary = function(x, operation, type, provider, ..., quiet) {
      captured$x <- x
      captured$operation <- operation
      captured$type <- type
      captured$provider <- provider
      captured$quiet <- quiet
      captured$dots <- list(...)
      
      list(
        data.frame(
          linked_type = "pmid",
          linked_value = "12345",
          provider = "crossref",
          stringsAsFactors = FALSE
        ),
        data.frame(
          linked_type = "doi",
          linked_value = "10.1000/test",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    detect_scholid_type = function(x) c("doi", "pmid", NA_character_),
    normalize_scholid = function(x, type) {
      if (identical(type, "doi")) {
        return("10.1000/abc")
      }
      if (identical(type, "pmid")) {
        return("31452104")
      }
      NA_character_
    },
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("doi raw", "pmid raw", "bad"),
    type = "auto",
    provider = "auto",
    foo = "bar",
    quiet = TRUE
  )
  
  expect_identical(captured$x, c("10.1000/abc", "31452104"))
  expect_identical(captured$operation, "links")
  expect_identical(captured$type, c("doi", "pmid"))
  expect_identical(captured$provider, "auto")
  expect_identical(captured$quiet, TRUE)
  expect_identical(captured$dots$foo, "bar")
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
  expect_identical(nrow(out), 2L)
  expect_identical(out$input, c("10.1000/abc", "31452104"))
  expect_identical(out$input_type, c("doi", "pmid"))
})

test_that("id_links() uses declared type for all elements", {
  captured <- new.env(parent = emptyenv())
  
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) {
      captured$checked_type <- type
      captured$checked_provider <- provider
      invisible(NULL)
    },
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .scholidonline_run_unary = function(x, operation, type, provider, ..., quiet) {
      captured$x <- x
      captured$type <- type
      captured$provider <- provider
      list(
        data.frame(
          linked_type = "doi",
          linked_value = "10.1000/a",
          provider = "ncbi",
          stringsAsFactors = FALSE
        ),
        data.frame(
          linked_type = "pmcid",
          linked_value = "PMC123",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    normalize_scholid = function(x, type) paste0("norm_", x),
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("1", "2"),
    type = "pmid",
    provider = "ncbi"
  )
  
  expect_identical(captured$checked_type, "pmid")
  expect_identical(captured$checked_provider, "ncbi")
  expect_identical(captured$x, c("norm_1", "norm_2"))
  expect_identical(captured$type, c("pmid", "pmid"))
  expect_identical(captured$provider, "ncbi")
  expect_identical(out$input, c("norm_1", "norm_2"))
  expect_identical(out$input_type, c("pmid", "pmid"))
})

test_that("id_links() drops elements with normalization failure before engine call", {
  captured <- new.env(parent = emptyenv())
  
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) invisible(NULL),
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .scholidonline_run_unary = function(x, operation, type, provider, ..., quiet) {
      captured$x <- x
      captured$type <- type
      list(
        data.frame(
          linked_type = "doi",
          linked_value = "10.1000/z",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    normalize_scholid = function(x, type) {
      if (identical(x, "bad")) {
        return(NA_character_)
      }
      paste0("norm_", x)
    },
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("good", "bad"),
    type = "pmid"
  )
  
  expect_identical(captured$x, "norm_good")
  expect_identical(captured$type, "pmid")
  expect_identical(nrow(out), 1L)
  expect_identical(out$input, "norm_good")
})

test_that("id_links() drops empty per-input results from engine output", {
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) invisible(NULL),
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .scholidonline_run_unary = function(x, operation, type, provider, ..., quiet) {
      list(
        data.frame(
          linked_type = character(),
          linked_value = character(),
          provider = character(),
          stringsAsFactors = FALSE
        ),
        data.frame(
          linked_type = "pmcid",
          linked_value = "PMC999",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    normalize_scholid = function(x, type) paste0("norm_", x),
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("a", "b"),
    type = "pmid"
  )
  
  expect_identical(nrow(out), 1L)
  expect_identical(out$input, "norm_b")
  expect_identical(out$linked_type, "pmcid")
  expect_identical(out$linked_value, "PMC999")
})

test_that("id_links() returns zero-row data.frame when engine returns only empty results", {
  local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") invisible(NULL),
    .scholidonline_check_quiet = function(quiet, arg = "quiet") invisible(NULL),
    .scholidonline_check_type_provider = function(type, provider) invisible(NULL),
    scholidonline_types = function() c("doi", "pmid", "pmcid", "orcid", "arxiv"),
    .scholidonline_providers = function() c("doi.org", "crossref", "ncbi", "epmc", "orcid", "arxiv"),
    .scholidonline_run_unary = function(x, operation, type, provider, ..., quiet) {
      list(
        data.frame(
          linked_type = character(),
          linked_value = character(),
          provider = character(),
          stringsAsFactors = FALSE
        ),
        data.frame(
          linked_type = character(),
          linked_value = character(),
          provider = character(),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "scholidonline"
  )
  
  local_mocked_bindings(
    normalize_scholid = function(x, type) paste0("norm_", x),
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("a", "b"),
    type = "pmid"
  )
  
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_identical(
    names(out),
    c("input", "input_type", "linked_type", "linked_value", "provider")
  )
})