testthat::test_that("id_links() errors for invalid x", {
  testthat::expect_error(
    id_links(x = 1),
    class = "rlang_error"
  )
})


testthat::test_that(
  "id_links() returns typed zero-row data.frame when no inputs are usable",
  {
    testthat::local_mocked_bindings(
      .scholidonline_check_x = function(x, arg = "x") {
        invisible(NULL)
      },
      .scholidonline_check_quiet = function(quiet, arg = "quiet") {
        invisible(NULL)
      },
      .scholidonline_check_type_provider = function(type, provider) {
        invisible(NULL)
      },
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid", "orcid", "arxiv")
      },
      .scholidonline_providers = function() {
        c(
          "doi.org",
          "crossref",
          "ncbi",
          "epmc",
          "orcid",
          "arxiv"
        )
      },
      .package = "scholidonline"
    )
    
    testthat::local_mocked_bindings(
      detect_scholid_type = function(x) {
        c(NA_character_, "isbn")
      },
      normalize_scholid = function(x, type) {
        NA_character_
      },
      .package = "scholid"
    )
    
    out <- id_links(
      x = c(NA_character_, "9781234567890"),
      type = "auto"
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    testthat::expect_identical(nrow(out), 0L)
  }
)


testthat::test_that(
  paste(
    "id_links() infers types, normalizes inputs,",
    "and delegates only valid elements"
  ),
  {
    captured <- new.env(parent = emptyenv())
    
    testthat::local_mocked_bindings(
      .scholidonline_check_x = function(x, arg = "x") {
        invisible(NULL)
      },
      .scholidonline_check_quiet = function(quiet, arg = "quiet") {
        invisible(NULL)
      },
      .scholidonline_check_type_provider = function(type, provider) {
        invisible(NULL)
      },
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid", "orcid", "arxiv")
      },
      .scholidonline_providers = function() {
        c(
          "doi.org",
          "crossref",
          "ncbi",
          "epmc",
          "orcid",
          "arxiv"
        )
      },
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
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
    
    testthat::local_mocked_bindings(
      detect_scholid_type = function(x) {
        c("doi", "pmid", NA_character_)
      },
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
    
    testthat::expect_identical(
      captured$x,
      c("10.1000/abc", "31452104")
    )
    testthat::expect_identical(captured$operation, "links")
    testthat::expect_identical(captured$type, c("doi", "pmid"))
    testthat::expect_identical(captured$provider, "auto")
    testthat::expect_identical(captured$quiet, TRUE)
    testthat::expect_identical(captured$dots$foo, "bar")
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
    testthat::expect_identical(nrow(out), 2L)
    testthat::expect_identical(
      out$query,
      c("10.1000/abc", "31452104")
    )
    testthat::expect_identical(
      out$query_type,
      c("doi", "pmid")
    )
  }
)


testthat::test_that("id_links() uses declared type for all elements", {
  captured <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_check_x = function(x, arg = "x") {
      invisible(NULL)
    },
    .scholidonline_check_quiet = function(quiet, arg = "quiet") {
      invisible(NULL)
    },
    .scholidonline_check_type_provider = function(type, provider) {
      captured$checked_type <- type
      captured$checked_provider <- provider
      invisible(NULL)
    },
    scholidonline_types = function() {
      c("doi", "pmid", "pmcid", "orcid", "arxiv")
    },
    .scholidonline_providers = function() {
      c(
        "doi.org",
        "crossref",
        "ncbi",
        "epmc",
        "orcid",
        "arxiv"
      )
    },
    .scholidonline_run_unary = function(
      x,
      operation,
      type,
      provider,
      ...,
      quiet
    ) {
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
  
  testthat::local_mocked_bindings(
    normalize_scholid = function(x, type) {
      paste0("norm_", x)
    },
    .package = "scholid"
  )
  
  out <- id_links(
    x = c("1", "2"),
    type = "pmid",
    provider = "ncbi"
  )
  
  testthat::expect_identical(captured$checked_type, "pmid")
  testthat::expect_identical(captured$checked_provider, "ncbi")
  testthat::expect_identical(captured$x, c("norm_1", "norm_2"))
  testthat::expect_identical(captured$type, c("pmid", "pmid"))
  testthat::expect_identical(captured$provider, "ncbi")
  testthat::expect_identical(out$query, c("norm_1", "norm_2"))
  testthat::expect_identical(
    out$query_type,
    c("pmid", "pmid")
  )
})


testthat::test_that(
  "id_links() drops elements with normalization failure before engine call",
  {
    captured <- new.env(parent = emptyenv())
    
    testthat::local_mocked_bindings(
      .scholidonline_check_x = function(x, arg = "x") {
        invisible(NULL)
      },
      .scholidonline_check_quiet = function(quiet, arg = "quiet") {
        invisible(NULL)
      },
      .scholidonline_check_type_provider = function(type, provider) {
        invisible(NULL)
      },
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid", "orcid", "arxiv")
      },
      .scholidonline_providers = function() {
        c(
          "doi.org",
          "crossref",
          "ncbi",
          "epmc",
          "orcid",
          "arxiv"
        )
      },
      .scholidonline_run_unary = function(
        x,
        operation,
        type,
        provider,
        ...,
        quiet
      ) {
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
    
    testthat::local_mocked_bindings(
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
    
    testthat::expect_identical(captured$x, "norm_good")
    testthat::expect_identical(captured$type, "pmid")
    testthat::expect_identical(nrow(out), 1L)
    testthat::expect_identical(out$query, "norm_good")
  }
)


testthat::test_that(
  "id_links() drops empty per-input results from engine output",
  {
    testthat::local_mocked_bindings(
      .scholidonline_check_x = function(x, arg = "x") {
        invisible(NULL)
      },
      .scholidonline_check_quiet = function(quiet, arg = "quiet") {
        invisible(NULL)
      },
      .scholidonline_check_type_provider = function(type, provider) {
        invisible(NULL)
      },
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid", "orcid", "arxiv")
      },
      .scholidonline_providers = function() {
        c(
          "doi.org",
          "crossref",
          "ncbi",
          "epmc",
          "orcid",
          "arxiv"
        )
      },
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
    
    testthat::local_mocked_bindings(
      normalize_scholid = function(x, type) {
        paste0("norm_", x)
      },
      .package = "scholid"
    )
    
    out <- id_links(
      x = c("a", "b"),
      type = "pmid"
    )
    
    testthat::expect_identical(nrow(out), 1L)
    testthat::expect_identical(out$query, "norm_b")
    testthat::expect_identical(out$linked_type, "pmcid")
    testthat::expect_identical(out$linked_id, "PMC999")
  }
)


testthat::test_that(
  paste(
    "id_links() returns zero-row data.frame",
    "when engine returns only empty results"
  ),
  {
    testthat::local_mocked_bindings(
      .scholidonline_check_x = function(x, arg = "x") {
        invisible(NULL)
      },
      .scholidonline_check_quiet = function(quiet, arg = "quiet") {
        invisible(NULL)
      },
      .scholidonline_check_type_provider = function(type, provider) {
        invisible(NULL)
      },
      scholidonline_types = function() {
        c("doi", "pmid", "pmcid", "orcid", "arxiv")
      },
      .scholidonline_providers = function() {
        c(
          "doi.org",
          "crossref",
          "ncbi",
          "epmc",
          "orcid",
          "arxiv"
        )
      },
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
    
    testthat::local_mocked_bindings(
      normalize_scholid = function(x, type) {
        paste0("norm_", x)
      },
      .package = "scholid"
    )
    
    out <- id_links(
      x = c("a", "b"),
      type = "pmid"
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
    testthat::expect_identical(
      names(out),
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      )
    )
  }
)