testthat::test_that(
  ".scholidonline_get_unary_meta() returns registry metadata", {
  meta <- .scholidonline_get_unary_meta(
    type = "doi",
    operation = "exists"
  )
  
  testthat::expect_type(meta, "list")
  testthat::expect_named(
    meta,
    c("providers", "default_provider", "dispatcher")
  )
  testthat::expect_true(all(c(
    "auto",
    "doi.org",
    "crossref"
    ) %in% meta$providers))
  testthat::expect_identical(meta$default_provider, "doi.org")
  testthat::expect_identical(meta$dispatcher, ".exists_doi")
})


testthat::test_that(".scholidonline_get_unary_meta() errors on unknown type", {
  testthat::expect_error(
    .scholidonline_get_unary_meta(
      type = "not_a_type",
      operation = "exists"
    ),
    "Unknown identifier type: not_a_type\\."
  )
})


testthat::test_that(
  ".scholidonline_get_unary_meta() errors on unsupported operation", {
  testthat::expect_error(
    .scholidonline_get_unary_meta(
      type = "doi",
      operation = "not_an_operation"
    ),
    "Operation `not_an_operation` not supported for type `doi`\\."
  )
})


testthat::test_that(
  ".scholidonline_get_unary_meta() errors on missing providers", {
  testthat::local_mocked_bindings(
    .scholidonline_registry = function() {
      list(
        doi = list(
          exists = list(
            default_provider = "doi.org",
            dispatcher = ".exists_doi"
          )
        )
      )
    }
  )
  
  testthat::expect_error(
    .scholidonline_get_unary_meta(
      type = "doi",
      operation = "exists"
    ),
    "Registry error: missing `providers` for doi -> exists\\."
  )
})


testthat::test_that(
  ".scholidonline_get_unary_meta() errors on missing default_provider", {
  testthat::local_mocked_bindings(
    .scholidonline_registry = function() {
      list(
        doi = list(
          exists = list(
            providers = c("auto", "doi.org"),
            dispatcher = ".exists_doi"
          )
        )
      )
    }
  )
  
  testthat::expect_error(
    .scholidonline_get_unary_meta(
      type = "doi",
      operation = "exists"
    ),
    "Registry error: missing `default_provider` for doi -> exists\\."
  )
})


testthat::test_that(
  ".scholidonline_get_unary_meta() errors on missing dispatcher", {
  testthat::local_mocked_bindings(
    .scholidonline_registry = function() {
      list(
        doi = list(
          exists = list(
            providers = c("auto", "doi.org"),
            default_provider = "doi.org"
          )
        )
      )
    }
  )
  
  testthat::expect_error(
    .scholidonline_get_unary_meta(
      type = "doi",
      operation = "exists"
    ),
    "Registry error: missing `dispatcher` for doi -> exists\\."
  )
})


testthat::test_that(
  ".scholidonline_resolve_unary_provider() validates inputs", {
  meta <- list(
    providers = c("auto", "ncbi", "epmc"),
    default_provider = "ncbi",
    dispatcher = ".exists_pmid"
  )
  
  testthat::expect_identical(
    .scholidonline_resolve_unary_provider("auto", meta),
    "auto"
  )
  
  testthat::expect_identical(
    .scholidonline_resolve_unary_provider("ncbi", meta),
    "ncbi"
  )
  
  testthat::expect_error(
    .scholidonline_resolve_unary_provider(
      provider = c("ncbi", "epmc"),
      meta = meta
    ),
    "`provider` must be a single, non-missing character string\\."
  )
  
  testthat::expect_error(
    .scholidonline_resolve_unary_provider(
      provider = NA_character_,
      meta = meta
    ),
    "`provider` must be a single, non-missing character string\\."
  )
  
  testthat::expect_error(
    .scholidonline_resolve_unary_provider(
      provider = "crossref",
      meta = meta
    ),
    "Provider `crossref` is not supported for this identifier type\\."
  )
})


testthat::test_that(
  ".scholidonline_resolve_unary_provider() checks meta structure", {
  testthat::expect_error(
    .scholidonline_resolve_unary_provider(
      provider = "auto",
      meta = "not_a_list"
    ),
    "`meta` must be a list\\."
  )
  
  testthat::expect_error(
    .scholidonline_resolve_unary_provider(
      provider = "auto",
      meta = list(default_provider = "ncbi")
    ),
    "`meta` must contain `providers`\\."
  )
})


testthat::test_that(
  ".scholidonline_unary_return_mode() maps known operations", {
  testthat::expect_identical(
    .scholidonline_unary_return_mode("exists"),
    "logical_scalar"
  )
  testthat::expect_identical(
    .scholidonline_unary_return_mode("meta"),
    "list_scalar"
  )
  testthat::expect_identical(
    .scholidonline_unary_return_mode("links"),
    "list_scalar"
  )
})


testthat::test_that(".scholidonline_unary_return_mode() rejects bad input", {
  testthat::expect_error(
    .scholidonline_unary_return_mode(c("exists", "meta")),
    "`operation` must be a single, non-missing character string\\."
  )
  
  testthat::expect_error(
    .scholidonline_unary_return_mode(NA_character_),
    "`operation` must be a single, non-missing character string\\."
  )
  
  testthat::expect_error(
    .scholidonline_unary_return_mode("nope"),
    "Unknown unary operation: `nope`\\."
  )
})


testthat::test_that(
  ".scholidonline_as_list_scalar() accepts NULL, data.frame, scalar list", {
  testthat::expect_equal(
    .scholidonline_as_list_scalar(NULL),
    data.frame()
  )
  
  df <- data.frame(a = 1, stringsAsFactors = FALSE)
  testthat::expect_identical(
    .scholidonline_as_list_scalar(df),
    df
  )
  
  lst <- list(a = 1)
  testthat::expect_identical(
    .scholidonline_as_list_scalar(lst),
    lst
  )
})


testthat::test_that(".scholidonline_as_list_scalar() rejects invalid inputs", {
  testthat::expect_error(
    .scholidonline_as_list_scalar(list(a = 1, b = 2)),
    "`x` must be a data.frame, NULL, or a scalar list object\\."
  )
  
  testthat::expect_error(
    .scholidonline_as_list_scalar("abc"),
    "`x` must be a data.frame, NULL, or a scalar list object\\."
  )
})


testthat::test_that(
  ".scholidonline_validate_unary_result() dispatches by return mode", {
  testthat::expect_identical(
    .scholidonline_validate_unary_result(
      x = TRUE,
      return_mode = "logical_scalar"
    ),
    .scholidonline_as_logical_scalar(TRUE)
  )
  
  df <- data.frame(a = 1, stringsAsFactors = FALSE)
  testthat::expect_identical(
    .scholidonline_validate_unary_result(
      x = df,
      return_mode = "list_scalar"
    ),
    df
  )
})


testthat::test_that(
  ".scholidonline_validate_unary_result() rejects bad return_mode", {
  testthat::expect_error(
    .scholidonline_validate_unary_result(
      x = TRUE,
      return_mode = NA_character_
    ),
    "`return_mode` must be a single, non-missing character string\\."
  )
  
  testthat::expect_error(
    .scholidonline_validate_unary_result(
      x = TRUE,
      return_mode = "not_a_mode"
    ),
    "Unknown unary `return_mode`: `not_a_mode`\\."
  )
})


testthat::test_that(
  ".scholidonline_run_unary_one() calls dispatcher and validates exists", {
  dispatcher <- function(x, provider, ..., quiet) {
    testthat::expect_identical(x, "12345")
    testthat::expect_identical(provider, "ncbi")
    testthat::expect_identical(quiet, TRUE)
    TRUE
  }
  
  out <- .scholidonline_run_unary_one(
    x = "12345",
    dispatcher = dispatcher,
    provider = "ncbi",
    operation = "exists",
    quiet = TRUE
  )
  
  testthat::expect_identical(out, TRUE)
})


testthat::test_that(
  ".scholidonline_run_unary_one() calls dispatcher and validates meta", {
  dispatcher <- function(x, provider, ..., quiet) {
    testthat::expect_identical(x, "10.1000/test")
    testthat::expect_identical(provider, "crossref")
    testthat::expect_identical(quiet, FALSE)
    data.frame(title = "A paper", stringsAsFactors = FALSE)
  }
  
  out <- .scholidonline_run_unary_one(
    x = "10.1000/test",
    dispatcher = dispatcher,
    provider = "crossref",
    operation = "meta",
    quiet = FALSE
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(out$title, "A paper")
})


testthat::test_that(
  ".scholidonline_run_unary_one() rejects non-function dispatcher", {
  testthat::expect_error(
    .scholidonline_run_unary_one(
      x = "12345",
      dispatcher = "not_a_function",
      provider = "ncbi",
      operation = "exists",
      quiet = TRUE
    ),
    "`dispatcher` must be a function\\."
  )
})


testthat::test_that(
  ".scholidonline_run_unary() expands scalar type and runs elementwise", {
    testthat::local_mocked_bindings(
      .scholidonline_get_unary_meta = function(type, operation) {
        testthat::expect_identical(type, "pmid")
        testthat::expect_identical(operation, "exists")
        list(
          providers = c("auto", "ncbi"),
          default_provider = "ncbi",
          dispatcher = ".exists_pmid"
        )
      },
      .scholidonline_resolve_unary_provider = function(provider, meta) {
        testthat::expect_identical(provider, "auto")
        testthat::expect_true(is.list(meta))
        "auto"
      },
      .get_unary_batch_dispatcher = function(meta) {
        NULL
      },
      .scholidonline_get_dispatcher = function(name) {
        testthat::expect_identical(name, ".exists_pmid")
        function(x, provider, ..., quiet) {
          identical(x, "1")
        }
      },
      .scholidonline_run_unary_one = function(
    x,
    dispatcher,
    provider,
    operation,
    ...,
    quiet
      ) {
        testthat::expect_true(is.function(dispatcher))
        testthat::expect_identical(provider, "auto")
        testthat::expect_identical(operation, "exists")
        testthat::expect_identical(quiet, TRUE)
        dispatcher(
          x = x,
          provider = provider,
          quiet = quiet,
          ...
        )
      }
    )
    
    out <- .scholidonline_run_unary(
      x = c("1", "2"),
      operation = "exists",
      type = "pmid",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, list(TRUE, FALSE))
})


testthat::test_that(
  ".scholidonline_run_unary() accepts type vector matching x", {
  testthat::local_mocked_bindings(
    .scholidonline_get_unary_meta = function(type, operation) {
      list(
        providers = c("auto", "stub"),
        default_provider = "stub",
        dispatcher = paste0(".dispatch_", type, "_", operation)
      )
    },
    .scholidonline_resolve_unary_provider = function(provider, meta) {
      "stub"
    },
    .scholidonline_get_dispatcher = function(name) {
      function(x, provider, ..., quiet) {
        list(name = name, x = x, provider = provider, quiet = quiet)
      }
    },
    .scholidonline_run_unary_one = function(
      x,
      dispatcher,
      provider,
      operation,
      ...,
      quiet
    ) {
      dispatcher(
        x = x,
        provider = provider,
        quiet = quiet,
        ...
        )
    }
  )
  
  out <- .scholidonline_run_unary(
    x = c("a", "b"),
    operation = "meta",
    type = c("doi", "pmid"),
    provider = "auto",
    quiet = FALSE
  )
  
  testthat::expect_length(out, 2)
  testthat::expect_identical(out[[1]]$name, ".dispatch_doi_meta")
  testthat::expect_identical(out[[2]]$name, ".dispatch_pmid_meta")
  testthat::expect_identical(out[[1]]$x, "a")
  testthat::expect_identical(out[[2]]$x, "b")
})


testthat::test_that(
  ".scholidonline_run_unary() errors on invalid type length", {
  testthat::expect_error(
    .scholidonline_run_unary(
      x = c("a", "b"),
      operation = "exists",
      type = c("pmid", "doi", "pmcid"),
      provider = "auto",
      quiet = TRUE
    ),
    "`type` must have length 1 or length\\(x\\)\\."
  )
})


testthat::test_that(
  ".scholidonline_run_unary() returns NA for NA x without dispatch", {
  called <- FALSE
  
  testthat::local_mocked_bindings(
    .scholidonline_get_unary_meta = function(type, operation) {
      called <<- TRUE
      list(
        providers = c("auto", "stub"),
        default_provider = "stub",
        dispatcher = ".stub"
      )
    },
    .scholidonline_resolve_unary_provider = function(provider, meta) {
      called <<- TRUE
      "stub"
    },
    .scholidonline_get_dispatcher = function(name) {
      called <<- TRUE
      function(x, provider, ..., quiet) TRUE
    },
    .scholidonline_run_unary_one = function(
      x,
      dispatcher,
      provider,
      operation,
      ...,
      quiet
    ) {
      called <<- TRUE
      TRUE
    }
  )
  
  out <- .scholidonline_run_unary(
    x = NA_character_,
    operation = "exists",
    type = "pmid",
    provider = "auto",
    quiet = TRUE
  )
  
  testthat::expect_identical(out, list(NA))
  testthat::expect_false(called)
})


testthat::test_that(
  ".scholidonline_run_unary() returns NA for NA type without dispatch", {
  called <- FALSE
  
  testthat::local_mocked_bindings(
    .scholidonline_get_unary_meta = function(type, operation) {
      called <<- TRUE
      list(
        providers = c("auto", "stub"),
        default_provider = "stub",
        dispatcher = ".stub"
      )
    },
    .scholidonline_resolve_unary_provider = function(provider, meta) {
      called <<- TRUE
      "stub"
    },
    .scholidonline_get_dispatcher = function(name) {
      called <<- TRUE
      function(x, provider, ..., quiet) TRUE
    },
    .scholidonline_run_unary_one = function(
      x,
      dispatcher,
      provider,
      operation,
      ...,
      quiet
    ) {
      called <<- TRUE
      TRUE
    }
  )
  
  out <- .scholidonline_run_unary(
    x = "12345",
    operation = "exists",
    type = NA_character_,
    provider = "auto",
    quiet = TRUE
  )
  
  testthat::expect_identical(out, list(NA))
  testthat::expect_false(called)
})


testthat::test_that(".scholidonline_run_unary() handles zero-length input", {
  out <- .scholidonline_run_unary(
    x = character(),
    operation = "exists",
    type = "pmid",
    provider = "auto",
    quiet = TRUE
  )
  
  testthat::expect_identical(out, list())
})