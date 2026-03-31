testthat::test_that(
  ".scholidonline_get_binary_meta() returns registry metadata",
  {
    meta <- .scholidonline_get_binary_meta(
      from = "pmid",
      to = "doi"
    )
    
    testthat::expect_type(meta, "list")
    testthat::expect_named(
      meta,
      c("providers", "default_provider", "dispatcher")
    )
    testthat::expect_true(
      all(c("auto", "ncbi", "epmc") %in% meta$providers)
    )
    testthat::expect_identical(meta$default_provider, "ncbi")
    testthat::expect_identical(
      meta$dispatcher,
      ".convert_pmid_to_doi"
    )
  }
)


testthat::test_that(
  ".scholidonline_get_binary_meta() errors on unknown source type",
  {
    testthat::expect_error(
      .scholidonline_get_binary_meta(
        from = "not_a_type",
        to = "doi"
      ),
      "Unknown source identifier type: not_a_type\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_get_binary_meta() errors on unsupported conversion",
  {
    testthat::expect_error(
      .scholidonline_get_binary_meta(
        from = "doi",
        to = "orcid"
      ),
      "Unsupported conversion: `doi` -> `orcid`\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_get_binary_meta() errors on missing providers",
  {
    testthat::local_mocked_bindings(
      .scholidonline_registry = function() {
        list(
          pmid = list(
            convert = list(
              doi = list(
                default_provider = "ncbi",
                dispatcher = ".convert_pmid_to_doi"
              )
            )
          )
        )
      }
    )
    
    testthat::expect_error(
      .scholidonline_get_binary_meta(
        from = "pmid",
        to = "doi"
      ),
      "Registry error: missing `providers` for pmid -> doi\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_get_binary_meta() errors on missing default_provider",
  {
    testthat::local_mocked_bindings(
      .scholidonline_registry = function() {
        list(
          pmid = list(
            convert = list(
              doi = list(
                providers = c("auto", "ncbi"),
                dispatcher = ".convert_pmid_to_doi"
              )
            )
          )
        )
      }
    )
    
    testthat::expect_error(
      .scholidonline_get_binary_meta(
        from = "pmid",
        to = "doi"
      ),
      "Registry error: missing `default_provider` for pmid -> doi\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_get_binary_meta() errors on missing dispatcher",
  {
    testthat::local_mocked_bindings(
      .scholidonline_registry = function() {
        list(
          pmid = list(
            convert = list(
              doi = list(
                providers = c("auto", "ncbi"),
                default_provider = "ncbi"
              )
            )
          )
        )
      }
    )
    
    testthat::expect_error(
      .scholidonline_get_binary_meta(
        from = "pmid",
        to = "doi"
      ),
      "Registry error: missing `dispatcher` for pmid -> doi\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_resolve_binary_provider() validates inputs",
  {
    meta <- list(
      providers = c("auto", "ncbi", "epmc"),
      default_provider = "ncbi",
      dispatcher = ".convert_pmid_to_doi"
    )
    
    testthat::expect_identical(
      .scholidonline_resolve_binary_provider("auto", meta),
      "auto"
    )
    
    testthat::expect_identical(
      .scholidonline_resolve_binary_provider("ncbi", meta),
      "ncbi"
    )
    
    testthat::expect_error(
      .scholidonline_resolve_binary_provider(
        provider = c("ncbi", "epmc"),
        meta = meta
      ),
      "`provider` must be a single, non-missing character string\\."
    )
    
    testthat::expect_error(
      .scholidonline_resolve_binary_provider(
        provider = NA_character_,
        meta = meta
      ),
      "`provider` must be a single, non-missing character string\\."
    )
    
    testthat::expect_error(
      .scholidonline_resolve_binary_provider(
        provider = "crossref",
        meta = meta
      ),
      "Unknown provider: `crossref`\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_resolve_binary_provider() checks meta structure",
  {
    testthat::expect_error(
      .scholidonline_resolve_binary_provider(
        provider = "auto",
        meta = "not_a_list"
      ),
      "`meta` must be a list\\."
    )
    
    testthat::expect_error(
      .scholidonline_resolve_binary_provider(
        provider = "auto",
        meta = list(default_provider = "ncbi")
      ),
      "`meta` must contain `providers`\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_binary_identity() detects identity mappings",
  {
    testthat::expect_true(
      .scholidonline_binary_identity(
        from = "doi",
        to = "doi"
      )
    )
    
    testthat::expect_false(
      .scholidonline_binary_identity(
        from = "pmid",
        to = "doi"
      )
    )
  }
)


testthat::test_that(
  ".scholidonline_binary_identity() rejects bad input",
  {
    testthat::expect_error(
      .scholidonline_binary_identity(
        from = c("pmid", "pmcid"),
        to = "doi"
      ),
      "`from` must be a single, non-missing character string\\."
    )
    
    testthat::expect_error(
      .scholidonline_binary_identity(
        from = NA_character_,
        to = "doi"
      ),
      "`from` must be a single, non-missing character string\\."
    )
    
    testthat::expect_error(
      .scholidonline_binary_identity(
        from = "pmid",
        to = c("doi", "pmcid")
      ),
      "`to` must be a single, non-missing character string\\."
    )
    
    testthat::expect_error(
      .scholidonline_binary_identity(
        from = "pmid",
        to = NA_character_
      ),
      "`to` must be a single, non-missing character string\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_run_binary_one() calls dispatcher and validates",
  {
    dispatcher <- function(x, from, to, provider, ..., quiet) {
      testthat::expect_identical(x, "12345")
      testthat::expect_identical(from, "pmid")
      testthat::expect_identical(to, "doi")
      testthat::expect_identical(provider, "ncbi")
      testthat::expect_identical(quiet, TRUE)
      "10.1000/test"
    }
    
    out <- .scholidonline_run_binary_one(
      x = "12345",
      dispatcher = dispatcher,
      from = "pmid",
      to = "doi",
      provider = "ncbi",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, "10.1000/test")
  }
)


testthat::test_that(
  ".scholidonline_run_binary_one() rejects non-function dispatcher",
  {
    testthat::expect_error(
      .scholidonline_run_binary_one(
        x = "12345",
        dispatcher = "not_a_function",
        from = "pmid",
        to = "doi",
        provider = "ncbi",
        quiet = TRUE
      ),
      "`dispatcher` must be a function\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_run_binary() expands scalar from and runs elementwise",
  {
    testthat::local_mocked_bindings(
      .scholidonline_binary_identity = function(from, to) {
        testthat::expect_identical(from, "pmid")
        testthat::expect_identical(to, "doi")
        FALSE
      },
      .scholidonline_get_binary_meta = function(from, to) {
        testthat::expect_identical(from, "pmid")
        testthat::expect_identical(to, "doi")
        list(
          providers = c("auto", "ncbi"),
          default_provider = "ncbi",
          dispatcher = ".convert_pmid_to_doi"
        )
      },
      .scholidonline_resolve_binary_provider = function(provider, meta) {
        testthat::expect_identical(provider, "auto")
        testthat::expect_true(is.list(meta))
        "auto"
      },
      .scholidonline_get_dispatcher = function(name) {
        testthat::expect_identical(name, ".convert_pmid_to_doi")
        function(x, from, to, provider, ..., quiet) {
          paste0("doi:", x)
        }
      },
      .scholidonline_run_binary_one = function(
        x,
        dispatcher,
        from,
        to,
        provider,
        ...,
        quiet
      ) {
        testthat::expect_true(is.function(dispatcher))
        testthat::expect_identical(from, "pmid")
        testthat::expect_identical(to, "doi")
        testthat::expect_identical(provider, "auto")
        testthat::expect_identical(quiet, TRUE)
        dispatcher(
          x = x,
          from = from,
          to = to,
          provider = provider,
          quiet = quiet,
          ...
        )
      }
    )
    
    out <- .scholidonline_run_binary(
      x = c("1", "2"),
      from = "pmid",
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("doi:1", "doi:2")
    )
  }
)


testthat::test_that(
  ".scholidonline_run_binary() accepts from vector matching x",
  {
    testthat::local_mocked_bindings(
      .scholidonline_binary_identity = function(from, to) {
        FALSE
      },
      .scholidonline_get_binary_meta = function(from, to) {
        list(
          providers = c("auto", "stub"),
          default_provider = "stub",
          dispatcher = paste0(".dispatch_", from, "_to_", to)
        )
      },
      .scholidonline_resolve_binary_provider = function(provider, meta) {
        "stub"
      },
      .scholidonline_get_dispatcher = function(name) {
        function(x, from, to, provider, ..., quiet) {
          paste(name, x, from, to, provider, quiet, sep = "|")
        }
      },
      .scholidonline_run_binary_one = function(
        x,
        dispatcher,
        from,
        to,
        provider,
        ...,
        quiet
      ) {
        dispatcher(
          x = x,
          from = from,
          to = to,
          provider = provider,
          quiet = quiet,
          ...
        )
      }
    )
    
    out <- .scholidonline_run_binary(
      x = c("a", "b"),
      from = c("pmid", "pmcid"),
      to = "doi",
      provider = "auto",
      quiet = FALSE
    )
    
    testthat::expect_length(out, 2)
    testthat::expect_match(
      out[[1]],
      "^\\.dispatch_pmid_to_doi\\|a\\|pmid\\|doi\\|stub\\|FALSE$"
    )
    testthat::expect_match(
      out[[2]],
      "^\\.dispatch_pmcid_to_doi\\|b\\|pmcid\\|doi\\|stub\\|FALSE$"
    )
  }
)


testthat::test_that(
  ".scholidonline_run_binary() errors on invalid from length",
  {
    testthat::expect_error(
      .scholidonline_run_binary(
        x = c("a", "b"),
        from = c("pmid", "pmcid", "doi"),
        to = "doi",
        provider = "auto",
        quiet = TRUE
      ),
      "`from` must have length 1 or length\\(x\\)\\."
    )
  }
)


testthat::test_that(
  ".scholidonline_run_binary() returns NA for NA x without dispatch",
  {
    called <- FALSE
    
    testthat::local_mocked_bindings(
      .scholidonline_binary_identity = function(from, to) {
        called <<- TRUE
        FALSE
      },
      .scholidonline_get_binary_meta = function(from, to) {
        called <<- TRUE
        list(
          providers = c("auto", "stub"),
          default_provider = "stub",
          dispatcher = ".stub"
        )
      },
      .scholidonline_resolve_binary_provider = function(provider, meta) {
        called <<- TRUE
        "stub"
      },
      .scholidonline_get_dispatcher = function(name) {
        called <<- TRUE
        function(x, from, to, provider, ..., quiet) "x"
      },
      .scholidonline_run_binary_one = function(
        x,
        dispatcher,
        from,
        to,
        provider,
        ...,
        quiet
      ) {
        called <<- TRUE
        "x"
      }
    )
    
    out <- .scholidonline_run_binary(
      x = NA_character_,
      from = "pmid",
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
    testthat::expect_false(called)
  }
)


testthat::test_that(
  ".scholidonline_run_binary() returns NA for NA from without dispatch",
  {
    called <- FALSE
    
    testthat::local_mocked_bindings(
      .scholidonline_binary_identity = function(from, to) {
        called <<- TRUE
        FALSE
      },
      .scholidonline_get_binary_meta = function(from, to) {
        called <<- TRUE
        list(
          providers = c("auto", "stub"),
          default_provider = "stub",
          dispatcher = ".stub"
        )
      },
      .scholidonline_resolve_binary_provider = function(provider, meta) {
        called <<- TRUE
        "stub"
      },
      .scholidonline_get_dispatcher = function(name) {
        called <<- TRUE
        function(x, from, to, provider, ..., quiet) "x"
      },
      .scholidonline_run_binary_one = function(
        x,
        dispatcher,
        from,
        to,
        provider,
        ...,
        quiet
      ) {
        called <<- TRUE
        "x"
      }
    )
    
    out <- .scholidonline_run_binary(
      x = "12345",
      from = NA_character_,
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
    testthat::expect_false(called)
  }
)


testthat::test_that(
  ".scholidonline_run_binary() returns identity mapping without dispatch",
  {
    called <- FALSE
    
    testthat::local_mocked_bindings(
      .scholidonline_binary_identity = function(from, to) {
        TRUE
      },
      .scholidonline_get_binary_meta = function(from, to) {
        called <<- TRUE
        list(
          providers = c("auto", "stub"),
          default_provider = "stub",
          dispatcher = ".stub"
        )
      },
      .scholidonline_resolve_binary_provider = function(provider, meta) {
        called <<- TRUE
        "stub"
      },
      .scholidonline_get_dispatcher = function(name) {
        called <<- TRUE
        function(x, from, to, provider, ..., quiet) "x"
      },
      .scholidonline_run_binary_one = function(
        x,
        dispatcher,
        from,
        to,
        provider,
        ...,
        quiet
      ) {
        called <<- TRUE
        "x"
      }
    )
    
    out <- .scholidonline_run_binary(
      x = c("A", "B"),
      from = "doi",
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, c("A", "B"))
    testthat::expect_false(called)
  }
)


testthat::test_that(
  ".scholidonline_run_binary() handles zero-length input",
  {
    out <- .scholidonline_run_binary(
      x = character(),
      from = "pmid",
      to = "doi",
      provider = "auto",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, character())
  }
)