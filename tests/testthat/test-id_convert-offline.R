testthat::test_that("id_convert() rejects invalid x", {
  testthat::expect_error(
    id_convert(NULL, to = "doi"),
    regexp = "x"
  )
})


testthat::test_that("id_convert() rejects invalid to", {
  testthat::expect_error(
    id_convert("31452104", to = "banana"),
    regexp = "arg"
  )
})


testthat::test_that("id_convert() rejects invalid from", {
  testthat::expect_error(
    id_convert("31452104", to = "doi", from = "banana"),
    regexp = "arg"
  )
})


testthat::test_that("id_convert() rejects invalid provider", {
  testthat::expect_error(
    id_convert("31452104", to = "doi", provider = "banana"),
    regexp = "arg"
  )
})


testthat::test_that("id_convert() rejects invalid quiet", {
  testthat::expect_error(
    id_convert("31452104", to = "doi", quiet = "no"),
    regexp = "quiet"
  )
})


testthat::test_that("id_convert() rejects unsupported declared pairs", {
  testthat::expect_error(
    id_convert(
      x = "0000-0002-1825-0097",
      to = "doi",
      from = "orcid"
    ),
    regexp = "Unsupported conversion"
  )
})


testthat::test_that("id_convert() returns NA when no input is usable", {
  out <- id_convert(
    x = "not_an_identifier",
    to = "doi"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_true(is.na(out))
})


testthat::test_that("id_convert() uses explicit from and calls binary engine", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
    x,
    from,
    to,
    provider,
    ...,
    quiet
    ) {
      seen$x <- x
      seen$from <- from
      seen$to <- to
      seen$provider <- provider
      seen$quiet <- quiet
      
      c("10.1000/a", "10.1000/b")
    }
  )
  
  out <- id_convert(
    x = c("31452104", "31437182"),
    to = "doi",
    from = "pmid",
    provider = "ncbi",
    quiet = TRUE
  )
  
  testthat::expect_equal(
    seen$x,
    c("31452104", "31437182")
  )
  testthat::expect_equal(
    seen$from,
    c("pmid", "pmid")
  )
  testthat::expect_equal(seen$to, "doi")
  testthat::expect_equal(seen$provider, "ncbi")
  testthat::expect_true(isTRUE(seen$quiet))
  testthat::expect_equal(
    out,
    c("10.1000/a", "10.1000/b")
  )
})


testthat::test_that("id_convert() auto-detects source types per element", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
    x,
    from,
    to,
    provider,
    ...,
    quiet
    ) {
      seen$x <- x
      seen$from <- from
      seen$to <- to
      
      c("10.1000/a", "10.1000/b")
    }
  )
  
  out <- id_convert(
    x = c("31452104", "PMC6821181"),
    to = "doi"
  )
  
  testthat::expect_equal(
    seen$x,
    c("31452104", "PMC6821181")
  )
  testthat::expect_equal(
    seen$from,
    c("pmid", "pmcid")
  )
  testthat::expect_equal(seen$to, "doi")
  testthat::expect_equal(
    out,
    c("10.1000/a", "10.1000/b")
  )
})


testthat::test_that("id_convert() drops unsupported auto-detected pairs", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
      x,
      from,
      to,
      provider,
      ...,
      quiet
    ) {
      seen$x <- x
      seen$from <- from
      
      c("10.1000/a")
    }
  )
  
  out <- id_convert(
    x = c("0000-0002-1825-0097", "31452104"),
    to = "doi"
  )
  
  testthat::expect_equal(seen$x, "31452104")
  testthat::expect_equal(seen$from, "pmid")
  testthat::expect_true(is.na(out[1]))
  testthat::expect_equal(out[2], "10.1000/a")
})


testthat::test_that("id_convert() preserves input order", {
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
      x,
      from,
      to,
      provider,
      ...,
      quiet
    ) {
      paste0("converted::", x)
    }
  )
  
  out <- id_convert(
    x = c("31452104", "31437182", "31455877"),
    to = "doi",
    from = "pmid"
  )
  
  testthat::expect_equal(
    out,
    c(
      "converted::31452104",
      "converted::31437182",
      "converted::31455877"
    )
  )
})


testthat::test_that("id_convert() preserves NA for unusable mixed inputs", {
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
      x,
      from,
      to,
      provider,
      ...,
      quiet
    ) {
      rep("10.1000/mock", length(x))
    }
  )
  
  out <- id_convert(
    x = c("31452104", "not_an_identifier", "31437182"),
    to = "doi"
  )
  
  testthat::expect_equal(out[1], "10.1000/mock")
  testthat::expect_true(is.na(out[2]))
  testthat::expect_equal(out[3], "10.1000/mock")
})


testthat::test_that("id_convert() returns character vector of input length", {
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
      x,
      from,
      to,
      provider,
      ...,
      quiet
    ) {
      rep("X", length(x))
    }
  )
  
  out <- id_convert(
    x = c("31452104", "31437182"),
    to = "doi",
    from = "pmid"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 2L)
})


testthat::test_that("id_convert() passes provider auto through to engine", {
  seen <- new.env(parent = emptyenv())
  
  testthat::local_mocked_bindings(
    .scholidonline_run_binary = function(
      x,
      from,
      to,
      provider,
      ...,
      quiet
    ) {
      seen$provider <- provider
      rep("X", length(x))
    }
  )
  
  id_convert(
    x = "31452104",
    to = "doi",
    from = "pmid",
    provider = "auto"
  )
  
  testthat::expect_equal(seen$provider, "auto")
})