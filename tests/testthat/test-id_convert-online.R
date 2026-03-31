testthat::test_that("id_convert() converts PMID to DOI online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "31452104",
    to = "doi",
    from = "pmid"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_false(is.na(out))
  testthat::expect_true(grepl("^10\\.", out))
})


testthat::test_that("id_convert() converts DOI to PMID online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "10.1038/nature12373",
    to = "pmid",
    from = "doi"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_false(is.na(out))
  testthat::expect_true(grepl("^[0-9]+$", out))
})


testthat::test_that("id_convert() converts PMCID to DOI online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "PMC6821181",
    to = "doi",
    from = "pmcid"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_false(is.na(out))
  testthat::expect_true(grepl("^10\\.", out))
})


testthat::test_that("id_convert() converts PMCID to PMID online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "PMC6821181",
    to = "pmid",
    from = "pmcid"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_false(is.na(out))
  testthat::expect_true(grepl("^[0-9]+$", out))
})


testthat::test_that("id_convert() supports auto source detection online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = c(
      "31452104",
      "10.1038/nature12373",
      "PMC6821181"
    ),
    to = "doi"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 3L)
  testthat::expect_false(is.na(out[1]))
  testthat::expect_false(is.na(out[2]))
  testthat::expect_false(is.na(out[3]))
  testthat::expect_true(grepl("^10\\.", out[1]))
  testthat::expect_true(grepl("^10\\.", out[2]))
  testthat::expect_true(grepl("^10\\.", out[3]))
})


testthat::test_that("id_convert() supports provider selection online", {
  skip_if_no_internet_for_live_tests()
  
  testthat::expect_no_warning(
    out_epmc <- id_convert(
      x = "31452104",
      to = "doi",
      from = "pmid",
      provider = "epmc"
    )
  )
  
  ncbi_warning <- NULL
  
  out_ncbi <- withCallingHandlers(
    id_convert(
      x = "31452104",
      to = "doi",
      from = "pmid",
      provider = "ncbi"
    ),
    warning = function(w) {
      ncbi_warning <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  
  if (!is.null(ncbi_warning)) {
    testthat::skip(
      paste0(
        "Skipping NCBI live check due to warning: ",
        ncbi_warning
      )
    )
  }
  
  if (length(out_ncbi) != 1L || is.na(out_ncbi)) {
    testthat::skip("Skipping NCBI live check due to missing result.")
  }
  
  testthat::expect_type(out_ncbi, "character")
  testthat::expect_type(out_epmc, "character")
  testthat::expect_length(out_ncbi, 1L)
  testthat::expect_length(out_epmc, 1L)
  testthat::expect_false(is.na(out_ncbi))
  testthat::expect_false(is.na(out_epmc))
  testthat::expect_true(grepl("^10\\.", out_ncbi))
  testthat::expect_true(grepl("^10\\.", out_epmc))
})


testthat::test_that("id_convert() returns identity mapping online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "31452104",
    to = "pmid",
    from = "pmid"
  )
  
  testthat::expect_equal(out, "31452104")
})


testthat::test_that("id_convert() returns NA for invalid identifier online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = "not_an_identifier",
    to = "doi"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 1L)
  testthat::expect_true(is.na(out))
})


testthat::test_that("id_convert() preserves length for mixed inputs online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = c(
      "31452104",
      "not_an_identifier",
      "PMC6821181"
    ),
    to = "doi"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 3L)
  testthat::expect_false(is.na(out[1]))
  testthat::expect_true(is.na(out[2]))
  testthat::expect_false(is.na(out[3]))
})


testthat::test_that("id_convert() vectorizes online", {
  skip_if_no_internet_for_live_tests()
  
  
  out <- id_convert(
    x = c("31452104", "31437182"),
    to = "doi",
    from = "pmid"
  )
  
  testthat::expect_type(out, "character")
  testthat::expect_length(out, 2L)
  testthat::expect_false(is.na(out[1]))
})