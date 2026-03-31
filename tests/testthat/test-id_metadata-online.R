testthat::test_that("id_metadata() retrieves DOI metadata online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "10.1038/nature12373",
    type = "doi"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "10.1038/nature12373")
  testthat::expect_equal(out$type, "doi")
  testthat::expect_false(is.na(out$provider))
  testthat::expect_false(is.na(out$title))
  testthat::expect_true(
    nzchar(out$title)
  )
})


testthat::test_that("id_metadata() retrieves PMID metadata online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "31452104",
    type = "pmid"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "31452104")
  testthat::expect_equal(out$type, "pmid")
  testthat::expect_false(is.na(out$provider))
  testthat::expect_false(is.na(out$title))
  testthat::expect_true(
    nzchar(out$title)
  )
})


testthat::test_that("id_metadata() retrieves PMCID metadata online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "PMC6821181",
    type = "pmcid"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "PMC6821181")
  testthat::expect_equal(out$type, "pmcid")
  testthat::expect_false(is.na(out$provider))
  testthat::expect_false(is.na(out$title))
  testthat::expect_true(
    nzchar(out$title)
  )
})


testthat::test_that("id_metadata() retrieves arXiv metadata online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "2101.00001",
    type = "arxiv",
    quiet = TRUE
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "2101.00001")
  testthat::expect_equal(out$type, "arxiv")
  
  if (!is.na(out$provider)) {
    testthat::expect_equal(out$provider, "arxiv")
  }
  
  if (!is.na(out$title)) {
    testthat::expect_true(
      nzchar(out$title)
    )
  }
})


testthat::test_that("id_metadata() retrieves ORCID metadata online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "0000-0002-1825-0097",
    type = "orcid"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "0000-0002-1825-0097")
  testthat::expect_equal(out$type, "orcid")
  testthat::expect_false(is.na(out$provider))
  testthat::expect_false(is.na(out$title))
  testthat::expect_true(
    nzchar(out$title)
  )
})


testthat::test_that("id_metadata() supports auto type detection online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(c(
    "10.1038/nature12373",
    "31452104",
    "PMC6821181",
    "2101.00001",
    "0000-0002-1825-0097"
  ),
  quiet = TRUE
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 5L)
  testthat::expect_equal(
    out$type,
    c("doi", "pmid", "pmcid", "arxiv", "orcid")
  )
  
  stable <- out$type %in% c("doi", "pmid", "pmcid", "orcid")
  
  testthat::expect_true(
    all(!is.na(out$provider[stable]))
  )
  testthat::expect_true(
    all(!is.na(out$title[stable]))
  )
  
  arxiv_row <- out$type == "arxiv"
  testthat::expect_equal(sum(arxiv_row), 1L)
})


testthat::test_that("id_metadata() supports provider selection online", {
  skip_if_no_internet_for_live_tests()
  
  out_crossref <- id_metadata(
    x = "10.1038/nature12373",
    type = "doi",
    provider = "crossref"
  )
  
  out_doiorg <- id_metadata(
    x = "10.1038/nature12373",
    type = "doi",
    provider = "doi.org"
  )
  
  out_ncbi <- id_metadata(
    x = "31452104",
    type = "pmid",
    provider = "ncbi"
  )
  
  out_epmc <- id_metadata(
    x = "31452104",
    type = "pmid",
    provider = "epmc"
  )
  
  testthat::expect_equal(out_crossref$provider, "crossref")
  testthat::expect_equal(out_doiorg$provider, "doi.org")
  testthat::expect_equal(out_ncbi$provider, "ncbi")
  testthat::expect_equal(out_epmc$provider, "epmc")
})


testthat::test_that("id_metadata() returns NA metadata for bad input online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata("not_an_identifier")
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$input, "not_an_identifier")
  testthat::expect_true(is.na(out$type))
  testthat::expect_true(is.na(out$provider))
  testthat::expect_true(is.na(out$title))
})


testthat::test_that("id_metadata() supports field selection online", {
  skip_if_no_internet_for_live_tests()
  
  out <- id_metadata(
    x = "10.1038/nature12373",
    type = "doi",
    fields = c("input", "title", "doi", "provider")
  )
  
  testthat::expect_equal(
    names(out),
    c("input", "title", "doi", "provider")
  )
  testthat::expect_equal(out$input, "10.1038/nature12373")
  testthat::expect_false(is.na(out$title))
  testthat::expect_false(is.na(out$doi))
  testthat::expect_false(is.na(out$provider))
})