epmc_live_fixture <- function() {
  skip_if_no_internet_for_live_tests()
  
  pmcid <- "PMC2808187"
  out <- .meta_pmcid_epmc(pmcid, quiet = TRUE)
  
  testthat::skip_if_not(
    nrow(out) == 1L,
    message = "Europe PMC fixture article not available."
  )
  
  pmid <- out$pmid[[1]]
  doi <- out$doi[[1]]
  
  testthat::skip_if_not(
    is.character(pmid) && length(pmid) == 1L &&
      !is.na(pmid) && nzchar(pmid),
    message = "Fixture PMID unavailable."
  )
  
  testthat::skip_if_not(
    is.character(doi) && length(doi) == 1L &&
      !is.na(doi) && nzchar(doi),
    message = "Fixture DOI unavailable."
  )
  
  list(
    pmcid = pmcid,
    pmid = pmid,
    doi = doi
  )
}


testthat::test_that(".meta_pmcid_epmc returns expected live metadata", {
  ids <- epmc_live_fixture()
  
  out <- .meta_pmcid_epmc(ids$pmcid, quiet = TRUE)
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_true(nzchar(out$title))
  testthat::expect_true(!is.na(out$year))
  testthat::expect_true(is.numeric(out$year) || is.integer(out$year))
  testthat::expect_equal(out$pmcid, ids$pmcid)
  testthat::expect_true(is.na(out$pmid) || identical(out$pmid, ids$pmid))
  testthat::expect_true(is.na(out$doi) || identical(out$doi, ids$doi))
  testthat::expect_match(out$url, "europepmc.org/article/PMC/")
  testthat::expect_equal(out$provider, "epmc")
})


testthat::test_that(".convert_*_epmc work for a known live article", {
  ids <- epmc_live_fixture()
  
  testthat::expect_equal(
    .convert_pmid_to_doi_epmc(ids$pmid, quiet = TRUE),
    ids$doi
  )
  
  testthat::expect_equal(
    .convert_doi_to_pmid_epmc(ids$doi, quiet = TRUE),
    ids$pmid
  )
  
  testthat::expect_equal(
    .convert_pmcid_to_pmid_epmc(ids$pmcid, quiet = TRUE),
    ids$pmid
  )
  
  testthat::expect_equal(
    .convert_pmcid_to_doi_epmc(ids$pmcid, quiet = TRUE),
    ids$doi
  )
  
  testthat::expect_equal(
    .convert_pmid_to_pmcid_epmc(ids$pmid, quiet = TRUE),
    ids$pmcid
  )
  
  testthat::expect_equal(
    .convert_doi_to_pmcid_epmc(ids$doi, quiet = TRUE),
    ids$pmcid
  )
})