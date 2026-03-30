testthat::test_that(
  "live dispatcher conversions work for a known NCBI article",
  {
    skip_if_no_internet_for_live_tests()
    
    pmcid <- "PMC2808187"
    
    links <- .links_pmcid_ncbi(pmcid, quiet = TRUE)
    
    testthat::skip_if_not(
      nrow(links) > 0L,
      message = "NCBI fixture article not available."
    )
    
    pmid <- links$linked_value[links$linked_type == "pmid"][[1]]
    doi <- links$linked_value[links$linked_type == "doi"][[1]]
    
    testthat::skip_if_not(
      is.character(pmid) && length(pmid) == 1L && nzchar(pmid),
      message = "Fixture PMID unavailable."
    )
    
    testthat::skip_if_not(
      is.character(doi) && length(doi) == 1L && nzchar(doi),
      message = "Fixture DOI unavailable."
    )
    
    testthat::expect_equal(
      .convert_pmid_to_doi(
        x = pmid,
        from = "pmid",
        to = "doi",
        provider = "auto",
        quiet = TRUE
      ),
      doi
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmid(
        x = doi,
        from = "doi",
        to = "pmid",
        provider = "auto",
        quiet = TRUE
      ),
      pmid
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_pmid(
        x = pmcid,
        from = "pmcid",
        to = "pmid",
        provider = "auto",
        quiet = TRUE
      ),
      pmid
    )
    
    testthat::expect_equal(
      .convert_pmcid_to_doi(
        x = pmcid,
        from = "pmcid",
        to = "doi",
        provider = "auto",
        quiet = TRUE
      ),
      doi
    )
    
    testthat::expect_equal(
      .convert_pmid_to_pmcid(
        x = pmid,
        from = "pmid",
        to = "pmcid",
        provider = "auto",
        quiet = TRUE
      ),
      pmcid
    )
    
    testthat::expect_equal(
      .convert_doi_to_pmcid(
        x = doi,
        from = "doi",
        to = "pmcid",
        provider = "auto",
        quiet = TRUE
      ),
      pmcid
    )
  }
)