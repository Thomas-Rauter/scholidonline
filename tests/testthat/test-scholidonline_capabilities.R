testthat::test_that("scholidonline_capabilities returns registry summary", {
  out <- scholidonline_capabilities()
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(nrow(out) > 0L)
  
  testthat::expect_equal(
    names(out),
    c("type", "operation", "target", "providers", "default_provider")
  )
  
  testthat::expect_true(all(
    c("arxiv", "doi", "orcid", "pmcid", "pmid") %in% out$type
  ))
  
  testthat::expect_true(all(
    c("exists", "links", "meta", "convert") %in% out$operation
  ))
  
  testthat::expect_true(all(
    !is.na(out$default_provider) & nzchar(out$default_provider)
  ))
  
  testthat::expect_true(all(
    !is.na(out$providers) & nzchar(out$providers)
  ))
  
  testthat::expect_true(all(
    is.na(out$target[out$operation != "convert"])
  ))
  
  testthat::expect_true(all(
    !is.na(out$target[out$operation == "convert"])
  ))
  
  pmid_convert <- subset(
    out,
    type == "pmid" & operation == "convert" & target == "doi"
  )
  
  testthat::expect_equal(nrow(pmid_convert), 1L)
  testthat::expect_equal(pmid_convert$default_provider, "ncbi")
  testthat::expect_match(pmid_convert$providers, "ncbi")
  testthat::expect_match(pmid_convert$providers, "epmc")
  
  doi_meta <- subset(
    out,
    type == "doi" & operation == "meta"
  )
  
  testthat::expect_equal(nrow(doi_meta), 1L)
  testthat::expect_equal(doi_meta$default_provider, "crossref")
})