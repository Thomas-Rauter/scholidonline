testthat::test_that(
  ".arxiv_extract_entry_blocks() handles invalid input",
  {
    testthat::expect_identical(
      .arxiv_extract_entry_blocks(NULL),
      character()
    )
    
    testthat::expect_identical(
      .arxiv_extract_entry_blocks(NA_character_),
      character()
    )
    
    testthat::expect_identical(
      .arxiv_extract_entry_blocks(""),
      character()
    )
    
    testthat::expect_identical(
      .arxiv_extract_entry_blocks("<feed></feed>"),
      character()
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_blocks() extracts XML entry blocks",
  {
    txt <- paste0(
      "<feed>",
      "<entry><id>https://arxiv.org/abs/0706.0001v1</id></entry>",
      "<entry><id>https://arxiv.org/abs/1503.07589v1</id></entry>",
      "</feed>"
    )
    
    out <- .arxiv_extract_entry_blocks(txt)
    
    testthat::expect_length(out, 2L)
    testthat::expect_true(
      grepl("0706.0001", out[[1L]], fixed = TRUE)
    )
    testthat::expect_true(
      grepl("1503.07589", out[[2L]], fixed = TRUE)
    )
  }
)

testthat::test_that(
  ".arxiv_extract_first_element_text() handles invalid input",
  {
    testthat::expect_identical(
      .arxiv_extract_first_element_text(NA_character_, "title"),
      NA_character_
    )
    
    testthat::expect_identical(
      .arxiv_extract_first_element_text("", "title"),
      NA_character_
    )
    
    testthat::expect_error(
      .arxiv_extract_first_element_text("<title>x</title>", NA_character_),
      "`tag` must be a single non-missing character string.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .arxiv_extract_first_element_text("<title>x</title>", ""),
      "`tag` must be a single non-missing character string.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".arxiv_extract_first_element_text() extracts and trims element text",
  {
    txt <- "<entry><title>  Elastic theory  </title></entry>"
    
    out <- .arxiv_extract_first_element_text(
      txt = txt,
      tag = "title"
    )
    
    testthat::expect_identical(
      out,
      "Elastic theory"
    )
  }
)

testthat::test_that(
  ".arxiv_extract_first_element_text() returns NA when tag is absent",
  {
    out <- .arxiv_extract_first_element_text(
      txt = "<entry><id>x</id></entry>",
      tag = "title"
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".arxiv_parse_meta_entry() skips non-arXiv entries",
  {
    out <- .arxiv_parse_meta_entry(
      "<entry><id>https://example.org/abs/0706.0001</id></entry>"
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".arxiv_parse_meta_entry() parses complete metadata entry",
  {
    entry <- paste0(
      "<entry>",
      "<id>https://arxiv.org/abs/hep-ex/0307015v1</id>",
      "<title>Multi-Electron Production at High Transverse Momenta</title>",
      "<published>2003-07-04T00:00:00Z</published>",
      "<arxiv:doi>10.1140/epjc/s2003-01326-x</arxiv:doi>",
      "</entry>"
    )
    
    out <- .arxiv_parse_meta_entry(entry)
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$arxiv_id,
      "hep-ex/0307015v1"
    )
    
    testthat::expect_true(
      grepl("Multi-Electron Production", out$title)
    )
    
    testthat::expect_identical(
      out$year,
      2003L
    )
    
    testthat::expect_identical(
      out$container,
      "arXiv"
    )
    
    testthat::expect_identical(
      out$doi,
      "10.1140/epjc/s2003-01326-x"
    )
    
    testthat::expect_identical(
      out$pmid,
      NA_character_
    )
    
    testthat::expect_identical(
      out$pmcid,
      NA_character_
    )
    
    testthat::expect_identical(
      out$url,
      "https://arxiv.org/abs/hep-ex/0307015v1"
    )
    
    testthat::expect_identical(
      out$provider,
      "arxiv"
    )
  }
)

testthat::test_that(
  ".arxiv_parse_meta_entry() handles missing year and DOI",
  {
    entry <- paste0(
      "<entry>",
      "<id>https://arxiv.org/abs/0706.0001v1</id>",
      "<title>Elastic theory</title>",
      "<published>not-a-date</published>",
      "</entry>"
    )
    
    out <- .arxiv_parse_meta_entry(entry)
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$year,
      NA_integer_
    )
    
    testthat::expect_identical(
      out$doi,
      NA_character_
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() rejects unused dots",
  {
    testthat::expect_error(
      .meta_arxiv_arxiv_batch(
        x = "0706.0001",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() rejects non-character input",
  {
    testthat::expect_error(
      .meta_arxiv_arxiv_batch(
        x = 706.0001,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() returns empty data.frame for no valid input",
  {
    out <- .meta_arxiv_arxiv_batch(
      x = c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() returns empty data.frame when query fails",
  {
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        NULL
      }
    )
    
    out <- .meta_arxiv_arxiv_batch(
      x = "0706.0001",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() returns empty data.frame without entries",
  {
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        "<feed></feed>"
      }
    )
    
    out <- .meta_arxiv_arxiv_batch(
      x = "0706.0001",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".meta_arxiv_arxiv_batch() parses and filters metadata entries",
  {
    txt <- paste0(
      "<feed>",
      "<entry>",
      "<id>https://arxiv.org/abs/0706.0001v1</id>",
      "<title>Elastic theory</title>",
      "<published>2007-06-01T00:00:00Z</published>",
      "</entry>",
      "<entry>",
      "<id>https://arxiv.org/abs/9999.9999v1</id>",
      "<title>Other article</title>",
      "<published>2099-01-01T00:00:00Z</published>",
      "</entry>",
      "</feed>"
    )
    
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        txt
      }
    )
    
    out <- .meta_arxiv_arxiv_batch(
      x = c("0706.0001", "1234.12345", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$arxiv_id,
      "0706.0001v1"
    )
    
    testthat::expect_identical(
      out$year,
      2007L
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() extracts arxiv:doi",
  {
    entry <- paste0(
      "<entry>",
      "<arxiv:doi>10.1140/epjc/s2003-01326-x</arxiv:doi>",
      "</entry>"
    )
    
    out <- .arxiv_extract_entry_doi(entry)
    
    testthat::expect_identical(
      out,
      "10.1140/epjc/s2003-01326-x"
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() extracts DOI from titled link",
  {
    entry <- paste0(
      "<entry>",
      "<link title=\"doi\" href=\"https://doi.org/10.1103/PhysRevLett.114.191803\" />",
      "</entry>"
    )
    
    out <- .arxiv_extract_entry_doi(entry)
    
    testthat::expect_identical(
      out,
      "10.1103/PhysRevLett.114.191803"
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() extracts DOI from doi.org href",
  {
    entry <- paste0(
      "<entry>",
      "<link href=\"https://dx.doi.org/10.1103/PhysRevLett.114.191803\" />",
      "</entry>"
    )
    
    out <- .arxiv_extract_entry_doi(entry)
    
    testthat::expect_identical(
      out,
      "10.1103/PhysRevLett.114.191803"
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() returns NA without DOI link",
  {
    out <- .arxiv_extract_entry_doi(
      "<entry><link href=\"https://arxiv.org/pdf/0706.0001\" /></entry>"
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() returns NA for DOI link without href",
  {
    out <- .arxiv_extract_entry_doi(
      "<entry><link title=\"doi\" /></entry>"
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".arxiv_extract_entry_doi() returns NA for empty DOI href",
  {
    out <- .arxiv_extract_entry_doi(
      "<entry><link title=\"doi\" href=\"https://doi.org/\" /></entry>"
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".arxiv_parse_links_entry() skips non-arXiv entries",
  {
    out <- .arxiv_parse_links_entry(
      "<entry><id>https://example.org/abs/0706.0001</id></entry>"
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".arxiv_parse_links_entry() skips entries without DOI",
  {
    entry <- paste0(
      "<entry>",
      "<id>https://arxiv.org/abs/0706.0001v1</id>",
      "</entry>"
    )
    
    out <- .arxiv_parse_links_entry(entry)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".arxiv_parse_links_entry() parses DOI links",
  {
    entry <- paste0(
      "<entry>",
      "<id>https://arxiv.org/abs/1503.07589v1</id>",
      "<arxiv:doi>10.1103/PhysRevLett.114.191803</arxiv:doi>",
      "</entry>"
    )
    
    out <- .arxiv_parse_links_entry(entry)
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$arxiv_id,
      "1503.07589v1"
    )
    
    testthat::expect_identical(
      out$linked_type,
      "doi"
    )
    
    testthat::expect_identical(
      out$linked_value,
      "10.1103/PhysRevLett.114.191803"
    )
    
    testthat::expect_identical(
      out$provider,
      "arxiv"
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() rejects unused dots",
  {
    testthat::expect_error(
      .links_arxiv_arxiv_batch(
        x = "1503.07589",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() rejects non-character input",
  {
    testthat::expect_error(
      .links_arxiv_arxiv_batch(
        x = 1503.07589,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() returns empty data.frame for no valid input",
  {
    out <- .links_arxiv_arxiv_batch(
      x = c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() returns empty data.frame when query fails",
  {
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        NULL
      }
    )
    
    out <- .links_arxiv_arxiv_batch(
      x = "1503.07589",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() returns empty data.frame without entries",
  {
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        "<feed></feed>"
      }
    )
    
    out <- .links_arxiv_arxiv_batch(
      x = "1503.07589",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_arxiv_arxiv_batch() parses and filters linked DOI entries",
  {
    txt <- paste0(
      "<feed>",
      "<entry>",
      "<id>https://arxiv.org/abs/1503.07589v1</id>",
      "<arxiv:doi>10.1103/PhysRevLett.114.191803</arxiv:doi>",
      "</entry>",
      "<entry>",
      "<id>https://arxiv.org/abs/9999.9999v1</id>",
      "<arxiv:doi>10.0000/not-real</arxiv:doi>",
      "</entry>",
      "</feed>"
    )
    
    testthat::local_mocked_bindings(
      .arxiv_query_id_list = function(x, quiet = FALSE) {
        txt
      }
    )
    
    out <- .links_arxiv_arxiv_batch(
      x = c("1503.07589", "1234.12345", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$arxiv_id,
      "1503.07589v1"
    )
    
    testthat::expect_identical(
      out$linked_value,
      "10.1103/PhysRevLett.114.191803"
    )
  }
)