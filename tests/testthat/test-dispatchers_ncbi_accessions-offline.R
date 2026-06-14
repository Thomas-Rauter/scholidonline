scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  "accession dispatchers use ncbi provider for auto",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .exists_bioproject_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PRJNA257197")
          TRUE
        },
        .exists_refseq_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "NM_000546.6")
          TRUE
        },
        .exists_sra_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "SRR1234567")
          TRUE
        },
        .exists_assembly_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "GCF_000001405.40")
          TRUE
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_true(
      .exists_bioproject("PRJNA257197", provider = "auto", quiet = TRUE)
    )
    testthat::expect_true(
      .exists_refseq("NM_000546.6", provider = "auto", quiet = TRUE)
    )
    testthat::expect_true(
      .exists_sra("SRR1234567", provider = "auto", quiet = TRUE)
    )
    testthat::expect_true(
      .exists_assembly("GCF_000001405.40", provider = "auto", quiet = TRUE)
    )
  }
)


testthat::test_that(
  "accession metadata dispatchers route to ncbi providers",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    do.call(
      testthat::local_mocked_bindings,
      list(
        .meta_bioproject_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "PRJNA257197")
          "bioproject"
        },
        .meta_refseq_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "NM_000546.6")
          "refseq"
        },
        .meta_sra_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "SRR1234567")
          "sra"
        },
        .meta_assembly_ncbi = function(x, ..., quiet = FALSE) {
          testthat::expect_identical(x, "GCF_000001405.40")
          "assembly"
        },
        .package = "scholidonline"
      )
    )

    testthat::expect_identical(
      .meta_bioproject("PRJNA257197", provider = "ncbi", quiet = TRUE),
      "bioproject"
    )
    testthat::expect_identical(
      .meta_refseq("NM_000546.6", provider = "ncbi", quiet = TRUE),
      "refseq"
    )
    testthat::expect_identical(
      .meta_sra("SRR1234567", provider = "ncbi", quiet = TRUE),
      "sra"
    )
    testthat::expect_identical(
      .meta_assembly("GCF_000001405.40", provider = "ncbi", quiet = TRUE),
      "assembly"
    )
  }
)


testthat::test_that(
  "NCBI accession existence dispatchers error on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    cases <- list(
      list(fn = .exists_bioproject, x = "PRJNA257197"),
      list(fn = .exists_refseq, x = "NM_000546.6"),
      list(fn = .exists_sra, x = "SRR1234567"),
      list(fn = .exists_assembly, x = "GCF_000001405.40")
    )

    for (case in cases) {
      testthat::expect_error(
        case$fn(
          x = case$x,
          provider = "crossref",
          quiet = TRUE
        ),
        "Unknown provider: crossref"
      )
    }
  }
)


testthat::test_that(
  "NCBI accession metadata dispatchers error on unknown provider",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )

    cases <- list(
      list(fn = .meta_bioproject, x = "PRJNA257197"),
      list(fn = .meta_refseq, x = "NM_000546.6"),
      list(fn = .meta_sra, x = "SRR1234567"),
      list(fn = .meta_assembly, x = "GCF_000001405.40")
    )

    for (case in cases) {
      testthat::expect_error(
        case$fn(
          x = case$x,
          provider = "crossref",
          quiet = TRUE
        ),
        "Unknown provider: crossref"
      )
    }
  }
)
