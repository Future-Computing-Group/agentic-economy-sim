# The software citation names both archive identifiers.

test_that("the citation file parses and carries the concept and version DOIs", {
  cff <- yaml::read_yaml(here::here("CITATION.cff"))
  expect_equal(cff$doi, "10.5281/zenodo.21041130")
  ids <- cff$identifiers
  expect_true(any(vapply(ids, function(i)
    identical(i$type, "doi") && identical(i$value, "10.5281/zenodo.22876728") &&
      identical(i$description, "Version DOI of v2.1.0"), logical(1))))
  expect_equal(cff$version, "v2.1.0")
})
