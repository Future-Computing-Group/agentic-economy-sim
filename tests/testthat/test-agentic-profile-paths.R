# The tracked profile files are named relative to the project root, so a store
# built on one machine is current on another: a file target that records an
# absolute path reads as missing everywhere but the host that wrote it.

test_that("the tracked profile paths are relative and resolve from the project root", {
  rel <- agentic_profile_files()
  expect_length(rel, 2)
  expect_false(any(grepl("^/", rel)))
  expect_true(all(file.exists(here::here(rel))))
  expect_identical(agentic_profile_path(), here::here(rel[1]))
  expect_identical(agentic_profile_paths(), here::here(rel))
})
