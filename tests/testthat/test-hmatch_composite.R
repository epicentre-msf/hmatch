test_that("hmatch_composite works as expected", {

  # test join types
  raw_types <- ne_raw[c(1, 8, 11),] # match, no-match, double-match
  raw_types$id <- 1:3

  m_resolve_left <- hmatch_composite(raw_types, ne_ref, type = "resolve_left")
  expect_equal(m_resolve_left$id, c(1, 2, 3))

  m_resolve_inner <- hmatch_composite(raw_types, ne_ref, type = "resolve_inner")
  expect_equal(sort(m_resolve_inner$id), 1)

  m_resolve_anti <- hmatch_composite(raw_types, ne_ref, type = "resolve_anti")
  expect_equal(sort(m_resolve_anti$id), c(2, 3))

  # test 1-column matching
  raw_onecol <- data.frame(x = c("x", "y", "z", NA), stringsAsFactors = FALSE)
  ref_onecol <- raw_onecol

  m_onecol <- hmatch_composite(raw_onecol, ref_onecol)
  expect_named(m_onecol, c("x", "ref_x", "match_type"))
  expect_equal(m_onecol$x, m_onecol$ref_x)

  # test 0 rows
  m_regular <- hmatch_composite(ne_raw, ne_ref, type = "resolve_left")

  m_norows_raw <- hmatch_composite(ne_raw[0,], ne_ref)
  expect_equal(names(m_norows_raw), names(m_regular))
  expect_equal(nrow(m_norows_raw), 0L)

  m_norows_ref <- hmatch_composite(ne_raw, ne_ref[0,])
  expect_equal(names(m_norows_ref), names(m_regular))
  expect_equal(nrow(m_norows_ref), nrow(m_regular))

  # test retains class
  m_tibble <- hmatch_composite(dplyr::as_tibble(ne_raw), ne_ref)
  expect_is(m_tibble, "tbl_df")
})

