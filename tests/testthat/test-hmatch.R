test_that("hmatch works as expected", {

  # test join types
  raw_types <- ne_raw[c(1, 8, 11),] # match, no-match, double-match
  raw_types$id <- 1:3

  m_left <- hmatch(raw_types, ne_ref, type = "left")
  expect_equal(sort(m_left$id), c(1, 2, 3, 3))

  m_inner <- hmatch(raw_types, ne_ref, type = "inner")
  expect_equal(sort(m_inner$id), c(1, 3, 3))

  m_anti <- hmatch(raw_types, ne_ref, type = "anti")
  expect_equal(sort(m_anti$id), c(2))

  m_resolve_inner <- hmatch(raw_types, ne_ref, type = "resolve_inner")
  expect_equal(sort(m_resolve_inner$id), 1)

  m_resolve_anti <- hmatch(raw_types, ne_ref, type = "resolve_anti")
  expect_equal(sort(m_resolve_anti$id), c(2, 3))

  # test 1-column matching
  raw_onecol <- data.frame(x = c("x", "y", "z", NA), stringsAsFactors = FALSE)
  ref_onecol <- raw_onecol

  m_onecol <- hmatch(raw_onecol, ref_onecol)
  expect_named(m_onecol, c("x", "ref_x"))
  expect_equal(m_onecol$x, m_onecol$ref_x)

  # test 0 rows
  m_regular <- hmatch(ne_raw, ne_ref, type = "resolve_left")

  m_norows_raw <- hmatch(ne_raw[0,], ne_ref)
  expect_equal(names(m_norows_raw), names(m_regular))
  expect_equal(nrow(m_norows_raw), 0L)

  m_norows_ref <- hmatch(ne_raw, ne_ref[0,])
  expect_equal(names(m_norows_ref), names(m_regular))
  expect_equal(nrow(m_norows_ref), nrow(m_regular))

  # test fuzzy_dist argument in fuzzy matching
  raw_fuzzy_dist <- data.frame(x = c("Patrick12", "Patrick123"), stringsAsFactors = FALSE)
  ref_fuzzy_dist <- data.frame(x = "Patrick", stringsAsFactors = FALSE)

  m_fuzzy_dist1 <- hmatch(raw_fuzzy_dist, ref_fuzzy_dist, fuzzy = TRUE, fuzzy_dist = 1, type = "inner")
  m_fuzzy_dist2 <- hmatch(raw_fuzzy_dist, ref_fuzzy_dist, fuzzy = TRUE, fuzzy_dist = 2, type = "inner")
  m_fuzzy_dist3 <- hmatch(raw_fuzzy_dist, ref_fuzzy_dist, fuzzy = TRUE, fuzzy_dist = 3, type = "inner")

  expect_equal(nrow(m_fuzzy_dist1), 0L)
  expect_equal(nrow(m_fuzzy_dist2), 1L)
  expect_equal(nrow(m_fuzzy_dist3), 2L)

  # test dictionary-based recoding
  ne_dict <- data.frame(
    value = "USA",
    replacement = "United States",
    variable = "adm0",
    stringsAsFactors = FALSE
  )

  dat_raw <- ne_raw[c(3, 4, 8),] # match, dict-match, no-match
  dat_raw$id <- 1:3

  m_dict1 <- hmatch(dat_raw, ne_ref, type = "inner")
  m_dict2 <- hmatch(dat_raw, ne_ref, dict = ne_dict, type = "inner")

  expect_equal(m_dict1$id, c(1))
  expect_equal(m_dict2$id, c(1, 2))

  # test retains class
  ne_raw_tibble <- dplyr::as_tibble(ne_raw)
  m_tibble <- hmatch(ne_raw_tibble, ne_ref)
  expect_is(ne_raw_tibble, "tbl_df")
})

