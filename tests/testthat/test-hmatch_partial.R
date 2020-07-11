test_that("hmatch_partial works as expected", {

  # test join types
  raw_types <- ne_raw[c(1, 8, 10),] # match, no-match, double-match
  raw_types$id <- 1:3

  m_left <- hmatch_partial(raw_types, ne_ref, type = "left")
  expect_equal(sort(m_left$id), c(1, 2, 3, 3))

  m_inner <- hmatch_partial(raw_types, ne_ref, type = "inner")
  expect_equal(sort(m_inner$id), c(1, 3, 3))

  m_inner_unique <- hmatch_partial(raw_types, ne_ref, type = "inner_unique")
  expect_equal(sort(m_inner_unique$id), 1)

  m_anti <- hmatch_partial(raw_types, ne_ref, type = "anti")
  expect_equal(sort(m_anti$id), c(2))

  m_anti_unique <- hmatch_partial(raw_types, ne_ref, type = "anti_unique")
  expect_equal(sort(m_anti_unique$id), c(2, 3))

  # test 1-column matching
  raw_onecol <- data.frame(x = c("x", "y", "z"), stringsAsFactors = FALSE)
  ref_onecol <- raw_onecol

  m_onecol <- hmatch_partial(raw_onecol, ref_onecol)
  expect_named(m_onecol, c("x", "ref_x"))
  expect_equal(m_onecol$x, m_onecol$ref_x)

  # test max_dist in maxdist matching
  raw_maxdist <- data.frame(x = c("Patrick12", "Patrick123"), stringsAsFactors = FALSE)
  ref_maxdist <- data.frame(x = "Patrick", stringsAsFactors = FALSE)

  m_maxdist1 <- hmatch_partial(raw_maxdist, ref_maxdist, fuzzy = TRUE, max_dist = 1, type = "inner")
  m_maxdist2 <- hmatch_partial(raw_maxdist, ref_maxdist, fuzzy = TRUE, max_dist = 2, type = "inner")
  m_maxdist3 <- hmatch_partial(raw_maxdist, ref_maxdist, fuzzy = TRUE, max_dist = 3, type = "inner")

  expect_equal(nrow(m_maxdist1), 0L)
  expect_equal(nrow(m_maxdist2), 1L)
  expect_equal(nrow(m_maxdist3), 2L)

  # test dictionary-based recoding
  ne_dict <- data.frame(
    value = "USA",
    replacement = "United States",
    variable = "adm0",
    stringsAsFactors = FALSE
  )

  dat_raw <- ne_raw[c(3, 4, 8),] # match, dict-match, no-match
  dat_raw$id <- 1:3

  m_dict1 <- hmatch_partial(dat_raw, ne_ref, type = "inner")
  m_dict2 <- hmatch_partial(dat_raw, ne_ref, dict = ne_dict, type = "inner")

  expect_equal(m_dict1$id, c(1))
  expect_equal(m_dict2$id, c(1, 2))

  # test retains class
  ne_raw_tibble <- dplyr::as_tibble(ne_raw)
  m_tibble <- hmatch_partial(ne_raw_tibble, ne_ref)
  expect_is(ne_raw_tibble, "tbl_df")
})

