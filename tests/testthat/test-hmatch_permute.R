test_that("hmatch_permute works as expected", {

  # # test join types
  # raw_types <- ne_raw[c(1, 8, 11),] # match, no-match, double-match
  # raw_types$id <- 1:3
  #
  # m_left <- hmatch_permute(raw_types, ne_ref, type = "left")
  # expect_equal(sort(m_left$id), c(1, 2, 3, 3))
  #
  # m_inner <- hmatch_permute(raw_types, ne_ref, type = "inner")
  # expect_equal(sort(m_inner$id), c(1, 3, 3))
  #
  # m_inner_unique <- hmatch_permute(raw_types, ne_ref, type = "inner_unique")
  # expect_equal(sort(m_inner_unique$id), 1)
  #
  # m_anti <- hmatch_permute(raw_types, ne_ref, type = "anti")
  # expect_equal(sort(m_anti$id), c(2))
  #
  # m_anti_unique <- hmatch_permute(raw_types, ne_ref, type = "anti_unique")
  # expect_equal(sort(m_anti_unique$id), c(2, 3))

  # test 1-column matching
  raw_onecol <- data.frame(x = c("x", "y", "z"), stringsAsFactors = FALSE)
  ref_onecol <- raw_onecol

  m_onecol <- hmatch_permute(raw_onecol, ref_onecol)
  expect_named(m_onecol, c("x", "ref_x"))
  expect_equal(m_onecol$x, m_onecol$ref_x)

  # test permutation
  ref_perm <- data.frame(
    ref_id = 1:4,
    x1 = c("a1", "b1", "a1", "b1"),
    x2 = c(NA  , NA  , "a2", "b2"),
    stringsAsFactors = FALSE
  )

  raw_perm <- data.frame(
    x1 = c("a2", "a1", "b2"),
    x2 = c("a1", NA  ,   NA),
    stringsAsFactors = FALSE
  )

  m_perm <- hmatch_permute(raw_perm, ref_perm)
  expect_equal(m_perm$ref_id, c(3, 1, 4))

  # test retains class
  ne_raw_tibble <- dplyr::as_tibble(ne_raw)
  m_tibble <- hmatch_permute(ne_raw_tibble, ne_ref)
  expect_is(ne_raw_tibble, "tbl_df")
})

