test_that("hmatch_composite works as expected", {

  # test join types
  raw_types <- ne_raw[c(1, 10, 14),] # match, no-match, double-match
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

  ## composite hmatch (with manual)
  ne_man <- data.frame(
    adm0 = NA_character_,
    adm1 = NA_character_,
    adm2 = "Bergen, N.J.",
    hcode = "211",
    stringsAsFactors = FALSE
  )

  m_comp_man <- hmatch_composite(ne_raw, ne_ref, ne_man, code_col = "hcode", fuzzy = TRUE)
  expect_true("manual" %in% m_comp_man$match_type)

  ## composite hmatch (all match exactly)
  m_comp_exact <- hmatch_composite(ne_ref, ne_ref, pattern = "^adm", fuzzy = TRUE)
  expect_equal(nrow(m_comp_exact), nrow(ne_ref))
  expect_true(all(m_comp_exact$match_type == "complete"))

  # test retains class
  m_tibble <- hmatch_composite(dplyr::as_tibble(ne_raw), ne_ref)
  expect_is(m_tibble, "tbl_df")
})

