test_that("hmatch_split works as expected", {


  data(ne_raw)
  data(ne_ref)

  # basic
  m1 <- hmatch_split(ne_raw, ne_ref)
  expect_is(m1, "list")
  expect_length(m1, 3)

  # test arguments levels and always_list
  m2 <- hmatch_split(ne_raw, ne_ref, levels = 1:2)
  expect_length(m2, 2)
  expect_named(m2, c("adm0", "adm1"))

  m3 <- hmatch_split(ne_raw, ne_ref, levels = c("adm0", "adm2"))
  expect_length(m3, 2)
  expect_named(m3, c("adm0", "adm2"))

  m4 <- hmatch_split(ne_raw, ne_ref, levels = "adm0")
  expect_is(m4, "data.frame")

  m5 <- hmatch_split(ne_raw, ne_ref, levels = "adm0", always_list = TRUE)
  expect_is(m5, "list")
  expect_length(m5, 1)

  # test 1-column matching
  raw_onecol <- data.frame(x = c("x", "y", "z", NA), stringsAsFactors = FALSE)
  ref_onecol <- raw_onecol

  m_onecol_1 <- hmatch_split(raw = raw_onecol, ref = ref_onecol)

  expect_named(m_onecol_1, c("x", "ref_x"))
  expect_equal(m_onecol_1$x, m_onecol_1$ref_x)

  # test 0 rows
  m_regular <- hmatch_split(ne_raw, ne_ref[0,], type = "resolve_left")

  m_norows_raw <- hmatch_split(raw = ne_raw[0,], ref = ne_ref)
  expect_length(m_norows_raw, 3)
  expect_true(all(vapply(m_norows_raw, nrow, 0) == 0))

  m_norows_ref <- hmatch_split(ne_raw, ne_ref[0,])
  expect_length(m_norows_ref, 3)
  expect_equal(vapply(m_regular, nrow, 0), vapply(m_norows_ref, nrow, 0))

  # test with hmatch_composite
  m_comp_1 <- hmatch_split(ne_raw, ne_ref, fn = "hmatch_composite", type = "resolve_inner")
  expect_length(m_comp_1, 3)
  expect_true("match_type" %in% names(m_comp_1[[1]]))

  # test with hmatch_tokens
  m_tokens_1 <- hmatch_split(
    ne_raw,
    ne_ref,
    fn = "hmatch_tokens",
    levels = "adm2"
  )

  m_tokens_2 <- hmatch_split(
    ne_raw,
    ne_ref,
    fn = "hmatch_tokens",
    exclude_values = "Bergen",
    levels = "adm2"
  )

  expect_true("Bergen" %in% m_tokens_1$ref_adm2)
  expect_true(!"Bergen" %in% m_tokens_2$ref_adm2)

})

