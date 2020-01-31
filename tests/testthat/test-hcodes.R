test_that("hcodes_ functions work as expected", {

  # hcodes_str
  hc_str <- hcodes_str(drc_ref, pattern = "^adm")
  expect_is(hc_str, "character")
  expect_equal(length(hc_str),  nrow(drc_ref))

  hc_str_sep <- hcodes_str(drc_ref, pattern = "^adm", sep = "__SEP__")
  expect_true(any(grepl("__SEP__", hc_str_sep)))


  # hcodes_int
  hc_int <- hcodes_int(drc_ref, pattern = "^adm")
  expect_is(hc_int, "character")
  expect_equal(length(hc_int),  nrow(drc_ref))

  hc_int_pre <- hcodes_int(drc_ref, pattern = "^adm", prefix = "BLAH_")
  expect_true(all(grepl("^BLAH", hc_int_pre)))
})

