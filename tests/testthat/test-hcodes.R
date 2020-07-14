test_that("hcodes_ functions work as expected", {

  ### hcodes_str, basic
  hc_str <- hcodes_str(ne_ref, pattern = "^adm")
  expect_is(hc_str, "character")
  expect_equal(length(hc_str),  nrow(ne_ref))

  hc_str_sep <- hcodes_str(ne_ref, pattern = "^adm", sep = "__SEP__")
  expect_true(any(grepl("__SEP__", hc_str_sep)))


  ### hcodes_int, basic
  hc_int <- hcodes_int(ne_ref, pattern = "^adm")
  expect_is(hc_int, "character")
  expect_equal(length(hc_int),  nrow(ne_ref))

  hc_int_pre <- hcodes_int(ne_ref, pattern = "^adm", prefix = "BLAH_")
  expect_true(all(grepl("^BLAH", hc_int_pre)))


  ### single hierarchical column
  x_onecol <- data.frame(x = letters)

  hc_int_onecol <- hcodes_int(x_onecol, by = "x")
  hc_str_onecol <- hcodes_str(x_onecol, by = "x")

  expect_equal(hc_int_onecol, formatC(seq_along(letters), width = 2, flag = "0"))
  expect_equal(hc_str_onecol, letters)


  ### large random ref
  set.seed(40692875)
  ref_large_compress <- data.frame(
    x1 = replicate(10, paste(sample(letters, 5), collapse = "")),
    x2 = replicate(100, paste(sample(letters, 5), collapse = "")),
    x3 = replicate(1000, paste(sample(letters, 5), collapse = "")),
    stringsAsFactors = FALSE
  )

  ref_large <- ref_expand(ref_large_compress, pattern = "^x")

  hc_int_large <- hcodes_int(ref_large, pattern = "^x")
  hc_str_large <- hcodes_str(ref_large, pattern = "^x")

  expect_equal(length(unique(nchar(hc_int_large))), 1L)
  expect_equal(length(unique(hc_str_large)), nrow(ref_large))
})

