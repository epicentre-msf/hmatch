test_that("ref_expand works as expected", {

  # subset example reference df to the admin-2 level
  ne_ref_adm2 <- ne_ref[!is.na(ne_ref$adm2),]

  # expand back to all levels
  ne_ref_recover1 <- ref_expand(ne_ref_adm2, pattern = "adm", lowest_level = 0L)

  expect_equal(nrow(ne_ref_recover1), nrow(ne_ref))
  expect_true(min(ne_ref_recover1$level) == 0L)


  # subset example reference df to the admin-0 and admin-2 levels
  ne_ref_adm02 <- ne_ref[is.na(ne_ref$adm0) | !is.na(ne_ref$adm2),]

  # expand back to all levels
  ne_ref_recover2 <- ref_expand(ne_ref_adm02, pattern = "adm", lowest_level = 0L)

  expect_equal(nrow(ne_ref_recover2), nrow(ne_ref))
  expect_true(min(ne_ref_recover2$level) == 0L)
})
