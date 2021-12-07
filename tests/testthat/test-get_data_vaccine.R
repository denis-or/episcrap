test_that("get data  works", {

  bd <- get_data_vaccine()
  testthat::expect_s3_class(bd, "tbl_df")
  testthat::expect_lt(nrow(bd), 6000)

})
