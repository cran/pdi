context('phenotypingTemplate')

test_that("phenotypingTemplate works", {
  temp <- tempdir()
  phenotypingTemplate(path = temp)
  
  expect_true(file.exists(str_c(temp,'phenotyping_template.xlsx',sep = '/')))
})
