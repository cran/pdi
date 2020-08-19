
context('minMaxScale')

test_that('minMaxScale works',{
  s <- seq(0,10,length.out = 11)
  s <- minMaxScale(s)
  
  expect_equal(s,seq(0,1,length.out = 11))
})