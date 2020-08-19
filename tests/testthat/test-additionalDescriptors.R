
context('additional descriptors')

m <- 50
t <- 50
r <- 7.525
h <- 27.5
l <- 8.1
c <- crownCondition(m,t)
a <- 10
A <- 1
b <- 10
B <- 1
d <- 1.224
n <- 20
v <- crownVolume(r,h,l,c)
sa <- crownSurfaceArea(r,h,l,c)

test_that('crown condition is calculated correctly',{
  cc <- crownCondition(m,t)
  expect_equal(cc,25)
})

test_that('crown volume is calculated correctly',{
  v <- crownVolume(r,h,l,c)
  expect_equal(round(v,4),431.3945)
})

test_that('bleed prevalence is calculated correctly',{
  p <- bleedPrevalence(a,A,b,B,d)
  expect_equal(round(p,9),0.001733714)
})

test_that('Agrilus exit hole density is calculated correctly',{
  d <- agrilusExitHoleDensity(n,d)
  expect_equal(round(d,6),2.600571)
})

test_that('live crown ratio is calculated correctly',{
  lr <- liveCrownRatio(h,l)
  expect_equal(round(lr,5),70.54545)
})

test_that('crown surface area is calculated correctly',{
  sa <- crownSurfaceArea(r,h,l,c)
  expect_equal(round(sa,4),160.4659)
})

test_that('crown production efficiency is calculated correctly',{
  pe <- crownProductionEfficiency(sa,v)
  expect_equal(round(pe,7),0.3719702)
})
