
context('calculate DIs')

files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'pdi'),full.names = TRUE) 
d <- map(files,readPhenotypeSheet)
p <- map(d,preparePhenotypeData) %>%
  bind_rows()
sc <- siteAdjustment(p)
a <- sc %>%
  mutate(`Live crown ratio (%)` = liveCrownRatio(`Total height (m)`,
                                                 `Lower crown height (m)`),
         `Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                `Crown transparency (%)`),
         `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                            `Total height (m)`,
                                            `Lower crown height (m)`,
                                            `Crown condition (%)`),
         `Bleed prevalence (%)` = bleedPrevalence(`Active bleed length (mm)`,
                                                  `Active bleeds`,
                                                  `Black staining length (mm)`,
                                                  `Black staining`,
                                                  `Diameter at breast height (m)`),
         `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (m)`)
  )
t <- makeAnalysisTable(a)
m <- rf(t,cls = NULL,nreps = 1)
DIs <- calcDIs(m)
descriptor_contributions <- descriptorContributions(m)

test_that('phenotype sheets parsed correctly',{
  files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'pdi'),full.names = TRUE) 
  d <- readPhenotypeSheet(files[[1]])
  
  expect_identical(class(d),'list')
  expect_equal(length(d),6)
})

test_that('phenotype data correctly prepared',{
  p <- preparePhenotypeData(d[[1]])
  
  expect_identical(class(p),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(p),36)
  expect_equal(nrow(p),20)
})

test_that('site correction works',{
  sc <- siteAdjustment(p)
  
  expect_identical(class(sc),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(sc),36)
  expect_equal(nrow(sc),40)
})

test_that('analysis table correctly constructed',{
  
  t <- a %>%
    mutate(ChosenGroup = NA) %>%
    makeAnalysisTable()
  
  expect_identical(class(t),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(t),36)
  expect_equal(nrow(t),40)
})

test_that('random forest works',{
  m <- rf(t,cls = NULL,nreps = 1)
  
  expect_identical(class(m),'list')
  expect_identical(class(m[[1]]),'randomForest')
})

test_that('DIs calculated correctly',{
  DIs <- calcDIs(m)
  
  expect_identical(class(DIs),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(DIs),2)
  expect_equal(nrow(DIs),40)
  expect_equal(round(mean(DIs$PDI),7),0.5345082)
  expect_equal(round(mean(DIs$DAI),8),-0.09114052)
  
  PDI <- calcDIs(m,DAI = FALSE)
  
  expect_identical(class(PDI),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(PDI),1)
  expect_equal(nrow(PDI),40)
  expect_equal(round(mean(PDI$PDI),7),0.5345082)
  
  DAI <- calcDIs(m,PDI = FALSE)
  
  expect_identical(class(DAI),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(DAI),1)
  expect_equal(nrow(DAI),40)
  expect_equal(round(mean(DAI$DAI),8),-0.09114052)
})

test_that('descriptor contributions calculated correctly',{
  descriptor_contributions <- descriptorContributions(m)
  
  expect_equal(ncol(descriptor_contributions),5)
  expect_equal(nrow(descriptor_contributions),36)
})