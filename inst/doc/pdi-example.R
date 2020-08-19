## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = 'center'
)

## ----library_load,message=FALSE-----------------------------------------------
library(pdi)

## ----additional_libraries,message=FALSE---------------------------------------
library(dplyr)
library(ggplot2)

## ----file_paths---------------------------------------------------------------
files <- list.files(
  system.file('phenotypeDataCollectionSheets',
              package = 'pdi'),
  full.names = TRUE) 

## ----template,eval=FALSE------------------------------------------------------
#  phenotypingTemplate(path = '.')

## ----read_files---------------------------------------------------------------
d <- map(files,readPhenotypeSheet)

## ----prepare_data-------------------------------------------------------------
p <- map(d,preparePhenotypeData) %>%
  bind_rows()

## ----data---------------------------------------------------------------------
print(p)

## ----DBH_site_plot------------------------------------------------------------
ggplot(p,aes(x = Location,y = `Diameter at breast height (m)`)) +
  geom_boxplot() +
  theme_bw()

## ----site_adjustment----------------------------------------------------------
sa <- siteAdjustment(p)

## ----DBH_site_adjusted_plot---------------------------------------------------
ggplot(sa,aes(x = Location,y = `Diameter at breast height (m)`)) +
  geom_boxplot() +
  theme_bw()

## -----------------------------------------------------------------------------
sa_factors <- siteAdjustmentFactors(p)
sa_factors

## ----additional_descriptors---------------------------------------------------
a <- sa %>%
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
         `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,
                                                                     `Diameter at breast height (m)`)
  )

## ----prepare_data_for_rf------------------------------------------------------
t <- makeAnalysisTable(a)

## ----random_forest------------------------------------------------------------
m <- rf(t,cls = NULL,nreps = 10)

## ----calculate_DIs------------------------------------------------------------
DIs <- calcDIs(m,DAI = FALSE,invertPDI = FALSE) %>%
  bind_cols(a %>%
              select(Location,ID,Status))

## ----fig.width=7--------------------------------------------------------------
ggplot(DIs,aes(x = Status,y = PDI)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~Location)

## ----contributions------------------------------------------------------------
descriptor_contributions <- m %>%
  descriptorContributions()

## ----contributions_plot, fig.height=6,fig.width=5-----------------------------
descriptor_contributions %>%
  arrange(MeanDecreaseAccuracy) %>%
  mutate(Descriptor = factor(Descriptor,levels = Descriptor)) %>%
  ggplot(aes(x = MeanDecreaseAccuracy,y = Descriptor)) +
  geom_point() +
  theme_bw()

## ----crown_volume_plot--------------------------------------------------------
DIs %>%
  bind_cols(a %>%
              select(`Crown volume (m^3)`)) %>%
  ggplot(aes(x = PDI,y = `Crown volume (m^3)`)) +
  geom_point() +
  theme_bw()

