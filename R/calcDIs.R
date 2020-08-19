#' Calculate Decline Indexes
#' @description Calculate Phenotypic Decline Index (PDI) and Decline Acuteness Index (DAI).
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @param PDI TRUE/FALSE, calculate PDI?
#' @param DAI TRUE/FALSE, calculate DAI?
#' @param invertPDI invert the PDI scale? TRUE/FALSE. Ignored if argument PDI is FALSE
#' @param invertDAI invert the DAI scale? TRUE/FALSE. Ignored if argument DAI is FALSE
#' @examples 
#' library(dplyr)
#' 
#' ## Retrieve file paths for example data
#' files <- list.files(system.file('phenotypeDataCollectionSheets',
#'   package = 'pdi'),full.names = TRUE)
#' 
#' ## Prepare data
#' d <- map(files,readPhenotypeSheet) %>%
#'   map(preparePhenotypeData) %>%
#'   bind_rows() %>%
#'   siteAdjustment() %>%
#'    mutate(`Live crown ratio (%)` = liveCrownRatio(`Total height (m)`,
#'      `Lower crown height (m)`),
#'      `Crown condition (%)` = crownCondition(`Missing crown (%)`,
#'                                `Crown transparency (%)`),
#'      `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
#'                                `Total height (m)`,
#'                                `Lower crown height (m)`,
#'                                `Crown condition (%)`),
#'      `Bleed prevalence (%)` = bleedPrevalence(`Active bleed length (mm)`,
#'                                `Active bleeds`,
#'                                `Black staining length (mm)`,
#'                                `Black staining`,
#'                                `Diameter at breast height (m)`),
#'      `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,
#'                                `Diameter at breast height (m)`)
#' )
#' 
#' t <- makeAnalysisTable(d)
#' 
#' ## Generate random forest models
#' m <- rf(t,cls = NULL,nreps = 10)
#' 
#' ## Calculate decline indexese
#' DIs <- calcDIs(m,DAI = FALSE,invertPDI = FALSE) %>%
#'   bind_cols(d %>%
#'     select(Location,ID,Status))
#' @export

calcDIs <- function(rfModels, PDI = TRUE, DAI = TRUE, invertPDI = TRUE, invertDAI = TRUE){
  DIs <- rfModels %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = minMaxScale(DAI) %>%
             {2 * . - 1})
  
  if (isFALSE(PDI)) {
    DIs <- DIs %>%
      select(-PDI)
  } else {
    if (isTRUE(invertPDI)) {
      DIs <- DIs %>%
        mutate(PDI = PDI %>%
                 {(. - 1) * -1})
    }
  }
  
  if (isFALSE(DAI)) {
    DIs <- DIs %>%
      select(-DAI)
  } else {
    if (isTRUE(invertDAI)) {
      DIs <- DIs %>%
        mutate(DAI = DAI %>%
                 {. * -1})
    }
  }
  
  return(DIs)
}