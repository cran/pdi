#' Make analysis table
#' @description prepare data table ready for random forest analysis
#' @param phenoData tibble containing phenotype data
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
#' @importFrom purrr map_dfc
#' @export

makeAnalysisTable <- function(phenoData){
  analysisTable <- phenoData %>%
    select(-Location,-ID,-GPS,-Status,-`Tree No`)
  
  if ('ChosenGroup' %in% names(analysisTable)) {
    analysisTable <- analysisTable %>%
      select(-ChosenGroup)
  }
  
  analysisTable[,{map_dfc(analysisTable,is.character) %>% as.logical()}] <- analysisTable[, {map_dfc(analysisTable,is.character) %>% as.logical()}] %>%
    map_dfc(factor)
  
  return(analysisTable)
}