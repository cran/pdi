#' Site adjustment factors
#' @description Return site adjustment factors of selected phenotypic descriptors.
#' @param phenoData phenoData tibble containing phenotype data
#' @param descriptors columns of phenoData on which calculate site correction factors
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
#'   siteAdjustment()
#'
#' sa_factors <- siteAdjustmentFactors(d)
#' @export

siteAdjustmentFactors <- function(phenoData,descriptors = c("Diameter at breast height (m)",
                                                            "Lower crown height (m)",
                                                            "Timber height (m)",
                                                            "Total height (m)",
                                                            "Crown radius (m)")){
  
  siteCorrect <- phenoData %>%
    select(Location,ID,descriptors) %>%
    gather('Descriptor','Value',-Location,-ID)
  
  overallMeans <- siteCorrect %>%
    group_by(Descriptor) %>%
    summarise(Mean = mean(Value))
  
  siteCorrections <- siteCorrect %>%
    group_by(Location,Descriptor) %>%
    summarise(Mean = mean(Value)) %>%
    ungroup() %>%
    split(.$Descriptor) %>%
    map(~{
      d <- .
      d %>%
        mutate(Adjustment = Mean - ({overallMeans %>% filter(Descriptor == d$Descriptor[1]) %>% .$Mean}))
    }) %>%
    bind_rows() %>%
    select(Descriptor,Location,Mean,Adjustment)
  return(siteCorrections)
}
