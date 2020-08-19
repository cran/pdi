#' Site adjustment
#' @description Perform a site adjustment of selected descriptors.
#' @param phenoData phenoData tibble containing phenotype data
#' @param descriptors columns of phenoData on which to perform site correction
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
#' @export

siteAdjustment <- function(phenoData,descriptors = c("Diameter at breast height (m)",
                                                     "Lower crown height (m)",
                                                     "Timber height (m)",
                                                     "Total height (m)",
                                                     "Crown radius (m)")){
  siteCorrect <- phenoData %>%
    select(Location,ID,descriptors) %>%
    gather('Descriptor','Value',-Location,-ID)
  
  siteCorrections <- siteAdjustmentFactors(phenoData,descriptors)
  
  siteCorrect <- siteCorrect %>%
    split(.$Descriptor) %>%
    map(~{
      d <- .
      d %>%
        split(.$Location) %>%
        map(~{
          d <- .
          d %>%
            mutate(Value = Value - { siteCorrections %>%
                filter(Descriptor == d$Descriptor[1],Location == d$Location[1]) %>% .$Adjustment})
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    spread(Descriptor,Value)
  
  correctedPhenoData <- phenoData %>%
    select(-descriptors) %>%
    left_join(siteCorrect,by = c("Location", "ID"))
  
  return(correctedPhenoData)
}