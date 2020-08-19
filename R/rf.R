#' Random forest analysis
#' @description Perform random forest repetitions.
#' @param analysisTable tibble of phenotype data suitable for random forest analysis as returned by \code{preparePhenotypeData}
#' @param cls analysisTable column to use as response vector. NULL for unsupervised analyses.
#' @param params additional arguments to pass to randomForest::randomForest
#' @param nreps number of repetitions
#' @param seed random number seed
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
#' @importFrom randomForest randomForest
#' @importFrom magrittr set_names
#' @export

rf <- function(analysisTable, cls, params = list(),nreps = 100, seed = 1234){
  set.seed(seed)

  map(seq(1,nreps),~{
    p <- formals(randomForest::randomForest)
    p$x <- analysisTable
    p$y <- cls
    p <- c(p,params,list(proximity = TRUE,importance  = TRUE))
    do.call(randomForest::randomForest,p)
  })
}

#' Descriptor contributions
#' @description Calculate average descriptor contributions to random forest models.
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @details See \code{see ?randomForest::importance} for details on random forest importance metrics.
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
#' descriptor_contributions <- m %>%
#'   descriptorContributions()
#' @importFrom randomForest importance
#' @importFrom dplyr summarise_all
#' @export

descriptorContributions <- function(rfModels){
  rfModels %>%
    map(~{
      .x %>%
        importance() %>%
        data.frame(check.names = FALSE) %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        rename(Descriptor = rowname)
    }) %>%
    set_names(seq(1,length(.)) %>%
                as.character()) %>%
    bind_rows(.id = 'rep') %>%
    group_by(Descriptor) %>%
    select(-rep) %>%
    summarise_all(mean)
}