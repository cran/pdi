#' Multidimensional scaling
#' @description perform multidimensional scaling of random forest proximities
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @param dimensions number of dimensions to scale to
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
#' mds_data <- mds(m,2)
#' @importFrom tibble as_tibble
#' @importFrom stats cmdscale
#' @importFrom purrr map
#' @importFrom stringr str_remove_all str_c
#' @importFrom dplyr rename_all
#' @export

mds <- function(rfModels,dimensions = 2){
  rfModels %>%
    map(~{.$proximity %>%
        as_tibble(.name_repair = 'minimal') %>%
        rowid_to_column(var = 'Sample1') %>%
        gather('Sample2','Proximity',-Sample1)}) %>%
    bind_rows(.id = 'Iteration') %>%
    mutate(Sample2 = as.numeric(Sample2)) %>%
    group_by(Sample1,Sample2) %>%
    summarise(Proximity = mean(Proximity),.groups = 'drop') %>%
    spread(Sample2,Proximity) %>%
    ungroup() %>%
    select(-Sample1) %>%
    as.matrix() %>%
    {1 - .} %>%
    cmdscale(k = dimensions) %>%
    {suppressMessages(as_tibble(.,.name_repair = 'universal'))} %>%
    rename_all(str_remove_all,pattern = coll('...')) %>%
    rename_all(function(x){str_c('Dimension ',x)})
}