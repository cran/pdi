#' Read phenptyping sheet
#' @description Parse .xlsx phenotype data collection sheets.
#' @param file file path to excel file to parse
#' @examples 
#' library(dplyr)
#' 
#' ## Retrieve file paths for example data
#' files <- list.files(system.file('phenotypeDataCollectionSheets',
#'   package = 'pdi'),full.names = TRUE)
#' 
#' ## Prepare data
#' d <- readPhenotypeSheet(files[1])
#' @importFrom readxl read_excel
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr gather
#' @importFrom stats na.omit
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter bind_rows select mutate
#' @export

readPhenotypeSheet <- function(file){
  suppressWarnings(suppressMessages(description <- read_excel(file,sheet = 'Description')))
  suppressWarnings(suppressMessages(symptoms <- read_excel(file,sheet = 'Symptoms')))
  
  location <- description[1,2] %>%
    unlist(use.names = FALSE)
  surveyor <- description[2,2] %>%
    unlist(use.names = FALSE)
  date <- description[1,6] %>%
    unlist(use.names = FALSE)
  
  emptyRows <- is.na(description$Description)
  
  description <- description[!emptyRows,]
  
  description <- description[-1,]
  
  directionObservations <- description[,9:28]
  description <- description[-1,-(9:28)]
  colnames(description) <- description[1,]
  description <- description[-1,]
  
  description <- description %>%
    .[,!is.na(colnames(.))] %>%
    rowid_to_column(var = 'ID') %>%
    gather(Descriptor,Value,-ID)
  
  description <- description %>%
    filter(!(Descriptor == "NA" | is.na(Descriptor)))
  
  description$Descriptor[description$Descriptor == "Symptomatic (Y/N)"] <- 'Symptomatic'
  description$Descriptor[description$Descriptor == "Social Class (1-5)"] <- 'Social Class'
  description$Descriptor[description$Descriptor == "Missing crown    %"] <- 'Missing crown'
  description$Descriptor[description$Descriptor == "Crown density     %"] <- 'Crown density'
  description$Descriptor[description$Descriptor == "Agrillus exit holes (1,2...50, 50+)"] <- 'Agrillus exit holes'
  
  description$Value[is.na(description$Value) & description$Descriptor == 'Insect defoliation type'] <- 'None'
  description$Value[description$Descriptor == 'Symptomatic'] <- description$Value[description$Descriptor == 'Symptomatic'] %>% toupper()
  
  directions <- directionObservations[1,] %>%
    unlist(use.names = FALSE) %>%
    na.omit() %>%
    toupper()
  
  directionObservations <- directionObservations[-1,]
  
  directionObservations <- split(seq(1,20),ceiling(seq_along(1:20)/5)) %>%
    map(~{
      d <- directionObservations[,.]
      colnames(d) <- d[1,]
      d <- d[-1,]
      if('Tap test (H/S)' %in% colnames(d)) {
        colnames(d)[colnames(d) == 'Tap test (H/S)'] <- 'Tap test'
      }
      d <- d %>%
        rowid_to_column(var = 'ID')
      return(d)
    })
  names(directionObservations) <- directions
  directionObservations <- directionObservations %>%
    bind_rows(.id = 'Direction') %>%
    gather('Descriptor','Value',-Direction,-ID) %>%
    select(ID,Direction,Descriptor,Value)
  
  directionObservations$Descriptor[directionObservations$Descriptor == "Canopy closure (Y/N)"] <- 'Canopy closure'
  
  symptoms <- symptoms[-(seq(1,2)),]
  symptoms <- split(seq(1,ncol(symptoms)),ceiling(seq_along(seq(1,ncol(symptoms)))/3)) %>%
    map(~{
      s <- symptoms[,.]
      colnames(s) <- s[1,]
      s <- s[-1,]
      s <- s %>%
        rowid_to_column(var = 'Crack No')
      return(s)
    })
  names(symptoms) <- description$ID[seq(1,length(symptoms))] %>%
    unique()
  
  suppressWarnings(
    symptoms <- symptoms %>%
      bind_rows(.id = 'ID') %>%
      gather('Symptom Type','Length',-ID,-`Crack No`) %>%
      mutate(Length = str_replace_all(Length,'[:alpha:]','') %>% as.numeric())
  )
  
  phenotypeData <- list(Date = date,Location = location,Surveyor = surveyor,Description = description,CardinalAssessments = directionObservations,Symptoms = symptoms)
  return(phenotypeData)
}
