#' Prepare phenotype data
#' @description Process parsed phenotype data sheets into a tibble suitable for random forest analysis.
#' @param phenotypeData parsed phenotype data collection sheet returned from \code{readPhenotypeSheet}
#' @examples 
#' library(dplyr)
#' 
#' ## Retrieve file paths for example data
#' files <- list.files(system.file('phenotypeDataCollectionSheets',
#'   package = 'pdi'),full.names = TRUE)
#' 
#' ## Prepare data
#' d <- map(files,readPhenotypeSheet) %>%
#'   map(preparePhenotypeData)
#' @importFrom tidyr spread
#' @importFrom dplyr filter group_by mutate summarise tbl_df rename left_join mutate_at vars rename_at everything ungroup
#' @importFrom tidyselect contains
#' @importFrom stringr coll str_replace
#' @export

preparePhenotypeData <- function(phenotypeData){
  description <- phenotypeData %>%
    .$Description %>%
    spread(Descriptor,Value)
  
  cardinalAssessments <- phenotypeData %>%
    .$CardinalAssessments %>%
    filter(!(Descriptor %in% c('Whole stem photo ref number',
                               '3m section photo ref number')
    )) %>%
    spread(Descriptor,Value)
  
  averageCrownRadius <- cardinalAssessments %>%
    group_by(ID) %>%
    mutate(`Crown radius` = `Crown radius` %>% as.numeric()) %>%
    summarise(`Average Crown radius` = mean(`Crown radius`))
  
  deadTissue <- cardinalAssessments %>%
    group_by(ID) %>%
    summarise(`Dead Stem Tissue` = 'Hollow' %in% `Tap test`)
  deadTissue$`Dead Stem Tissue`[deadTissue$`Dead Stem Tissue` == TRUE] <- 'present'
  deadTissue$`Dead Stem Tissue`[deadTissue$`Dead Stem Tissue` == FALSE] <- 'absent'
  
  canopyClosure <- cardinalAssessments %>%
    group_by(ID) %>%
    summarise(`Canopy Closure` = 'Y' %in% `Canopy closure`)
  canopyClosure$`Canopy Closure`[canopyClosure$`Canopy Closure` == TRUE] <- 'present'
  canopyClosure$`Canopy Closure`[canopyClosure$`Canopy Closure` == FALSE] <- 'absent'
  
  symptoms <- phenotypeData %>%
    .$Symptoms
  symptoms$Length[is.na(symptoms$Length)] <- 0
  
  crackFrequencies <- symptoms
  crackFrequencies$Length[crackFrequencies$Length > 0] <- 1
  
  crackFrequencies <- crackFrequencies %>%
    group_by(ID,`Symptom Type`) %>%
    summarise(Count = sum(Length)) %>%
    spread(`Symptom Type`,Count) %>%
    ungroup() %>%
    mutate(ID = as.numeric(ID))
  
  
  averageCrackLength <- symptoms %>%
    group_by(ID,`Symptom Type`) %>%
    summarise(`Average Crack Length` = mean(Length,na.rm = TRUE)) %>%
    spread(`Symptom Type`,`Average Crack Length`) %>%
    rename(`Average Active Bleed Length` = `Active bleeds`,
           `Average Black Staining Length` = `Black staining`,
           `Average Calloused Wound Length` = `Calloused wound`) %>%
    ungroup() %>%
    mutate(ID = as.numeric(ID))
  
  description <- description %>%
    left_join(deadTissue, by = "ID") %>%
    left_join(averageCrownRadius, by = "ID") %>%
    left_join(canopyClosure, by = "ID") %>%
    left_join(crackFrequencies, by = "ID") %>%
    left_join(averageCrackLength, by = "ID")
  
  description$`Insect defoliation type` <- tolower(description$`Insect defoliation type`)
  
  description$`Crown fruiting bodies`[description$`Crown fruiting bodies` == 'P'] <- 'present'
  description$`Crown fruiting bodies`[description$`Crown fruiting bodies` == 'A'] <- 'absent'
  description$`Pruning / branch loss`[description$`Pruning / branch loss` == 'P'] <- 'present'
  description$`Pruning / branch loss`[description$`Pruning / branch loss` == 'A'] <- 'absent'
  description$`Ground level fruiting bodies`[description$`Ground level fruiting bodies` == 'P'] <- 'present'
  description$`Ground level fruiting bodies`[description$`Ground level fruiting bodies` == 'A'] <- 'absent'
  description$`Stem  fruiting bodies`[description$`Stem  fruiting bodies` == 'P'] <- 'present'
  description$`Stem  fruiting bodies`[description$`Stem  fruiting bodies` == 'A'] <- 'absent'
  description$`Oval shaped exit holes`[description$`Oval shaped exit holes` == 'P'] <- 'present'
  description$`Oval shaped exit holes`[description$`Oval shaped exit holes` == 'A'] <- 'absent'
  description$`Small circular shaped exit holes`[description$`Small circular shaped exit holes` == 'P'] <- 'present'
  description$`Small circular shaped exit holes`[description$`Small circular shaped exit holes` == 'A'] <- 'absent'
  
  description$`Active bleeds`[is.na(description$`Active bleeds`)] <- 0
  description$`Black staining`[is.na(description$`Black staining`)] <- 0
  description$`Calloused wound`[is.na(description$`Calloused wound`)] <- 0
  description$`Average Active Bleed Length`[is.na(description$`Average Active Bleed Length`)] <- 0
  description$`Average Black Staining Length`[is.na(description$`Average Black Staining Length`)] <- 0
  description$`Average Calloused Wound Length`[is.na(description$`Average Calloused Wound Length`)] <- 0
  description$`Agrillus exit holes`[is.na(description$`Agrillus exit holes`)] <- 0
  
  description <- description %>%
    mutate(`Agrillus exit holes` = `Agrillus exit holes` %>% as.numeric(),
           `Crown density` = `Crown density` %>% as.numeric(),
           Dbh = Dbh %>% as.numeric(),
           `Lower Crown Ht` = `Lower Crown Ht` %>% as.numeric(),
           `Missing crown` = `Missing crown` %>% as.numeric(),
           `Timber Ht` = `Timber Ht` %>% as.numeric(),
           `Total Ht` = `Total Ht` %>% as.numeric(),
           `Average Crown radius` =`Average Crown radius` %>%  as.numeric(),
           `Active bleeds` = `Active bleeds` %>% as.numeric(),
           `Black staining` = `Black staining` %>% as.numeric(),
           `Calloused wound` = `Calloused wound` %>% as.numeric(),
           `Average Active Bleed Length` = `Average Active Bleed Length`  %>% as.numeric(),
           `Average Black Staining Length` = `Average Black Staining Length` %>% as.numeric(),
           `Average Calloused Wound Length` = `Average Calloused Wound Length` %>% as.numeric(),
           `Crown contact % of crown circumference` = `Crown contact % of crown circumference` %>% as.numeric()
    )
  
  description <- description %>%
    rename(Status = Symptomatic,
           `Crown transparency (%)` =  `Crown density`,
           `Diameter at breast height (cm)` = `Dbh`,
           `Lower crown height (m)` = `Lower Crown Ht`,
           `Missing crown (%)` = `Missing crown`,
           `Timber height (m)` = `Timber Ht`,
           `Total height (m)` = `Total Ht`,
           `Crown radius (m)` = `Average Crown radius`,
           `Active bleed length (cm)` = `Average Active Bleed Length`,
           `Black staining length (cm)` = `Average Black Staining Length` ,
           `Calloused wound length (cm)` = `Average Calloused Wound Length`,
           `Agrilus exit holes` = `Agrillus exit holes`,
           `Stem fruiting bodies` = `Stem  fruiting bodies`,
           `Canopy closure` = `Canopy Closure`,
           `Social class` = `Social Class`,
           `Dead stem tissue` = `Dead Stem Tissue`,
           `Crown contact (%)` = `Crown contact % of crown circumference`)
  
  description$Status[description$Status == 'Y'] <- 'Symptomatic'
  description$Status[description$Status == 'N'] <- 'Non-symptomatic'
  
  description <- description %>%
    mutate_at(vars(contains(coll('(cm)'))),~{. * 10}) %>%
    rename_at(vars(contains(coll('(cm)'))),~{str_replace(.,'(cm)','mm')}) %>%
    mutate(`Diameter at breast height (mm)` = `Diameter at breast height (mm)` / 1000) %>%
    rename(`Diameter at breast height (m)` = `Diameter at breast height (mm)`) %>%
    mutate(Location = phenotypeData$Location) %>%
    select(Location,everything())
  
  return(description)
}
