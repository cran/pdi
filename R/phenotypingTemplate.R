#' Phenotyping template
#' @description Export a copy of the oak phenotyping data collection spreadsheet.
#' @param path directory path for export output
#' @examples 
#' \dontrun{
#' phenotypingTemplate()
#' }
#' @export

phenotypingTemplate <- function(path = '.'){
  template_path <- system.file('template/phenotyping_template.xlsx',package = 'pdi')
  template_base <- basename(template_path)
  
  invisible(file.copy(template_path,str_c(path,template_base,sep = '/')))
}