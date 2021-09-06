
#' Read in CSV for Cook County Deaths
#' 
#' @source https://maps.cookcountyil.gov/medexamcovid19/
#' @source https://datacatalog.cookcountyil.gov/Public-Safety/Medical-Examiner-Case-Archive-COVID-19-Related-Dea/3trz-enys
#' 
read_cook_county_deaths <- function() { 
  readr::read_csv(here("Medical_Examiner_Case_Archive.csv"))
}