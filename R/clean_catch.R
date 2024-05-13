#' Combine catch files
#'
#' @param this.catchfileno 
#'
#' @return catch.file
#' @export
#'
#' @examples
clean_catch <- function(this.catchfileno, catch.files.clean){
  
  this.catchfile <- catch.files.clean[this.catchfileno]
  print(this.catchfileno)
  print(this.catchfile)
  
  catch.file <- data.table::fread(this.catchfile) %>% 
  mutate(landed_w_kg = as.numeric(landed_w_kg),
         value_mxn = as.numeric(value_mxn),
         year = as.numeric(year),
         rnp_code = as.character(rnp_code),
         NOM_ENT = as.character(NOM_ENT),
         NOM_LOC = as.character(NOM_LOC))
  
  return(catch.file)
}