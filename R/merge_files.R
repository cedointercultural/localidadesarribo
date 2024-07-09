#' Merge nc files 
#'
#' @param this.realization 
#' @param esm.slab.files 
#' @param merged.dir 
#'
#' @return
#' @export
#'
#' @examples
merge_files <- function(this.rel, esm.slab.files, esm.slab.future.files, merged.dir, data.type, data.period, data.range){
  
  list.rel.files <- grep(this.rel, esm.slab.files, value=TRUE)
  list.rel.per.files <- grep(paste(data.period,collapse="|"), list.rel.files, value=TRUE)
  
  list.future.files <- grep(this.rel, esm.slab.future.files, value=TRUE)
  no.files <- length(list.rel.per.files)
  
  
  if(data.type=="historical"){
    
    rel.future.files <- grep(this.rel, list.future.files, value = TRUE)
    
    future.files <- grep(paste(data.period,collapse="|"), rel.future.files, value= TRUE)
    
    list.merge.files <- c(list.rel.per.files, future.files)
    
  }
  
  if(data.type=="future"){
    
    rel.future.files <- grep(this.rel, list.future.files, value = TRUE)
    
    future.files <- grep(paste(data.period,collapse="|"), rel.future.files, value = TRUE)
    
    list.merge.files <- future.files
  }
  
  print(list.merge.files)
  
  merge.file <- paste0(merged.dir, "tos_Omon_MPI-ESM1-2-HR_",this.rel,"_", data.range[1],"-",data.range[2],"_merged.nc")
  if(!file.exists(merge.file)){

    ClimateOperators::cdo("-mergetime", list.merge.files, merge.file)
    #this option can be used to create smaller files, not sure how R treats netcdf v2 files
    #https://code.mpimet.mpg.de/boards/1/topics/908
    # ClimateOperators::cdo("-f nc2 mergetime", list.realization.files, merge.file)
    #read netcdf info
    ClimateOperators::cdo("sinfo",merge.file)
    
  }

}
