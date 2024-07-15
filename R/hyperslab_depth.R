#' Hyperslab depth layers
#'
#' @param this.merge.file 
#' @param ini.depth 
#' @param end.depth 
#'
#' @return
#' @export
#'
#' @examples
hyperslab_depth <- function(this.merge.file,ini.depth, end.depth){
  
  print(this.merge.file)
  system(paste0("cdo sinfon ",this.merge.file), wait=TRUE)
  #system(paste0("ncdump -h ",this.merge.file), wait=TRUE)
  depth.file <- gsub("_merged.nc","_depth.nc", this.merge.file)
  
  print(depth.file)
  if(!file.exists(depth.file)){
    print("creating hyperslab")
    system(paste0("ncks -F -d"," lev,",ini.depth,",",end.depth,",1 ", this.merge.file," ", depth.file), wait=TRUE)
    
    system(paste0("cdo sinfon ",depth.file))
    
    #-F so it starts indexing at 1
    #https://stackoverflow.com/questions/54367298/hyperslab-of-a-4d-netcdf-variable-using-ncks
    #system(paste0("ncdump -h ",depth.file), wait=TRUE)
    # use this to dump whole file and check data  
    #  system(paste0("ncdump ",depth.file," >ncdump.txt"), wait=TRUE)
    #could not figure out cdo syntax
    #system(paste0("cdo select,levrange=",ini.depth,",",end.depth,",name=Z ",this.merge.file," ", depth.file), wait = TRUE)
    #cdo select,levrange=25,900,name=T icon_oce.nc sellevel.nc
    
  }
}
