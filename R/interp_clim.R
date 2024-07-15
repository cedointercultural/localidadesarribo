#' Add ESM interpolated delta to GLORYS climatology
#'
#' @param this.rel 
#' @param esm.inter.files 
#' @param glorys.clim.files 
#' @param delta.dir 
#' @param type 
#' @param timestep 
#'
#' @return
#' @export
#'
#' @examples
interp_clim <- function(this.rel, esm.inter.files, glorys.clim.files, delta.dir, type, timestep){
  
  this.esm.interp <- grep(this.rel, esm.inter.files, value=TRUE)
  newflext <- gsub("interp.nc", "interpyr_", this.esm.interp)
  
  print(this.esm.interp)
  
  #separates singe file into multiple years
  system(paste0("cdo splityear ", this.esm.interp, " ",newflext), wait=TRUE)
  
  esm.inter.yr.files <- list.files(path = delta.dir, pattern = "*interpyr*.*", full.names = TRUE)
  this.rel.yr.files<- grep(this.rel,esm.inter.yr.files, value = TRUE)
  
  glorys.time.res <- gsub("clim","resclim",glorys.clim.files)
  #Reset reference time for files
  system(paste0("cdo setreftime,1850-01-01,00:00:00,1day ",glorys.clim.files," ",glorys.time.res), wait = TRUE)
  
  
  for(eachfile in this.rel.yr.files){
    
    this.proj.file <- gsub("interpyr","projfut",eachfile)  
   
     if(timestep=='month'){
      fxn <- ifelse(type=="add",'ymonadd','ymonmul')
      system(paste0("cdo griddes ", this.proj.file), wait=TRUE)
      system(paste0("cdo griddes ", glorys.time.res), wait=TRUE)
      
      cmd4 <- paste0('cdo ',fxn,' ',eachfile, " ",glorys.time.res," ",this.proj.file)
      system(cmd4, wait=TRUE)
    }
  }
  
}
