#' Aggregate depth bins
#'
#' @param this.glorys 
#' @param agg.lev 
#' @param max.level 
#'
#' @return
#' @export
#'
#' @examples
agg_depth_bin <- function(this.glorys, agg.lev, max.level){
  
  this.glorys.setlev <- gsub(".nc","_setlev.nc",this.glorys)
  
  #first subset depth layers
  if(!file.exists(this.glorys.setlev)){
    
    #create file for all depths between 6 and 30m
    this.glorys.base <- gsub(".nc","_base.nc",this.glorys)
    system(paste0("ncks -O -F -d"," depth,",agg.lev,",",max.lev,",1 ", this.glorys," ", this.glorys.base), wait=TRUE) #inital depth should be 0.0
    ClimateOperators::cdo("sinfon",this.glorys.base)
    
    print("creating hyperslab for mean depth")
    #select depth levels
    this.glorys.depth <- gsub(".nc","_depth.nc",this.glorys)
    system(paste0("ncks -O -F -d"," depth,0.0,",agg.lev,",1 ", this.glorys," ", this.glorys.depth), wait=TRUE) #inital depth should be 0.0
    
    ClimateOperators::cdo("sinfon",this.glorys.depth)
    
    this.glorys.agg <- gsub(".nc","_agg.nc",this.glorys)
    #calculate mean
    system(paste0("cdo vertmean ",this.glorys.depth," ",this.glorys.agg))
    #(cdo vertmean ifile ofile)
    ClimateOperators::cdo("sinfon",this.glorys.agg)
    
    system(paste0("cdo -L -merge -setlevel,6.0 ", this.glorys.agg, " ",this.glorys.base, " ", this.glorys.setlev))
    #cdo -L -merge -setlevel,0.4 data1.nc -setlevel,1 data2.nc merged.nc
    
    ClimateOperators::cdo("sinfon",this.glorys.setlev)
    
  }
  
}
