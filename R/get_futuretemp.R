#' Get future mean temperature for cluster areas
#'
#' @param future.files 
#' @param future.time 
#' @param rel.list 
#' @param future.path 
#' @param cluster.nums 
#' @param cluster.coords 
#'
#' @return
#' @export
#'
#' @examples
get_futuretemp <- function(eachrel, rel.list, future.files, future.time, future.path, cluster.nums, cluster.coords, ini.depth, end.depth){
  
  print(eachrel)
  this.rel <- rel.list[eachrel]
  this.rel.files <- grep(this.rel, future.files, value=TRUE)
  
  for(thisrelfile in 1:length(this.rel.files)){
    
    eachrelfile <- this.rel.files[thisrelfile]
    
    print(eachrelfile)
    
    for(eachcluster in cluster.nums){
      
      print(eachcluster)
      this.cluster.coords <- grep(eachcluster, cluster.coords, value= TRUE)
      this.cluster.future.file <- gsub("mprojfut",paste0("clust_",eachcluster), eachrelfile)
      
      #print(readLines(this.cluster.coords))
      #create mask for cluster convex hull
      if(!file.exists(this.cluster.future.file)){
      print("mask region")
      system(paste0("cdo -maskregion,",this.cluster.coords," ",eachrelfile, " ", this.cluster.future.file), wait=TRUE)
         }
      this.cluster.raster <- get_nc_description(this.cluster.future.file)
      terra::plot(this.cluster.raster)
      
      #monthly means by depth
      this.mean.future.file <- gsub("mprojfut",paste0("mclust_",eachcluster), eachrelfile)
      
       if(!file.exists(this.mean.future.file)){
      print("get monthly depth")
      system(paste0("cdo fldmean ",this.cluster.future.file," ", this.mean.future.file), wait=TRUE)
        
        }
      system(paste0("cdo sinfon ",this.mean.future.file), wait=TRUE)

      #monthly means across all depths
      #first calculate mean across vertical levels
      this.vertmean.future.file <- gsub("mprojfut",paste0("mvclust_",eachcluster), eachrelfile)
      
      if(!file.exists(this.vertmean.future.file)){
        print("get vertical mean")
        system(paste0("cdo vertmean ",this.cluster.future.file," ", this.vertmean.future.file), wait=TRUE)
      }
      
      this.cluster.raster <- get_nc_description(this.vertmean.future.file)
      terra::plot(this.cluster.raster)
      
      #calculate mean across spatial domain
      this.vertmeanspa.future.file <- gsub("mprojfut",paste0("mvsclust_",eachcluster), eachrelfile)
      
      if(!file.exists(this.vertmeanspa.future.file)){
        print("calculate spatial mean")
        system(paste0("cdo fldmean ",this.vertmean.future.file," ", this.vertmeanspa.future.file), wait=TRUE)
      }
       system(paste0("cdo sinfon ",this.vertmeanspa.future.file), wait = TRUE)
      
      
      #monthly means across 0-10 m
      this.mvsdepth.future.file <- gsub("mprojfut",paste0("mvsdepth_",eachcluster), eachrelfile)
      this.mvdsel.future.file <- gsub("mprojfut",paste0("mvdsel_",eachcluster), eachrelfile)
      this.mvslice.future.file <- gsub("mprojfut",paste0("mvslice_",eachcluster), eachrelfile)
      
      print(this.mvslice.future.file)
      
      #calculate mean across vertical levels 0-10 m
      
      if(!file.exists(this.mvslice.future.file)){
        print("creating hyperslab for 10 m")
        system(paste0("ncks -F -d"," lev,",ini.depth,",",end.depth,",1 ", this.mean.future.file," ", this.mvsdepth.future.file), wait=TRUE)
        system(paste0("cdo vertmean ", this.mvsdepth.future.file," ",  this.mvdsel.future.file), wait=TRUE)
        system(paste0("cdo fldmean ", this.mvdsel.future.file," ",  this.mvslice.future.file), wait=TRUE)
        #-F so it starts indexing at 1
        #https://stackoverflow.com/questions/54367298/hyperslab-of-a-4d-netcdf-variable-using-ncks
        #system(paste0("ncdump -h ",depth.file), wait=TRUE)
        
      }
      
      system(paste0("cdo sinfon ", this.mvsdepth.future.file))
      system(paste0("cdo sinfon ", this.mvslice.future.file))
      
      
      print("get data for monthly mean across all depths")
      print(this.mean.future.file)
      vert.nc.tibble <- tidync::tidync(this.mean.future.file) %>% 
        tidync::hyper_tibble() 
      print(vert.nc.tibble)
      #extract time
      tunit <- ncmeta::nc_atts(this.mean.future.file, "time") %>% 
        tidyr::unnest(cols = c(value)) %>% 
        dplyr::filter(name == "units")
      #convert time into date
      vert.time.date <- RNetCDF::utcal.nc(tunit$value, vert.nc.tibble$time) %>% 
        tidyr::as_tibble() %>% 
        dplyr::select(year, month)
      #create data tibble
      vert.this.file.data <- vert.nc.tibble %>% 
        dplyr::select(-lon, -lat, -time) %>% 
        dplyr::rename(depth_m = lev) %>% 
        dplyr::bind_cols(vert.time.date) %>% 
        dplyr::distinct(year,month,depth_m, thetao) %>% 
        dplyr::mutate(cluster = eachcluster, depth_m = as.character(depth_m))
      
      print(vert.this.file.data)
      
      print("get mean for specific depths")
      depth10.data <- get_meandepth(this.mvslice.future.file, depthname="mean_10m", eachcluster) 
      
      depth30.data <- get_meandepth(this.vertmeanspa.future.file, depthname="mean_30m", eachcluster) 
      
      this.year <- unique(vert.this.file.data$year)
      
      this.clust.val <- dplyr::bind_rows(vert.this.file.data, depth10.data, depth30.data)
      print(this.clust.val)
      print("write results")
      readr::write_csv(this.clust.val,paste0(future.path,"/","cluster_temp_yr",this.year,"_clust",eachcluster,"_",this.rel, ".csv"))
      
    } #end cluster
    
  } # end nc file
  
  
}
