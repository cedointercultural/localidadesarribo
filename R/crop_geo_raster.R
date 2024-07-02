

crop_geo_raster <- function(this.file){

  
  raster.nc <- terra::rast(this.file)
  #raster.nc <- terra::raster(this.file,  varname = this.var, stopIfNotEqualSpaced = FALSE)
  proj4string(raster.nc) = "+proj=longlat +datum=WGS84"
  
  # Deal with fact that we want West Longitude as negative longitude, but CNRM uses continuous degrees East longitude only
  raster.nc.rotate <- terra::rotate(raster.nc) # https://stackoverflow.com/questions/25730625/how-to-convert-longitude-from-0-360-to-180-180
  plot(raster.nc.rotate)
  
  #-----------------
  # We want to crop map to just area around WA and Oregon and British Columbia
  #extent.study <- extent(-130, -121.5, 46, 51.5) # (first row: xmin, xmax; second row: ymin, ymax)
  # filter(lat < 40 & lat > (-10) & lon > (-130) & lon < (-50))
  raster.nc.rotate.ext <- crop(raster.nc.rotate, PugetSoundRaster)
  plot(raster.nc.rotate.ext)  
  
}
