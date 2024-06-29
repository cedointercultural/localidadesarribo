
time.periods <- c("198501-198912","199001-199412","199501-199912","200001-200412", "200501-200912","201001-201412")
start.file.name <- "tos_Omon_MPI-ESM1-2-HR_historical_r"
end.file.name <- "i1p1f1_gn"
realizations <- 1:10
string.ini <- 'sudo azcopy copy '
storage.blob <- '"https://morzariacedostorage.blob.core.windows.net/cmipdata/tos/'
string.end <- '?sv=2022-11-02&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2024-09-14T00:41:20Z&st=2024-06-13T16:30:20Z&spr=https,http&sig=xWrqDJQLzLXKMH8vsOl9zLv5ZFFuPpdxeNJaAQteXY8%3D" "/home/atlantis/ESM_CMIP6/MPI_ESM"  --recursive --overwrite=ifsourcenewer'

start.file <- '#!/bin/bash 
echo "Will download all ESM historical projections from cedo Azure storage"'
end.file <- "if [ -d $HOME/bin ]; then
PATH=$PATH:$HOME/bin
fi"
cat(start.file, file="get_esm.sh", append = FALSE)

for(eachrealization in realizations){
  
  for(eachtimeperiod in time.periods){
    
 extract.string <- paste0(string.ini, storage.blob, start.file.name, eachrealization, end.file.name, eachtimeperiod,".nc", string.end)
    
    cat(get.file, file="get_esm.sh", append = TRUE)
  }
  
  
}


cat(end.file, file="get_esm.sh", append = TRUE)