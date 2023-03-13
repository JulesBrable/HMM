load_csv <- function(file, path = NULL){
  return(read.csv(paste0(path, file)))
}

load_Rdata <- function(file, path = NULL){
  return(miceadds::load.Rdata2(paste0(path, file)))
}