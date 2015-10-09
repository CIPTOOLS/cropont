#' Creates an empty dictionary table
#'
#' With dummy data
#'
#' @param crop string
#' @author Reinhard Simon
#' @export
#' @return dataframe
new_dictionary_table <- function(crop){
  n = 3
  id <- 1:n
  shortn <- paste("ID_",id)
  altern <- rep("", n)
  fulln <-  rep("", n)
  local <-  rep("", n)
  latd <-  rep(0, n)
  lond <- rep(0, n)
  elev <-  rep(0, n)
  crops <-  rep("", n)
  aez <-  rep("", n)
  cont<-  rep("", n)
  creg<-  rep("", n)
  cntry<-  rep("", n)
  adm4<-  rep("", n)
  adm3<-  rep("", n)
  adm2<-  rep("", n)
  adm1<-  rep("", n)
  comment<-  rep("", n)
  
  res <- as.data.frame(cbind(
    id, shortn, altern, fulln, local, latd, lond, elev, crops, aez,
    cont, creg, cntry, adm4, adm3, adm2, adm1,  comment),
    stringsAsFactors = FALSE)
  res
}

#' Gets a dictionary table.
#'
#' If not yet present creates a dummy one.
#'
#' @param crop character
#' @author Reinhard Simon
#' @export
#' @return dataframe
get_dictionary_table <- function(crop){
  fns <- fbglobal::fname_dictionary(crop)
  
  if(!file.exists(fns)) {
    base_dir <-  dirname(fns)
    if(!file.exists(base_dir)) dir.create(base_dir)
    table_dictionary <- new_dictionary_table(crop)
    save(table_dictionary, file = fns)
  }
  load(fns)
  table_dictionary
}

#' Posts a dictionary table locally.
#'
#' Posts a data.frame containing location data.
#'
#' @param crop character
#' @param table_dictionary data.frame
#' @author Reinhard Simon
#' @export
post_site_table <- function(crop, table_dictionary){
  save(table_dictionary, file = fbglobal::fname_sites(crop))
}

