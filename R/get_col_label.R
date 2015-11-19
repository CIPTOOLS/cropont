get_col_label_by <- function(abbreviation, lookup_column,  tbl){
  #tbl <- readxl::read_excel(file_in, 2) # do some more checks if this is the right tab
  lcl <- which(stringr::str_detect(names(tbl), lookup_column))
  lbl <- tbl[tbl[, lcl] == abbreviation, c("Variable label", "Variable ID")]
  paste(lbl, collapse = "|")
}

# file_in = "D:/projects/ibp-sweetpotato-traits/ontology_cip_2015_10_26_short.xlsx"
# 
# onto = readxl::read_excel(file_in, 2)
# 
# get_col_label_by( "WED1", "Synonym Grueneberg2010",  onto)
