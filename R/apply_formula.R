library(magrittr)
# fieldbook = cbind(FW = c(212, 200, 220), DW = c(80, 75, 90), DM = c("","","")) %>% 
#   as.data.frame(stringsAsFactors = FALSE)
# 
# formula = c("DMpct = DW / FW * 100", "DM = DW / FW")


get_function_name <- function(formula){
  fn = NULL
  if(stringr::str_detect(formula,"=")){
    fn = stringr::str_split(formula, "=")[[1]][1] %>% stringr::str_trim(side = "both")
  }
  fn
}


apply_formula <- function(fieldbook, formula){
  af <- function(fieldbook, formula){
  terms  = get_terms(formula)
  
  if (all(terms[-1] %in% names(fieldbook))) {
    
    try({
      n = length(terms[-1])
      for(i in 1:n){
        fieldbook[, terms[i+1]] = as.numeric(fieldbook[, terms[i+1]])
      }
    })
    
    if(!all(lapply(fieldbook[, terms[-1]], is.numeric) %>% unlist)){
      return(fieldbook)
    }
    
    x <- with(fieldbook,{
      # check if all term columns are present
      eval(parse(text = formula))
      
    })
    fn = get_function_name(formula)
    if(!(fn %in% names(fieldbook))){
      fieldbook = cbind(fieldbook, x)
      names(fieldbook)[ncol(fieldbook)] = fn
    }
    if(fn %in% names(fieldbook)){
      fieldbook[, fn] = x
    }
  }
  fieldbook
  }
  for(i in 1:length(formula)){
    fieldbook = af(fieldbook, formula[i])
  }
  fieldbook
}

#apply_formula(fieldbook, formula)

