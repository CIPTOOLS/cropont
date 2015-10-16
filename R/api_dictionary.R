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
  trait_id = 1:n               
  trait = rep("", n)
  entity = rep("", n)
  attribute = rep("", n)
  trait_synonyms = rep("", n)
  trait_abbreviation = rep("", n)
  trait_description = rep("", n)
  trait_class = rep("", n)
  trait_status = rep("", n)          
  trait_xref = rep("", n)
  method_id = rep("", n)
  method = rep("", n)
  method_description = rep("", n)
  formula = rep("", n)
  method_class = rep("", n)
  method_reference = rep("", n)
  scale_id = rep("", n)
  scale_name = rep("", n)
  scale_class = rep("", n)
  decimal_places = rep("", n)
  lower_limit = rep("", n)
  upper_limit = rep("", n)
  scale_xref = rep("", n)
  category_0 = rep("", n)
  category_1 = rep("", n)
  category_2 = rep("", n)
  category_3 = rep("", n)
  category_4 = rep("", n)
  category_5 = rep("", n)
  category_6 = rep("", n)
  category_7 = rep("", n)
  category_8 = rep("", n)
  category_9 = rep("", n)
  category_10 = rep("", n)
  variable_id = rep("", n)
  variable_name = rep("", n)
  variable_synonyms = rep("", n)
  context_of_use = rep("", n)
  growth_stage = rep("", n)
  variable_status = rep("", n)
  variable_xref = rep("", n)
  scientist = rep("", n)
  institution = rep("", n)
  language_of_submission = rep("", n)
  date_of_submission = rep("", n)
  crop = rep(crop, n)
  
  res <- as.data.frame(cbind(
    trait_id, trait, entity, attribute, trait_synonyms, trait_abbreviation, trait_description,
    trait_class, trait_status, trait_xref, method_id, method, method_description, formula,
    method_class, method_reference, scale_id, scale_name, scale_class, decimal_places,
    lower_limit, upper_limit, scale_xref, category_0, category_1, category_2, category_3,
    category_4, category_5, category_6, category_7, category_8, category_9, category_10,
    variable_id, variable_name, variable_synonyms, context_of_use, growth_stage, variable_status,
    variable_xref, scientist, institution, language_of_submission, date_of_submission, crop  
    ),
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
  #print(fns)
  if (is.null(fns)) return(NULL)
  if (!file.exists(fns)) {
    base_dir <-  dirname(fns)
    if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
    table_dictionary <- new_dictionary_table(crop)
    saveRDS(table_dictionary, file = fns)
  }
  readRDS(fns)
}

#' Posts a dictionary table locally.
#'
#' Posts a data.frame containing location data.
#'
#' @param crop character
#' @param table_dictionary data.frame
#' @author Reinhard Simon
#' @export
post_dictionary_table <- function(crop, table_dictionary){
  saveRDS(table_dictionary, file = fbglobal::fname_dictionary(crop))
}

