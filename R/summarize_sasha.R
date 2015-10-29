base_path = "D:\\data\\SASHA"

all_excel <- file.path(base_path, list.files(base_path, pattern = ".xls" ,recursive = TRUE))

is_fieldbook <- function(file_name){
  ok = FALSE
  try({
    ok = all(c("Fieldbook", "General", "Master") %in% readxl::excel_sheets(file_name))  
  })
  ok
}

smpl <- "D:\\data\\SASHA\\ssp-wa\\2011 data archiving\\Raw data, some analysis\\2011 varietal trials\\VTGHH11AAPokuase.xls"

n = length(all_excel)
is_fb = logical(n)

is_fb <- unlist(lapply(all_excel, is_fieldbook))

fieldbook_names = all_excel[is_fb]

#################
#
# is data dictionary the same?

tbl <- readxl::read_excel(fieldbook_names[1], "Fieldbook")

library(magrittr)
dd_ok = logical(length(fieldbook_names))

nms <- readxl::read_excel(fieldbook_names[1], "Fieldbook") %>% names
cols <- c(1:8, 15, 22:24, 26:32, 36:48, 52:60, 63:72, 75:86)
nms = nms[cols]
for(i in 2:length(fieldbook_names)){
  nms2 = readxl::read_excel(fieldbook_names[i], "Fieldbook") %>% names
  nms2 = nms2[cols]
  dd_ok[i] = all(nms2 %in% nms)
}
work_set <- fieldbook_names[dd_ok]

# exclude those without data
no_data <- function(file_path){
  fb <- readxl::read_excel(file_path, "Fieldbook")
  fb <- fb[, cols]
  fbv <- fb[24:nrow(fb), 13:ncol(fb)]
  for(i in 1:ncol(fbv)) fbv[, i] <- as.numeric(fbv[, i])
  all(is.na(fbv))  
}

nd <- lapply(work_set, no_data) %>% unlist
work_set <- work_set[!nd]

# exclude those without germplasm names
no_gpnames <- function(file_path){
  fb <- readxl::read_excel(file_path, "Fieldbook")
  fb <- fb[, cols]
  fbv <- fb[25:nrow(fb), 7]
  all(is.na(fbv))
}
nn <- lapply(work_set, no_gpnames) %>% unlist
work_set <- work_set[!nn]

# exclude those with additional variables for the moment

 x = stringr::str_replace(work_set, "D:\\\\data\\\\", '')
writeLines(x, con = "work_list.txt")

# clean list of var-names
writeLines(nms, con = "work_list_variables.txt")


# exclude for BTI those that don't have full identifiers
no_ids <- function(file_path){
  fb <- readxl::read_excel(file_path, "Fieldbook")
  fb <- fb[, cols]
  fbv <- fb[24:nrow(fb), 7]
  is_letter <- function(s){
    x <- stringr::str_sub(s, 1, 1) %>% toupper
    x %in% LETTERS[1:25]
  }
  no_ids <- lapply(fbv, is_letter) %>% unlist
  #print(fbv)
  #print(no_ids)
  any(!no_ids)
}
ni <- lapply(work_set, no_ids) %>% unlist
writeLines(work_set[ni], con = "todo!_fieldbooks.txt")
work_set <- work_set[!ni]


# extract list of germplasm 

# check if year and location is present
get_factor <-function(sheet, fac){
  idx <- which(stringr::str_detect(sheet[, 1], fac))
  sheet[idx, 2]
}

get_year <- function(file_path, sheet){
  m_year_c <- get_factor(sheet, "Year") # use as final candidate?
  m_year = NA
  is_sasha_ssa <- function(file_path){
    stringr::str_detect(file_path, "ssp-sa")
  }
  
  if (!(is_sasha_ssa(file_path)) ){
    bn <- basename(file_path)
    m_year <- stringr::str_extract(bn, "[0-9]{2,4}[AB]{0,1}")
  }
    # try to get it from full path
  if(is.na(m_year)){
    dn <- dirname(file_path)
    m_year <- stringr::str_extract(dn, "[0-9]{2,4}[\\sAB]{0,1}")
  }
  if(is.na(m_year)) {
    m_year <- stringr::str_extract(dn, "_[0-9]{2,4}")
    m_year = stringr::str_sub(m_year, 2, 5)
  }
  if(is.na(m_year)){
    m_year = m_year_c # if all else fails
    if(is.na(m_year)) m_year = "0000"
  }
  if(nchar(m_year) == 5){
    m_year = stringr::str_sub(m_year, 1, 4)
  } 
  if(nchar(m_year) == 3){
    m_year = paste0("20", stringr::str_sub(m_year, 1, 2))
  }
  m_year <- stringr::str_replace(m_year, ".000000","")
  m_year
}

get_subregion <- function(file_path){
  stringr::str_extract(file_path, "ssp-[a-z]{2}")
}

get_trialtype <- function(file_path, sheet=NULL){
  if(is.null(sheet)) {
    tt = basename(file_path)
  } else {
    tt = get_factor(sheet, "Type of Trial")[1]  
    if(length(tt) == 0) tt = NA
  }
 
 if(is.na(tt)){
   tt = basename(file_path)
 }
 if(is.na(tt)) return("NN")
 if(stringr::str_detect(tt, "P[Y]{0,1}T")) return("PT")
 if(stringr::str_detect(tt, "PYGT")) return("PT")
 if(stringr::str_detect(tt, "A[Y]{0,1}T")) return("AT")
 if(stringr::str_detect(tt, "[O0]{1}[Y]{0,1}T")) return("OT")
 if(stringr::str_detect(tt, "M[Y]{0,1}[L]{0,1}T{0,1}")) return("MT")
 if(stringr::str_detect(tt, "[0-9]locations")) return("MT")
 if(stringr::str_detect(tt, "LM")) return("MT")
 if(stringr::str_detect(tt, "V[Y]{0,1}T")) return("VT")
 if(stringr::str_detect(tt, "VAR")) return("VT")
 if(stringr::str_detect(tt, "Var")) return("VT")
 if(stringr::str_detect(tt, "UN")) return("UN")
 if(stringr::str_detect(tt, "OFT")) return("OF")  
 "NN"
}

get_country <- function(file_path, sheet){
  m_country <- get_factor(sheet, "Country") %>% toupper
  if(is.na(m_country)){
    if(stringr::str_detect(file_path, "UG")){
      m_country = "UGANDA"
    }
    if(stringr::str_detect(file_path, "MZ")){
      m_country = "MOZAMBIQUE"
    }
  }
  if(is.na(m_country)){
    m_country = "NN"
  }
  m_country
}

get_trial_metadata <- function(file_path){
  sheet <- readxl::read_excel(file_path, "General")
  m_title <- get_factor(sheet, "Title")
  m_country <- get_country(file_path, sheet)
  m_year <- get_year(file_path, sheet)
  m_subregion <- get_subregion(file_path)
  m_trialtype <- get_trialtype(file_path, sheet)
  m_site <- get_factor(sheet, "Site Name")
  
  if(length(m_site)== 0 | is.na(m_site)  ) {
    m_site = "NN"
  }
  
  m_contact <- get_factor(sheet, "Scientist")
  if(length(m_contact)== 0 | is.na(m_contact)  ) {
    m_contact = "NN"
  }
  out <- paste(basename(file_path),"\t", m_title, m_year, m_subregion, m_trialtype, 
        m_country,  m_site, 
        m_contact)
  nok <- stringr::str_detect(out, "NN")
  write(out, file = "meta.txt", append = TRUE)
  nok
}

m_data = lapply(work_set, get_trial_metadata) %>% unlist
writeLines(work_set[m_data], con="todo2.txt")
work_set = work_set[!m_data]

get_valid_ids <- function(file_path){
  fb <- readxl::read_excel(file_path, "Fieldbook")
  fb <- fb[, cols]
  fbv <- fb[24:nrow(fb), 7]
  is_cip_number <- function(s){
    ss = stringr::str_extract(s, "CIP[ ]{0,1}[0-9]{6}[\\.]{0,1}[0-9]{0,3}")
    nchar(s) == nchar(ss)
  }
  is_name <- function(s){
    ss = stringr::str_extract(s, "[a-zA-Z ]{3,30}")
    nchar(s) == nchar(ss)
  }
  is_valid_id <- function(id){
    any(is_name(id), is_cip_number(id))
  }
  out = lapply(fbv, is_valid_id) %>% unlist
  x <- unique(fbv[out])
  paste(x, collapse = ", ")
}

accs <- lapply(work_set, get_valid_ids) %>% unlist
accs <- paste(accs, collapse = ", ")
accs <- stringr::str_split(accs, ", ") %>% unlist %>% sort %>% unique
accs <- accs[!(accs %in% c("", "LOCAL"))]
writeLines(accs, con = "valid_accs.txt")

# filter out variables without data:
get_var_na <- function(file_path, vars){
  fb <- readxl::read_excel(file_path, "Fieldbook")
  fb <- fb[, cols]
  fbv <- fb[24:nrow(fb), vars]
  all_na <- function(var){
    all(is.na(var))
  }
  lapply(fbv, all_na) %>% unlist %>% as.logical
}
vars <- names(fbv)
n <- length(work_set)
vna <- as.data.frame(matrix(FALSE, nrow = n, ncol = length(vars)))
names(vna) = vars
for(i in 1:n){
  vna[i, ] <- get_var_na(work_set[i], vars)
}



# subregion, year, country, place, file_src, file_new, trial type, contact, name, pedigree



