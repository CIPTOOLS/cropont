library(magrittr)
new_prefix = "SPBTO"

write_header1 <- function(meta, file_out){
  write("format-version: 1.2", file = file_out, append = FALSE) # 1st! to start a new file
  adate = "03:11:2015 12:00"
  write(paste("date:", adate), file = file_out, append = TRUE)
  write(paste("saved-by:", Sys.getenv("USERNAME")), file = file_out, append = TRUE)
  write("auto-generated-by: cropont 0.0.1", file = file_out, append = TRUE)
  write(paste("default-namespace: Sweetpotato"), file = file_out, append = TRUE) # last line
}

add_point_to_text <- function(def){
  aptt <- function(def){
    def = stringr::str_trim(def)
    lst = stringr::str_sub(def, nchar(def), nchar(def)+1)
    if(lst != ".") def = paste0(def, ".")
    def
  }
  lapply(def, aptt) %>% unlist %>% as.vector
}

add_quotes_to_text <- function(def){
  aqtt <- function(def){
    paste0("\"", def, "\"")
  }
  lapply(def, aqtt) %>% unlist %>% as.vector
}

add_xref_to_text <- function(def, xref){
  axtt <- function(def, xref){
    if(is.na(xref) | xref == "") return(paste0(def, " []"))
    if(stringr::str_detect(xref, ", ")){
      xref <- stringr::str_split(xref, ", ")[[1]]
    }
    xref <- paste0(xref, " \"\"")
    xref <- paste(xref, collapse = ", ")    
    paste0(def, " [", xref, "]")
  }
  n = length(def)
  out = character(n)
  for(i in 1:n) {
    out[i] <- axtt(def[i], xref[i])
  }
  out
}

write_term_line <- function(line, file_out, new_prefix = 'SPBTO'){
  if(is.na(line)) return(line)
  if(is.null(line)) return(line)
  if(nchar(line) == 0) return("")
  if(line =="") return("")
  txt = ""

  if(stringr::str_detect(names(line), "def")) {
    #line = add_point_to_def(line)
    txt = paste0("def: ", line)
  } else
  if(stringr::str_detect(names(line), "synonym")) {
    if(nchar(line) > 0){
      if(stringr::str_detect(line, ", ")){
        line = stringr::str_split(line, ", ")[[1]]
      }
      
      txt = paste0("synonym: \"", line, "\" EXACT []")
      if(length(txt) > 1) txt = paste(txt, collapse="\n")
    }
  } else
  if(stringr::str_detect(names(line), "relationship")) {
    if(nchar(line) > 0){
      if(stringr::str_detect(line, "; ")){
        line = stringr::str_split(line, "; ")[[1]]
        line = line[line!=""]
      }
      txt = paste0("relationship: ", line)
      if(length(txt) > 1) txt = paste(txt, collapse="\n")
      
    }
  } else
  if(stringr::str_detect(names(line), "is_a")) {
    if(nchar(line) > 0){
      txt = paste0("is_a: ", line)
  }
  }else 
  
  #print(paste(line, txt))
    txt = paste0(names(line),": ", line)
  if(!(stringr::str_detect(txt, "NULL")
       || stringr::str_detect(txt, "NA")
       || nchar(txt) == 0)) {
    
    write(txt , file = file_out, append = TRUE)
  }  
}

get_is_a_name <- function(id, id_col, name_col, terms){
  if(is.na(id)) return("")
  terms <- terms[!is.na(terms[id_col]),]
  
  terms[ terms[,id_col] == id, name_col] 
}

write_term <- function(items, file_out, type = "Term"){
  
  for(i in 1:nrow(items)){
    write(paste0("[",type,"]"), file = file_out, append = TRUE)
    write_term_line(list(id = items[i, c("id")]), file_out)
    write_term_line(list(name  = items[i, c("name")]), file_out)
    if("namespace" %in% names(items)) write_term_line(list(namespace = items[i, c("namespace")]), file_out)
    if("def" %in% names(items)) write_term_line(list(def = items[i, c("def")]), file_out)
    if("comment" %in% names(items))write_term_line(list(comment = items[i, c("comment")]), file_out)
    if("synonym" %in% names(items)) write_term_line(list(synonym = items[i, c("synonym")]), file_out)
    if("synonyms" %in% names(items)) write_term_line(list(synonyms = items[i, c("synonyms")]), file_out)
    if("relationship" %in% names(items)) write_term_line(list(relationship = items[i, c("relationship")]), file_out)
    if("is_a" %in% names(items)) write_term_line(list(is_a = items[i, c("is_a")]), file_out)
    if("trait_class" %in% names(items)) write_term_line(list(is_a = items[i, c("trait_class")]), file_out)
    write("", file = file_out, append = TRUE)
  }
}

add_rel_name <- function(terms, rel_col,id_col, id_name, lookup = terms){
  for(i in 1:nrow(terms)){
    srch <-terms[i, rel_col]
    #print(paste(i, srch))
    txt <- get_is_a_name(srch, id_col, id_name, lookup)
    #print(str(txt))
    if(length(txt)==0) stop(paste("'", srch, "' not defined as term!"))
    if(nchar(txt) > 0)  {
      if(!stringr::str_detect(srch, ":")){
        terms[i, rel_col] <- paste0(txt, " ! ", srch)  
      } else {
        
        terms[i, rel_col] <- paste0(srch, " ! ", txt)  
      }
        
    }
  }
  terms
}

get_rels_of_ids <- function(src_id, tgt_id, tgt_name,  onto, rel_type = "method_of"){
  sel <- unique(onto[, src_id])
  n <- length(sel)
  rels = character(n)
  for(i in 1:n){
    #filter
    aset <- onto[onto[, src_id] == sel[i], ]
    aset <- aset[!duplicated(aset[, tgt_id]), ]

    txt <- paste0(rel_type," ", aset[, tgt_id], " ! ", aset[, tgt_name ])
    rels[i] <- paste(txt, collapse = "; ")    
  }
  rels
}

add_cols_to_comments <- function(cols, name = names(cols)){
  # TODO handle case of existing comments
  if(!is.data.frame(cols)) {
    cols = as.data.frame(cols, stringsAsFactors = FALSE)
    names(cols)[1] = name
  }
  n = nrow(cols)
  out = character(nrow(cols))
  
  for(i in 1:n){
    for(j in 1:ncol(cols)){
      if(!is.na(cols[i, j])) {
        cols[i, j] <- paste(name[j], ": ", cols[i,j], sep="")  
      } else {
        cols[i, j] <- NA
      }
    }
    colss <- cols[i, ]
    colss <- colss[!is.na(colss)]
    out[i] <- paste(colss, collapse = " | ")
    out[i] = (paste0("| ", out[i], ".", sep=""))
    if(out[i] == "| .") out[i] = ""
  }
  
  out
}

write_scale_cat <- function(scales, file_out){
  # filter out non cat scales
  scalec = scales[stringr::str_detect(scales$name, "nal"), ]
  if(nrow(scalec)>0){
    stanza <- scalec[1, 1:6]
    names(stanza) <- c("id", "name", "namespace", "relationship", "synonym", "is_a")
    n = nrow(scalec)
    for(i in 1:n){
      stanza[1, "id"] = scalec$id[i]
      stanza[1, "name"] = scalec$name[i]
      stanza[1, "namespace"] = scalec$namespace[i]
      stanza[1, "relationship"] = scalec$relationship[i]
      stanza[1, "synonym"] = ""
      stanza[1, "is_a"] = ""
      
      for(j in 0:9){
        coln <- paste("Category",j)
        catv <- scalec[i, coln]
        if(!is.na(catv)){
          arow = character(6)
          arow[1] = paste0(scalec$id[i],":",j)
          arow[2] = catv
          arow[3] = scalec$namespace[i]
          arow[4] = ""
          arow[5] = j
          arow[6] = paste0(scalec$id[i], " ! ", scalec$name[i])
          stanza = rbind(stanza, arow)
          
          
        }
        
      }
      write_term(stanza, file_out)
      stanza = stanza[1, ]
    }
    
  }
}

get_term_formula <- function(s){
  trms <- stringr::str_extract_all(s, "[a-zA-Z_\\.]{2,20}[:]{2}[a-zA-Z_\\.]{2,20}")[[1]]
  trms[!duplicated(trms)]
}


get_terms <- function(s){
  trms <- stringr::str_extract_all(s, "[a-zA-Z]{1}[a-zA-Z-_0-9\\.]{2,20}")[[1]]
  trms[!duplicated(trms)]
}

get_rels_of_var_formula <- function(temp){
  get_variable_rel <- function(v, temp){
    if(v == "" | is.na(v)) return("")
    x = character(nrow(temp))
    for(j in 1:nrow(temp)){
      #x = ""
      cmp = temp$`Variable synonyms`[j] %>% stringr::str_trim(side = "both")
      if(stringr::str_detect(cmp, ",")){
        cmp = stringr::str_split(cmp, ", ")[[1]]
      }
      if(v %in% cmp){
        p = paste0("derives_from ", temp[j, "Variable ID"]," ! ", temp[j, "Variable name"],
                   " (", temp[j, "Variable synonyms"], ")")
      } else {
        p = ""
      }
      x[j] = p
    }
    # print(x)
    x = x[x != ""]
    x
  }
  grvf <- function(rec, temp){
    if(temp$Formula[rec] == "" | is.na(temp$Formula[rec])) return("")
    trms <- get_terms(temp$Formula[rec])[-1]
    n = length(trms)
    x = character(n)
    for(i in 1:n){
      w <- get_variable_rel(trms[i],  temp )  
      if(length(w)!=0) x[i] = w
    }
    # print(x)
    #x <- x[x!=""]
    x = paste(x, collapse = "; ")
    x
  }
  out = character(nrow(temp))
  for(i in 1:nrow(temp)){
    out[i] = grvf(i, temp)
  }
  out
}

co2obo <- function(file_in, file_out = paste0(file_in,"x.obo")){
  #print(file_out)
  stopifnot(file.exists(file_in))
  sheets <- readxl::excel_sheets(file_in)
  stopifnot(c("READ ME") %in% sheets) 
  has_ontology_list = stringr::str_detect(sheets, "ontology")
  stopifnot((any(has_ontology_list)))
  
  sh_ont = sheets[has_ontology_list]
  onto <- readxl::read_excel(file_in, sh_ont)
  meta <- readxl::read_excel(file_in, "Meta")
  met2 <- readxl::read_excel(file_in, "Meta2")
  
  
  write_header1(met2, file_out)
  meta_terms = meta[, "type"] %in% "Term"
  meta_terms = meta[meta_terms, ]
  meta_terms = add_rel_name(meta_terms, "trait_class", "id", "name")
  meta_terms$def <- meta_terms$def %>%  add_point_to_text %>% add_quotes_to_text
  meta_terms$def <- add_xref_to_text(meta_terms$def, meta$xref)
  
  write_term(meta_terms, file_out)

  meta_types = meta[, "type"] %in% "Typedef"
  meta_types = meta[meta_types, ]
  
  
  write_term(meta_types, file_out, type = "Typedef")
  
  # Traits
  trait <- onto[, c("Trait ID", "Trait", "Trait description", 
                    "Trait abbreviation", "Trait synonyms", "Trait class")]
  trait_terms = add_rel_name(trait, "Trait class", "name", "id", meta_terms)
  txt = met2[met2$variable == "namespace_trait", "value"]
  txt = as.character(txt)
  trait_terms = cbind(trait_terms, 
                      rep(txt, nrow(trait_terms)))
  names(trait_terms) = c("id", "name", "def", "synonym", "synonyms", "trait_class", "namespace")
  trait_terms = trait_terms[, c("id", "name", "namespace", "def", "synonym", "synonyms", "trait_class")]
  trait_terms[, "namespace"] = as.character(trait_terms[, "namespace"])
  
  trait_terms$def <- trait_terms$def %>%  add_point_to_text %>% add_quotes_to_text
  trait_terms$def <- add_xref_to_text(trait_terms$def, onto$`Trait Xref`)
  comment = add_cols_to_comments(onto[, c("Entity", "Attribute", "Trait status")])
  trait_terms = cbind(trait_terms, comment)
  trait_terms$comment <- as.character(trait_terms$comment) # %>% add_quotes_to_text
  write_term(trait_terms, file_out, type = "Term")
  
  ## Methods
  methd <- onto[!duplicated(onto$`Method ID`), 
                c("Method ID", "Method", "Method description", "Method class", 
                  "Method reference", "Formula")]
  methd$Method <- paste0(methd$`Method class`,":", methd$Method)
  methd <- cbind(methd, rep(met2[met2$variable == "namespace_method", "value"], nrow(methd)))
  
  names(methd) = c("id", "name", "def", "relationship", "xref", "Formula", "namespace" )
  methd$namespace <- as.character(methd$namespace)
  methd[, "relationship"] <- get_rels_of_ids("Method ID", "Trait ID", "Trait", onto )
  
  methd$def <- methd$def %>%  add_point_to_text %>% add_quotes_to_text
  methd$def <- add_xref_to_text(methd$def, methd$xref)

  #comment = add_cols_to_comments(methd[, c("Formula")])
  #methd$Formula <- as.character(comment)
  #names(methd)[ncol(methd)-1] = "comment"
  
  write_term(methd, file_out, type = "Term")
  
  
  
  ## Scales
  scales <- onto[!duplicated(onto$`Scale ID`), 
               c("Scale ID", "Scale name", "Scale class", 
                 "Decimal places", "Lower limit", "Upper limit", "Scale Xref",
                 "Category 0", "Category 1", "Category 2", "Category 3", "Category 4", 
                 "Category 5", "Category 6", "Category 7", "Category 8", "Category 9" 
                 )]
  names(scales)[1:3] = c("id", "name", "class")
  names(scales)[7] = "xref"
  #scales$def <- add_xref_to_text(scales$def, methd$xref)
  scales <- cbind(scales, rep(met2[met2$variable == "namespace_scale", "value"], nrow(scales)))
  names(scales)[ncol(scales)] = "namespace"
  scales$namespace = as.character(scales$namespace)
  scales$name <- paste0(scales$class,":", scales$name)
  temp = onto
  temp$Method = paste0(temp$`Method class`,": ", temp$Method )
  scales$class <- get_rels_of_ids("Scale ID", "Method ID", "Method", temp, rel_type = "scale_of" )
  names(scales)[3] = "relationship"

  comment = add_cols_to_comments(scales[, c("Decimal places", "Lower limit", "Upper limit")])
  scales$`Decimal places` <- as.character(comment)
  names(scales)[4] = "comment"
  
  temp = scales[!stringr::str_detect(scales$name, "nal"), ]
  write_term(temp[, c("id", "name", "namespace","comment", "relationship")], file_out, type = "Term")
  temp = NULL
  ## special treatment for categories
  write_scale_cat(scales, file_out)
  
  # Variables
  ## TODO discuss: Variable def == Trait ?
  vars <- onto[!duplicated(onto$`Variable ID`),
               c("Variable ID", "Variable label", "Trait", "Variable synonyms", "Variable Xref",
                 "Variable name", 
                 "Context of use", "Growth stage", "Variable status", "CV Term ID",
                 "Scientist", "Institution", "Language of submission", "Date of submission",
                 "Trait ID", 
                 "Method ID", "Method", "Method class",
                 "Scale ID", "Scale name", "Scale class")]
  names(vars)[1:5] = c("id", "name", "def", "synonyms", "xref")
  vars$def = add_point_to_text(vars$def) %>% add_quotes_to_text
  vars$def = add_xref_to_text(vars$def, vars$xref)
  for(i in 1:nrow(vars)){
    vars$synonyms[i] = paste0(vars$synonyms[i], ", ", vars$'Variable name'[i])
  }

  temp = vars
  temp$xref = get_rels_of_ids("id", "Trait ID", "def", temp, rel_type = "variable_of" )
  temp$Method = paste0(temp$`Method class`,": ", temp$Method )
  temp$`Method class` <- get_rels_of_ids("id", "Method ID", "Method", temp, rel_type = "variable_of" )
  temp$`Scale name` = paste0(temp$`Scale class`,": ", temp$`Scale name` )
  temp$`Scale class` <- get_rels_of_ids("id", "Scale ID", "Scale name", temp, rel_type = "variable_of" )
  tmp <- get_rels_of_var_formula(onto) 
  for(i in 1:nrow(vars)){
    vars$xref[i] = paste(c(temp$xref[i], temp$`Method class`[i], temp$`Scale class`[i], tmp[i]), collapse = "; ")
  }
  names(vars)[5] = "relationship"
  temp = NULL
  
  names(vars)[20] = "namespace"
  vars$namespace = rep(met2[met2$variable == "namespace_variable", "value"], nrow(vars))
  
  names(vars)[19] = "comment"
  vars$`Date of submission` <- as.character(vars$`Date of submission`)
  vars$comment = add_cols_to_comments(vars[, c("Context of use", 
                                                "Growth stage",
                                                "Variable status",
                                                "CV Term ID", 
                                                "Scientist", 
                                                "Institution",
                                                "Language of submission", 
                                                "Date of submission"
                                               )])
  
  write_term(vars[, c(1:5, 19:20)], file_out)
  
}

#file_in = "D:/projects/ibp-sweetpotato-traits/ontology_cip_2015_10_26_short.xlsx"

#co2obo(file_in)
