#' @import RosyUtils
#' @import RosyApp
#' @title get sidebar_choices directory
#' @inheritParams save_DB
#' @export
sidebar_choices <- function(DB,n_threshold=1){
  choices <- annotate_choices(DB)
  choices <- choices[which(choices$n>=n_threshold),]
  sbc <- data.frame(
    form_name = choices$form_name,
    field_name = choices$field_name,
    name = choices$name,
    label = paste0(choices$label, " (n = ",clean_num(choices$n),")")
  )
  return(sbc)
}
#' @title make_table1
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
make_table1_DB<-function(DB,DF,group="no_choice",variables ,render.missing = F){
  variables<-variables[variables%in%colnames(DF)]
  # if(any(!x))warning(paste0(x,collapse = ", ")," <- not in the form you specified")
  if(length(variables)==0)stop("Provide variable names of at least length 1!")
  forumla <- paste0(variables, collapse = " + ")
  # caption  <- "Basic stats"
  # footnote <- "ᵃ Also known as Breslow thickness"
  if(group!="no_choice"){
    x<- DB$metadata$field_label[which(DB$metadata$field_name==group)]
    DF$group <-  DF[[group]] %>% factor()
    DF <- DF[which(!is.na(DF$group)),]
    table1::label(DF$group)       <- ifelse(is.na(x),group,x)
    forumla <- paste0(forumla, " | group")
  }
  forumla <- as.formula(paste0("~",forumla))
  if(render.missing){
    table1::table1(forumla,data=DF,render.missing=NULL)
  }else{
    table1::table1(forumla,data=DF)
  }
}
#' @title Run Quality Checks
#' @inheritParams save_DB
#' @export
run_quality_checks <- function(DB){
  DB <- validate_DB(DB)
  if(is_something(DB$quality_checks)){
    for (qual_check in names(DB$quality_checks)){
      the_function <- DB$quality_checks[[qual_check]]
      if(is.function(the_function)){
        DB <- the_function(DB)
      }
    }
  }
  return(DB)
}
#' @title split_choices
#' @export
split_choices <- function(x){
  oops <- x
  x <- gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x <- x %>% strsplit(" [:|:] ") %>% unlist()
  check_length <- length(x)
  # code <- x %>% stringr::str_extract("^[^,]+(?=, )")
  # name <- x %>% stringr::str_extract("(?<=, ).*$")
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  # x <- data.frame(
  #   code=x %>% strsplit(", ") %>% sapply(`[`, 1),
  #   name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  # )
  x <- data.frame(
    code=result[,2],
    name=result[,3]
  )
  rownames(x) <- NULL
  if(nrow(x)!=check_length)stop("split choice error: ",oops)
  x
}
