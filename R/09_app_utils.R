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
