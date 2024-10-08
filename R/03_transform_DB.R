#' @import RosyUtils
#' @import RosyApp
#' @title get_original_forms
#' @export
get_original_forms <- function(DB){
  forms <- DB$metadata$forms
  if(DB$internals$is_transformed){
    forms <- DB$transformation$original_forms
  }
  return(forms)
}
#' @title get_original_fields
#' @export
get_original_fields <- function(DB){
  fields <- DB$metadata$fields
  if(DB$internals$is_transformed){
    fields <- DB$transformation$original_fields
  }
  return(fields)
}
get_transformed_fields <- function(DB){
  fields <- NULL
  if(DB$internals$is_transformed){
    fields <- DB$metadata$fields
  }
  return(fields)
}
get_transformed_forms <- function(DB){
  forms <- NULL
  if(DB$internals$is_transformed){
    forms <- DB$metadata$forms
    forms$form_name <- forms$form_name_remap
    forms$form_label <- forms$form_label_remap
    forms <- forms[,which(colnames(forms)%in%c("form_name","form_label","repeating","repeating_via_events"))] %>% unique()
  }
  return(forms)
}
#' @title default_forms_transformation
#' @export
default_forms_transformation <- function(DB){
  forms_transformation <- get_original_forms(DB)
  if("repeating_via_events"%in% colnames(forms_transformation)){
    forms_transformation <- forms_transformation[order(forms_transformation$repeating_via_events),]
  }
  forms_transformation <- forms_transformation[order(forms_transformation$repeating),]
  merge_form_name <- DB$internals$merge_form_name
  forms_transformation$form_name_remap <- forms_transformation$form_name
  forms_transformation$form_label_remap <- forms_transformation$form_label
  forms_transformation$form_name_remap[which(!forms_transformation$repeating)] <- merge_form_name
  merge_form_name_label <- merge_form_name
  if(merge_form_name %in% forms_transformation$form_name){
    merge_form_name_label <- forms_transformation$form_label[which(forms_transformation$form_name==merge_form_name)]
  }
  forms_transformation$form_label_remap[which(!forms_transformation$repeating)] <- merge_form_name_label
  forms_transformation$merge_to <- merge_form_name
  forms_transformation$by.y <- forms_transformation$by.x <- forms_transformation$merge_to %>% sapply(function(form_name){
    if(form_name %in% names(DB$metadata$form_key_cols)){
      DB$metadata$form_key_cols[[form_name]] %>% paste0(collapse = "+") %>% return()
    }else{
      rows<- which(!forms_transformation$repeating)
      if(length(rows)==0)return(NA)
      form_name <- forms_transformation$form_name[rows[[1]]]
      DB$metadata$form_key_cols[[form_name]] %>% paste0(collapse = "+") %>% return()
    }
  })
  forms_transformation$x_first <- F
  forms_transformation$x_first[which(forms_transformation$repeating)] <- T
  return(forms_transformation)
}
#' @title default_forms_transformation
#' @export
default_fields_transformation <- function(DB){
  DB$transformation <- list(
    forms = NULL,
    fields = NULL,
    original_forms = NULL,
    original_fields = NULL,
    data_updates = NULL
  )
  fields_transformation <- NULL
  DB$metadata$form_key_cols %>% names() %>% lapply(function(form_name){
    DB$metadata$form_key_cols[[form_name]]
  })
  forms <- get_original_forms(DB)
  last_non_rep <- forms$form_name[which(!forms$repeating)] %>% dplyr::last()
  form_names <- forms$form_name[which(forms$repeating)]
  id_col <- DB$metadata$form_key_cols[[last_non_rep]]
  has_non_rep <- length(last_non_rep)>0
  if(has_non_rep){
    for(form_name in form_names){
      form_label <- forms$form_label[which(forms$form_name==form_name)]
      DB <- DB %>% add_field_transformation(
        field_name = paste0("n_forms_",form_name),
        form_name = last_non_rep,
        field_type = "text",
        field_type_R = "integer",
        field_label = paste0(form_label," Forms"),
        units = "n",
        data_func = function(DB,field_name,form_name){
          form <- gsub("n_forms_","",field_name)
          id_col <- DB$metadata$form_key_cols[[form_name]]
          DB$data[[form_name]][[id_col]] %>% matches(DB$data[[form]][[id_col]],count_only = T) %>% as.character() %>% return()
        }
      )
    }
  }
  for(form_name in form_names){
    form_label <- forms$form_label[which(forms$form_name==form_name)]
    DB <- DB %>% add_field_transformation(
      field_name = paste0(form_name,"_compound_key"),
      form_name = form_name,
      field_type = "text",
      field_type_R = "character",
      field_label = paste(form_label,"Compound Key"),
      data_func = function(DB,field_name,form_name){
        cols <- DB$metadata$form_key_cols[[form_name]]
        OUT <- NULL
        while(length(cols)>0){
          if(is.null(OUT)){
            OUT <- DB$data[[form_name]][[cols[1]]]
          }else{
            OUT <- OUT %>% paste0("_",DB$data[[form_name]][[cols[1]]])
          }
          cols <- cols[-1]
        }
        return(OUT)
      }
    )
  }
  fields_transformation <- DB$transformation$fields
  return(fields_transformation)
}
#' @title add_forms_transformation
#' @export
add_forms_transformation <- function(DB,forms_transformation,ask=T){
  if(missing(forms_transformation))forms_transformation <- default_forms_transformation(DB)
  forms_tranformation_cols <-c(
    "form_name",
    "form_label",
    "repeating",
    "form_name_remap",
    "form_label_remap",
    "merge_to",
    "by.x",
    "by.y",
    "x_first"
  )
  if(any(!names(forms_transformation)%in%forms_tranformation_cols)){
    bullet_in_console("Use `default_forms_transformation(DB)` is an example!")
    stop("forms_transformation needs the following colnames... ", forms_tranformation_cols %>% as_comma_string())
  }
  choice <- T
  if(!is.null(DB$transformation)){
    if(!identical(DB$transformation$forms,forms_transformation)){
      if(ask){
        choice <- utils::askYesNo("Do you want to add transformation? (it doesn't match previous transform)")
      }
    }
  }
  #add more checks
  DB$transformation$forms <- forms_transformation
  return(DB)
}
#' @title add_field_transformation
#' @export
add_field_transformation <- function(
    DB,
    field_name,
    form_name,
    field_type,
    field_type_R = NA,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "",
    units = NA,
    data_func = NULL
) {
  DB <-validate_DB(DB)
  if(wl(DB$transformation$fields$field_name==field_name)>0){
    DB$transformation$fields <- DB$transformation$fields[which(DB$transformation$fields$field_name!=field_name),]
  }
  # if(!DB$data %>% is_something())stop("Must have transformed data to add new vars.")
  fields <- get_original_fields(DB)
  in_original_redcap <- field_name %in% fields$field_name
  if(is_something(select_choices_or_calculations))select_choices_or_calculations <- choice_vector_string(select_choices_or_calculations)
  if(in_original_redcap){
    original_fields_row <- fields[which(fields$field_name==field_name),]
    if(missing(form_name))form_name <- original_fields_row$form_name
    if(missing(field_type))field_type <- original_fields_row$field_type
    if(is.na(field_label))field_label <- original_fields_row$field_label
    if(is.na(select_choices_or_calculations))select_choices_or_calculations <- original_fields_row$select_choices_or_calculations
    if(is.na(field_note))field_note <- original_fields_row$field_note
    if(identifier=="")identifier <- original_fields_row$identifier
  }
  if(is.null(data_func))warning("if no `data_func` is provided, the column is only added to the metadata",immediate. = T)
  if(!is.null(data_func)){
    func_template <- "data_func = function(DB,field_name){YOUR FUNCTION}"
    if(!is.function(data_func))stop("`data_func` must be a function ... ",func_template)
    allowed_args <- c("DB","field_name","form_name")
    if(all(!allowed_args %in% names(formals(data_func))))stop("`data_func` must have two aruguments (DB and field_name) ... ",func_template)
    if(any(!names(formals(data_func)) %in% allowed_args))stop("`data_func` can only have two aruguments (DB and field_name) ... ",func_template)
  }
  field_row <- data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = select_choices_or_calculations,
    field_note = field_note,
    identifier = identifier,
    field_type_R = field_type_R,
    units = units,
    in_original_redcap = in_original_redcap,
    field_label_short = field_label,
    field_func = data_func %>% function_to_string()
  )
  DB$transformation$fields <- DB$transformation$fields %>% dplyr::bind_rows(field_row)
  message("added '",field_name,"' column")
  return(DB)
}
combine_original_transformed_fields <- function(DB){
  the_names <- DB$transformation$fields$field_name
  fields <- get_original_fields(DB)
  if(is.null(the_names)){
    bullet_in_console("Nothing to add. Use `add_field_transformation()`",bullet_type = "x")
    return(fields)
  }
  for(field_name in the_names){
    field_row <- DB$transformation$fields[which(DB$transformation$fields$field_name==field_name),]
    form_name <- field_row$form_name
    # if(any(fields$field_name==field_name))stop("field_name already included")
    current_row <- which(fields$field_name==field_name)
    if(length(current_row)>0){
      fields <- fields[-current_row,]
      i <- current_row
      if(i>1)i <- i - 1
    }else{
      i<-which(fields$form_name == form_name&fields$field_name == paste0(form_name,"_complete"))
      if(length(i)>0){
        if(i[[1]]>1){
          i <- i-1
        }
      }
      if(length(i)==0){
        i<-which(fields$form_name == form_name)
      }
      if(length(i)>1){
        i <- i[[1]]
      }
      if(length(i)==0)i <- nrow(fields)
    }
    if(length(i)==0)stop("insert_after error")
    top <- fields[1:i,]
    bottom <- NULL
    if(i < nrow(fields))bottom <- fields[(i+1):nrow(fields),]
    fields <- top %>% dplyr::bind_rows(field_row) %>% dplyr::bind_rows(bottom)
  }
  return(fields)
}
#' @title run_fields_transformation
#' @export
run_fields_transformation <- function(DB,ask = T){
  the_names <- DB$transformation$fields$field_name
  if(is.null(the_names)){
    bullet_in_console("Nothing to run. Use `add_field_transformation()`",bullet_type = "x")
    return(DB)
  }
  original_fields <- get_original_fields(DB)
  the_names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  the_names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  fields_to_update <- NULL
  for(field_name in c(the_names_existing,the_names_new)){
    OUT <- NA
    row_of_interest <- DB$transformation$fields[which(DB$transformation$fields$field_name==field_name),]
    form_name <- row_of_interest$form_name
    if(row_of_interest$field_func!="NULL"){
      if(form_name %in% names(DB$data)){
        restored_func <- eval(parse(text = row_of_interest$field_func))
        OUT <- restored_func(DB = DB, field_name = field_name,form_name = form_name)
      }
    }
    if(field_name %in% the_names_existing){
      OLD <- DB$data[[form_name]][[field_name]]
      if(!identical(OUT,OLD)){
        ref_cols <- DB$metadata$form_key_cols[[form_name]]
        new <- old <- DB$data[[form_name]][,c(ref_cols,field_name)]
        new[[field_name]] <- OUT
        DF <-  find_df_diff2(
          new = new,
          old = old,
          ref_cols = ref_cols,
          view_old = ask,
          message_pass = paste0(form_name," - ",field_name,": ")
        )
        if(is_something(DF)){
          DB$transformation$data_updates[[field_name]] <- DF
        }
      }
    }
    if (form_name %in% names(DB$data)) {
      DB$data[[form_name]][[field_name]] <- OUT
    }
  }
  bullet_in_console(paste0("Added new fields to ",DB$short_name," `DB$data`"),bullet_type = "v")
  return(DB)
}
#' @title remap_named_df_list
#' @export
transform_DB <- function(DB,ask = T){
  if(DB$internals$is_transformed){
    bullet_in_console("Already transformed... nothing to do!",bullet_type = "x")
    return(DB)
  }
  forms_transformation <- DB$transformation$forms
  DB$transformation$original_col_names <- DB$data %>% names() %>% lapply(function(l){
    DB$data[[l]] %>% colnames()
  })
  names(DB$transformation$original_col_names) <- DB$data %>% names()
  # if(any(!names(transformation)%in%names(DB$data)))stop("must have all DB$data names in transformation")
  if(is_something(process_df_list(DB$data,silent = T)))DB <- run_fields_transformation(DB,ask = ask)
  named_df_list <- DB$data
  OUT <- NULL
  for(i in (1:nrow(forms_transformation))){
    TABLE <- forms_transformation$form_name[i]
    ref <- named_df_list[[TABLE]]
    if(!is.null(ref)){
      if(is_something(ref)){
        a<- forms_transformation[i,]
        z<-as.list(a)
        ref <- named_df_list[[TABLE]]
        rownames(ref)<- NULL
        by.x <- z$by.x <- z$by.x %>% strsplit("\\+") %>% unlist()
        by.y <- z$by.y <- z$by.y %>% strsplit("\\+") %>% unlist()
        if(length(z$by.x) != length(z$by.y) )stop("by.x and by.y must be same length... [",z$form_name,"] (",z$by.x %>% as_comma_string(),") AND (",z$by.y %>% as_comma_string(),")")
        if(TABLE == z$merge_to){
          OUT[[z$form_name_remap]] <- ref
        }else{
          mer <- named_df_list[[z$merge_to]]
          if(z$merge_to %in% names(OUT)){
            mer <- OUT[[z$merge_to]]
          }
          ref_names<-names(ref)
          mer_names<-names(mer)
          # new_name <- by.x %>% vec1_not_in_vec2(by.y)
          new_names <- ref_names %>% vec1_in_vec2(mer_names) %>% vec1_not_in_vec2(by.x)
          for(new_name in new_names){
            COL <- which(colnames(mer)==new_name)
            replace_name <- paste0(new_name,"_merged")
            a <- mer[,1:COL]
            a[[replace_name]] <- a[[COL]]
            b <- mer[,(COL+1):ncol(mer)]
            mer <- cbind(a,b)
          }
          bad_cols <- which(!by.x%in%by.y)
          z$by.x[bad_cols]
          z$by.y[bad_cols]
          if(length(bad_cols)>0){
            for(col in bad_cols){
              new_col_name <- paste0(z$by.y[col],"_merged")
              ref[[new_col_name]] <- ref[[z$by.x[col]]]
              z$by.x[col] <- new_col_name
            }
          }
          by.x <- z$by.x
          by.y <- z$by.y
          ref_names<-names(ref) %>% vec1_not_in_vec2(
            by.x %>% vec1_not_in_vec2(by.y)
          )
          mer_names<-names(mer)
          # new_name <- by.x %>% vec1_not_in_vec2(by.y)
          del_names <- mer_names %>% vec1_in_vec2(ref_names) %>% vec1_not_in_vec2(by.y)
          mer[,del_names] <- NULL
          ref$sort_me_ftlog <- 1:nrow(ref)
          if(is.null(mer)){
            a <- ref
          }else{
            a<- merge(
              x = ref,
              y = mer,
              by.x = by.x,
              by.y = by.y,
              all.x = T,
              sort = F
            )
          }
          a <- a[order(a$sort_me_ftlog),]
          all_names <- c(ref_names,names(mer)) %>% unique()
          if(is_something(z$x_first)){
            if(!z$x_first){
              all_names <- c(by.y %>% vec1_in_vec2(by.x),names(mer) %>% vec1_not_in_vec2(by.y),ref_names) %>% unique()
            }
          }
          a<- a[,match(all_names,names(a))]
          rownames(a)<- NULL
          OUT[[z$form_name_remap]] <- a
        }
      }
    }
  }
  if(any(!names(OUT)%in%unique(forms_transformation$form_name_remap)))stop("not all names in OUT objext. Something wrong with transform_DB()")
  DB$data <- OUT
  DB$internals$is_transformed <- T
  bullet_in_console(paste0(DB$short_name," transformed according to `DB$transformation`"),bullet_type = "v")
  # forms ---------
  DB$transformation$original_forms <- DB$metadata$forms
  #new function RosyUtils
  cols_to_keep <- c("form_name_remap","form_label_remap","repeating","repeating_via_events")
  cols_to_keep <- cols_to_keep[which(cols_to_keep%in% colnames(forms_transformation))]
  DB$metadata$forms <- forms_transformation[,cols_to_keep] %>% unique()
  colnames(DB$metadata$forms)[which(colnames(DB$metadata$forms)=="form_name_remap")] <-"form_name"
  colnames(DB$metadata$forms)[which(colnames(DB$metadata$forms)=="form_label_remap")] <-"form_label"
  DB$metadata$forms$original_form_name <- DB$metadata$forms$form_name %>% sapply(function(form_name){
    forms_transformation$form_name[which(forms_transformation$form_name_remap==form_name)] %>% paste0(collapse = " | ")
  }) %>% as.character()
  # fields------------
  DB$transformation$original_fields <- DB$metadata$fields
  fields <- combine_original_transformed_fields(DB)
  fields$original_form_name <- fields$form_name
  fields$form_name <- forms_transformation$form_name_remap[match(fields$form_name,forms_transformation$form_name)]
  fields <- fields[order(match(fields$form_name,forms_transformation$form_name)),]
  #new function RosyUtils
  first <- 1:which(colnames(fields)=="form_name")
  move <- which(colnames(fields)=="original_form_name")
  last <- which(colnames(fields)!="original_form_name")[-first]
  fields <-fields[,c(first,move,last)]
  DB$metadata$fields <- fields
  bullet_in_console(paste0("Added mod fields to ",DB$short_name," `DB$metadata$fields`"),bullet_type = "v")
  DB$metadata$choices <- fields_to_choices(DB$metadata$fields)
  DB$internals$last_data_transformation <- Sys.time()
  return(DB)
}
#' @title untransform_DB
#' @export
untransform_DB <- function(DB){
  if(!DB$internals$is_transformed){
    bullet_in_console("Already not transformed... nothing to do!",bullet_type = "x")
    return(DB)
  }
  named_df_list <- DB$data
  forms_transformation <- DB$transformation$forms
  original_form_names <- DB$transformation$original_forms$form_name
  original_fields <- DB$metadata$fields
  keys <- DB$metadata$form_key_cols
  OUT <- NULL
  if(any(!original_form_names%in%forms_transformation$form_name))stop("Must have all original form names in transformation!")
  # TABLE <- original_form_names%>%sample1()
  for(TABLE in original_form_names){
    DF <- named_df_list[[forms_transformation$form_name_remap[which(forms_transformation$form_name==TABLE)]]]
    COLS <- DB$transformation$original_col_names[[TABLE]]
    if(any(!COLS%in%colnames(DF)))stop("Missing cols from orginal DF transformation... ",TABLE)
    DF <- DF[,COLS]
    OUT[[TABLE]] <- DF
  }
  DB$data <- OUT
  DB$internals$is_transformed <- F
  DB$metadata$forms <- DB$transformation$original_forms
  DB$metadata$fields <- DB$transformation$original_fields
  bullet_in_console(paste0(DB$short_name," untransformed according to `DB$transformation`"),bullet_type = "v")
  return(DB)
}
missing_form_names <- function(DB){
  form_names <- names(DB$data)
  form_names <- form_names[which(!form_names%in% DB$metadata$forms$form_name)]
  return(form_names)
}
missing_field_names <- function(DB){
  md <- data.frame(
    field_name = DB$metadata$fields$field_name,
    form_name = DB$metadata$fields$form_name
  )
  d <- DB$data %>% names() %>% lapply(function(form_name){
    data.frame(
      form_name = form_name,
      field_name = colnames(DB$data[[form_name]])
    )
  }) %>% dplyr::bind_rows()
  # return(form_names)
}
