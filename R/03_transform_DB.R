#' @import RosyUtils
#' @import RosyApp
#' @title default_forms_transformation
#' @export
default_forms_transformation <- function(DB){
  transformation_template <- DB$metadata$forms[,c("instrument_name","instrument_label","repeating")]#longitudinal_fix
  transformation_template <- transformation_template[order(transformation_template$repeating),]
  merge_form_name <- DB$internals$merge_form_name
  transformation_template$instrument_name_remap <- transformation_template$instrument_name
  transformation_template$instrument_label_remap <- transformation_template$instrument_label
  transformation_template$instrument_name_remap[which(!transformation_template$repeating)] <- merge_form_name
  merge_form_name_label <- merge_form_name
  if(merge_form_name %in% transformation_template$instrument_name){
    merge_form_name_label <- transformation_template$instrument_label[which(transformation_template$instrument_name==merge_form_name)]
  }
  transformation_template$instrument_label_remap[which(!transformation_template$repeating)] <- merge_form_name_label
  transformation_template$merge_to <- merge_form_name
  original_forms <- get_original_forms(DB)
  transformation_template$by.y <- transformation_template$by.x <- transformation_template$merge_to %>% sapply(function(instrument_name){ DB$metadata$form_key_cols[[instrument_name]] %>% paste0(collapse = "+")})
  transformation_template$x_first <- F
  transformation_template$x_first[which(transformation_template$repeating)] <- T
  return(transformation_template)
}
#' @title add_forms_transformation_to_DB
#' @export
add_forms_transformation_to_DB <- function(DB,forms_tranformation,ask=T){
  forms_tranformation_cols <-c(
    "instrument_name",
    "instrument_label",
    "repeating",
    "instrument_name_remap",
    "instrument_label_remap",
    "merge_to",
    "by.x",
    "by.y",
    "x_first"
  )
  if(any(!names(forms_tranformation)%in%forms_tranformation_cols)){
    bullet_in_console("Use `default_forms_transformation(DB)` is an example!")
    stop("forms_tranformation needs the following colnames... ", forms_tranformation_cols %>% as_comma_string())
  }
  choice <- T
  if(!is.null(DB$transformation)){
    if(!identical(DB$transformation$forms,forms_tranformation)){
      if(ask){
        choice <- utils::askYesNo("Do you want to add transformation? (it doesn't match previous transform)")
      }
    }
  }
  #add more checks
  DB$transformation$forms <- forms_tranformation
  return(DB)
}
#' @title remap_named_df_list
#' @export
transform_DB <- function(DB,ask = T){
  if(DB$internals$is_transformed){
    bullet_in_console("Already transformed... nothing to do!",bullet_type = "x")
    return(DB)
  }
  named_df_list <- DB$data
  forms_transformation <- DB$transformation$forms
  DB$transformation$original_col_names <- DB$data %>% names() %>% lapply(function(l){
    DB$data[[l]] %>% colnames()
  })
  names(DB$transformation$original_col_names) <- DB$data %>% names()
  # if(any(!names(transformation)%in%names(DB$data)))stop("must have all DB$data names in transformation")
  OUT <- NULL
  for(i in (1:nrow(forms_transformation))){

    TABLE <- forms_transformation$instrument_name[i]
    ref <- named_df_list[[TABLE]]
    if(!is.null(ref)){
      a<- forms_transformation[i,]
      z<-as.list(a)
      ref <- named_df_list[[TABLE]]
      rownames(ref)<- NULL
      by.x <- z$by.x <- z$by.x %>% strsplit("\\+") %>% unlist()
      by.y <- z$by.y <- z$by.y %>% strsplit("\\+") %>% unlist()
      if(length(z$by.x) != length(z$by.y) )stop("by.x and by.y must be same length... [",z$instrument_name,"] (",z$by.x %>% as_comma_string(),") AND (",z$by.y %>% as_comma_string(),")")
      if(TABLE == z$merge_to){
        OUT[[z$instrument_name_remap]] <- ref
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
        OUT[[z$instrument_name_remap]] <- a
      }
    }
  }
  if(any(!names(OUT)%in%unique(forms_transformation$instrument_name_remap)))stop("not all names in OUT objext. Something wrong with transform_DB()")
  DB$data <- OUT
  DB$internals$is_transformed <- T
  DB$transformation$original_forms <- DB$metadata$forms
  transformation_edit <- forms_transformation
  transformation_edit$instrument_name <- transformation_edit$instrument_name_remap
  transformation_edit$instrument_label <- transformation_edit$instrument_label_remap
  transformation_edit <- transformation_edit[,c("instrument_name","instrument_label","repeating")] %>% unique()
  DB$metadata$forms <- transformation_edit
  bullet_in_console(paste0(DB$short_name," transformed according to `DB$transformation`"),bullet_type = "v")
  #fields------------
  fields <- DB$transformation$original_fields <- DB$metadata$fields
  fields$form_name <- forms_transformation$instrument_name_remap[match(fields$form_name,forms_transformation$instrument_name)]
  fields <- fields[order(match(fields$form_name,transformation_edit$instrument_name)),]
  DB$metadata$fields <- fields
  DB <- insert_new_fields_to_metadata(DB)
  DB <- run_transformation_fields(DB,ask = ask)
  DB$internals$last_data_transformation <- Sys.time()
  return(DB)
}
get_original_forms <- function(DB){
  forms <- DB$metadata$forms
  if(DB$internals$is_transformed){
    forms <- DB$transformation$original_forms
  }
  return(forms)
}
get_original_fields <- function(DB){
  fields <- DB$metadata$fields
  if(DB$internals$is_transformed){
    fields <- DB$transformation$original_fields
  }
  return(fields)
}
get_transformed_fields_metadata <- function(DB){
  transformed_fields <- NULL
  for(field_name in names(DB$transformation$fields)){
    transformed_fields <- dplyr::bind_rows(transformed_fields,DB$transformation$fields[[field_name]]$field_row)
  }
  return(transformed_fields)
}
insert_new_fields_to_metadata <- function(DB){
  the_names <- names(DB$transformation$fields)
  if(is.null(the_names)){
    bullet_in_console("Nothing to add. Use `add_edit_fields()`",bullet_type = "x")
    return(DB)
  }
  fields <- DB$metadata$fields
  for(field_name in the_names){
    field_row <- DB$transformation$fields[[field_name]]$field_row
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
  fields_original <-
    DB$metadata$fields <- fields
  bullet_in_console(paste0("Added new fields to ",DB$short_name," `DB$metadata$fields`"),bullet_type = "v")
  return(DB)
}
run_transformation_fields <- function(DB,ask = T){
  the_names <- names(DB$transformation$fields)
  if(is.null(the_names)){
    bullet_in_console("Nothing to run. Use `add_edit_fields()`",bullet_type = "x")
    return(DB)
  }
  original_fields <- get_original_fields(DB)
  the_names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  the_names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  fields_to_update <- NULL
  for(field_name in c(the_names_existing,the_names_new)){
    OUT <- NA
    form_name <- DB$transformation$fields[[field_name]]$field_row$form_name
    if(!is.null(DB$transformation$fields[[field_name]]$field_func)){
      OUT <- DB$transformation$fields[[field_name]]$field_func(data_list = DB$data)
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
          DB$data_update$transform[[field_name]] <- DF
        }
      }
    }
    DB$data[[form_name]][[field_name]] <- OUT
  }
  bullet_in_console(paste0("Added new fields to ",DB$short_name," `DB$data`"),bullet_type = "v")
  return(DB)
}
#' @title purify_names_df_list
#' @export
untransform_DB <- function(DB){
  if(!DB$internals$is_transformed){
    bullet_in_console("Already not transformed... nothing to do!",bullet_type = "x")
    return(DB)
  }
  named_df_list <- DB$data
  forms_transformation <- DB$transformation$forms
  original_form_names <- DB$transformation$original_forms$instrument_name
  original_fields <- DB$metadata$fields
  keys <- DB$metadata$form_key_cols
  OUT <- NULL
  if(any(!original_form_names%in%forms_transformation$instrument_name))stop("Must have all original form names in transformation!")
  # TABLE <- original_form_names%>%sample1()
  for(TABLE in original_form_names){
    DF <- named_df_list[[forms_transformation$instrument_name_remap[which(forms_transformation$instrument_name==TABLE)]]]
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
#' @title add_edit_fields
#' @export
add_edit_fields <- function(
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
  # if(!DB$data_transform %>% is_something())stop("Must have transformed data to add new vars.")
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
    field_label_short = field_label
  )
  if(is.null(data_func))warning("if no `data_func` is provided, the column is only added to the metadata",immediate. = T)
  DB$transformation$fields[[field_name]]<-list()
  DB$transformation$fields[[field_name]]$field_row <- field_row
  DB$transformation$fields[[field_name]]$field_func <- data_func
  message("added '",field_name,"' column")
  return(DB)
}
