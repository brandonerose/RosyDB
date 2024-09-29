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
  transformation_template$by.y <- transformation_template$by.x <- DB$metadata$form_key_cols[[merge_form_name]]
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
    if(!identical(DB$transformation,forms_tranformation)){
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
transform_DB <- function(DB){
  if(DB$internals$is_transformed){
    bullet_in_console("Already transformed... nothing to do!",bullet_type = "x")
    return(DB)
  }
  named_df_list <- DB$data
  transformation <- DB$transformation$forms
  DB$transformation$original_col_names <- DB$data %>% names() %>% lapply(function(l){
    DB$data[[l]] %>% colnames()
  })
  names(DB$transformation$original_col_names) <- DB$data %>% names()
  # if(any(!names(transformation)%in%names(DB$data)))stop("must have all DB$data names in transformation")
  OUT <- NULL
  for(i in (1:nrow(transformation))){
    TABLE <- transformation$instrument_name[i]
    a<- transformation[i,]
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
      a<- merge(
        x = ref,
        y = mer,
        by.x = by.x,
        by.y = by.y,
        all.x = T,
        sort = F
      )
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
  if(any(!names(OUT)%in%unique(transformation$instrument_name_remap)))stop("not all names in OUT objext. Something wrong with transform_DB()")
  DB$data <- OUT
  DB$internals$is_transformed <- T
  DB$transformation$original_forms <- DB$metadata$forms
  transformation2 <- transformation
  transformation2$instrument_name <- transformation2$instrument_name_remap
  transformation2$instrument_label <- transformation2$instrument_label_remap
  transformation2 <- transformation2[,c("instrument_name","instrument_label","repeating")] %>% unique()
  DB$metadata$forms <- transformation2
  bullet_in_console(paste0(DB$short_name," transformed according to `DB$transformation`"),bullet_type = "v")
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
  bullet_in_console(paste0(DB$short_name," untransformed according to `DB$transformation`"),bullet_type = "v")
  return(DB)
}
