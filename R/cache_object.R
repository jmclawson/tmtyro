cache_exists <- function(object, dir = "data") {
  if (is.name(substitute(object))) {
    obj_name <- deparse(substitute(object))
  } else if (is.character(object)) {
    obj_name <- object
  }
  rds_path <- paste0(dir, "/", obj_name, ".rds")
  file.exists(rds_path)
}

cache_object <- function(object, dir = "data", replace = FALSE, obj_name = NULL) {
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(object))
  }
  rds_path <- paste0(dir, "/", obj_name, ".rds")
  if(any(!file.exists(rds_path), replace)){
    saveRDS(object, rds_path)
  }
  invisible(object)
}

cache_get <- function(object, dir = "data") {
  if (is.name(substitute(object))) {
    obj_name <- deparse(substitute(object))
  } else if (is.character(object)) {
    obj_name <- object
  }
  rds_path <- paste0(dir, "/", obj_name, ".rds")
  if(file.exists(rds_path)) {
    readRDS(rds_path)
  } else {
    stop(paste0("No RDS file exists at ", rds_path, "."))
  }
}
