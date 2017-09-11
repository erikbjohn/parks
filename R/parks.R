#' \code{parks} package
#'
#' parks
#'
#' See Vignette
#'
#' @docType package
#' @name streetview
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' @title parks_get
#'
#' @description load and cleans parks shapefiles
#' @param path.root location of current dropbox root Example: ~/Dropbox/pkg.data/
#' @param proj.name Projection to use for project default: denver
#' @param fresh Redo import and clean raw shapes
#' @keywords parks denver load google panoids
#' @export
#' @importFrom pkg.data.paths paths
#' @importFrom api.keys import.key
#' @importFrom lubridate ymd year quarter
#' @importFrom data.table data.table rbindlist
#' @importFrom pkg.data.paths paths
#' @importFrom rgdal readOGR
#' @import methods.shapes
#' @import utils
parks_get <- function(path.root = NULL, proj.name = NULL, fresh=FALSE){
  file.name <- NULL; file.body <- NULL
  # Initialize api and paths
  api.key <- api.keys::import.key(str.api.name = 'google')
  # Load package data from dropbox
  get.parks.path <- pkg.data.paths::paths(path.root = path.root, str.pkg.name = 'parks')
  check <- parks_check(path.root)
  if (!check$raw) stop(paste('parks shapefile parks_project not found in', get.parks.path$pkg.root[1], '/raw/'))
  if (check$clean & !fresh){
    parks.path.clean <- get.parks.path[file.name == 'parks.rdata' & grepl('^clean', file.body, perl = TRUE)]
    load(parks.path.clean$sys.path)
  } else {
    parks_raw <-  rgdal::readOGR(dsn = paste0(path.expand(get.parks.path$pkg.root[[1]]), '/raw/'), layer = "parks_project", verbose = FALSE)
    parks.clean.path <- paste0(path.expand(get.parks.path$pkg.root[[1]]), '/clean/parks.rdata')
    parks <- methods.shapes::clean.shape(x = parks_raw, proj.env.name = proj.name)
    names(parks@data) <- stringr::str_to_lower(names(parks@data))
    # Bring id into data
    parks@data$park_id <- sapply(seq(1,length(parks@polygons)), function(x) slot(parks@polygons[[x]], 'ID'))
    parks <- save(parks, file=parks.clean.path)
  }
   return(parks)
}
#' @title parks_check
#'
#' @description checks parks for raw and clean shapes
#' @param path.root location of current dropbox root Example: ~/Dropbox/pkg.data/
#' @keywords denver parks_check
#' @export
#' @importFrom pkg.data.paths paths
#' @importFrom data.table data.table rbindlist
#' @importFrom pkg.data.paths paths
#' @import utils
parks_check <- function(path.root = NULL){
  file.name <- NULL; file.body <- NULL
  check <- list()
  get.parks.path <- pkg.data.paths::paths(path.root = path.root, str.pkg.name = 'parks')
  check$raw <- nrow(get.parks.path[file.name == 'parks_project.shp' & grepl('^raw', file.body, perl = TRUE)])>0
  check$clean <- nrow(get.parks.path[file.name == 'parks.rdata' & grepl('^clean', file.body, perl = TRUE)])>0
  return(check)
}
