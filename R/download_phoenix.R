#' Download the Phoenix Dataset
#'
#' Download and unzip all of the data files for the Phoenix dataset from the 
#' Phoenix data website into a given directory.
#'
#' @param destpath The path to the directory where Phoenix should go.
#' @param phoenix_version. Download a specific version of Phoenix ("v0.1.0" or "current").
#'
#' @return NULL
#' @author Andy Halterman
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' download_phoenix("~/OEDA/phoxy_test/", phoenix_version = "current")
#'
#' @rdname download_phoenix


# get all the URLs on a page
get_links <- function (version = 'current') {
  require(XML)
  url <- paste0('http://phoenixdata.org/data/', version)
  page <- htmlParse(url)
  all_links <- as.vector(xpathSApply(page, "//a/@href")) # xpath to extract strings
  links = all_links[grepl('zip$', all_links)] # only links ending with "zip"
  
  return(links)
}

# given a link, download the file and write it to the specified directory
dw_file <- function(link, destpath, phoenix_version) {
  version_nodots <- gsub(".", "", phoenix_version, fixed=T)
  'v'
  baseurl <- paste0("https://s3.amazonaws.com/oeda/data/", version_nodots, "/")
  filename <- gsub(baseurl, "", link)
  filename <- paste0(destpath, filename)
  bin <- getBinaryURL(link, ssl.verifypeer=FALSE)
  con <- file(filename, open = "wb")
  writeBin(bin, con)
  close(con)
  unzip(filename, exdir = destpath, unzip = "internal", setTimes = FALSE)
  unlink(filename)
}

#' @export
#' @importFrom plyr l_ply progress_text
download_phoenix <- function(destpath, phoenix_version = "current"){
  library(RCurl)
  if (stringr::str_sub(destpath, -1) != "/"){
    stop("Destination paths need to have trailing forward slashes")
  }
  ll <- get_links(phoenix_version = phoenix_version)
  message("Downloading and unzipping files.")
  plyr::l_ply(ll, dw_file, phoenix_version = phoenix_version, destpath = destpath, .progress = plyr::progress_text(char = '='))
}





