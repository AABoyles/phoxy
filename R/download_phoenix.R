#' Download the Phoenix Dataset
#'
#' Download and unzip all of the data files for the Phoenix dataset from the 
#' Phoenix data website into a given directory.
#'
#' @param destpath The path to the directory where Phoenix should go.
#' @param v. Download a specific version of Phoenix ("v0.1.0" or the current version by default).
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
get_links <- function (v = 'current') {
  require(XML) # to parse HTML
  
  v <- gsub('.', '', v, fixed = T) # remove dots
  
  # check version user input, either 'current' or up to 3 digits
  # with optional 'v' in the beginning
  if (!grepl('(current|v?\\d{,3})', v)) stop('Incorrect version name.')
  
  if (!grepl('^(v|current)', v)) { # if the user submitted a version without 'v'
    v <- paste0('v', v)
  }
  
  url <- paste0('http://phoenixdata.org/data/', v)
  page <- htmlParse(url)
  all_links <- as.vector(xpathSApply(page, "//a/@href")) # xpath to extract url strings
  links <- all_links[grepl('zip$', all_links)] # only links ending with "zip"
  
  return(links)
}

# given a link, download the file and write it to the specified directory
dw_file <- function(link, destpath) {
  # extract filename from link
  m <- regexpr('events\\.full\\.\\d{8}\\.txt', link)
  filename <- regmatches(link, m)
  
  # add trailing '/' to destpath if it's not there
  if (!grepl('/$', destpath)) destpath <- paste0(destpath, '/')
  
  # download and unzip to destpath
  temp <- tempfile()
  download.file(link, temp, method = 'curl', quiet = T)
  unzip(temp, exdir = destpath)
  unlink(temp)
}

#' @export
#' @importFrom plyr l_ply progress_text
download_phoenix <- function(destpath, v = 'current'){
  links <- get_links(v = v)
  message("Downloading and unzipping files.")
  plyr::l_ply(links, dw_file, destpath = destpath, .progress = plyr::progress_text(char = '='))
}