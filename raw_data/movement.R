download_movement <- function(root, redownload = FALSE) {
  dest <- file.path(root, "raw_data", "movement.zip")
  if (file.exists(dest)) {
    if (redownload) {
      unlink(dest)
    } else {
      return(dest)
    }
  }
  ## it's a big old file, we need a long timeout (1h here).  Using
  ## curl, rather than download.file, as it's much more reliable.
  url <- "https://mountainscholar.org/bitstreams/e7237627-74a0-4d9d-a31d-0e6999d68d08/download"
  message("Downloading very large file, this may take a while...")
  curl::curl_download(url, dest, quiet = FALSE, mode = "wb")

  message("\nVerifying file") # curl does not leave trailing newline
  md5_found <- tools::md5sum(dest)
  md5_expected <- "092818bf9b1120da8e432d189c19fe5e"
  if (md5_found != md5_expected) {
    stop("Unexpected file download, the hash does not match")
  }
  dest
}


process_movement <- function(root, redownload = FALSE) {
  path <- download_movement(root, redownload)
  ## ... your processing here, return movement data
}
