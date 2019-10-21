# run at package load
.onLoad <- function(libname, pkgname){

    # lazy load packages
    require(RCurl)
    require(tidyr)

    # welcome
    message("package sftp v 2.0.1 by Theodor Stenevang Klemming")
}
