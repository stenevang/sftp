
# internal function - not exported #############################################
trim_slashes <- function(string) {
    string <- gsub("^/*", "", string) # remove any leading slash
    string <- gsub("/*$", "", string) # remove any trailing slash
    return(string)
} # end of internal function trim_slashes ######################################



# internal function - not exported #############################################
sftp_log <- function(message,
                     log_file) {
    readr::write_lines(x = paste(Sys.time(),
                                 message, "\r"),
                       path = log_file,
                       append = T)
}
################################################################################


# internal function - not exported #############################################
check_libcurl <- function() {
    your_libcurl <- RCurl::curlVersion()
    if ("sftp" %in% your_libcurl$protocols) {
        return("OK")
    }
    return(paste0("This system has libcurl version ",
                   your_libcurl$version,
                   " and SFTP is not among the supported protocols. ",
                   "Run 'Rcurl::curlVersion()' for full details."))
}
################################################################################

