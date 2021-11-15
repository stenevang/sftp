
# internal function - not exported #############################################
trim_slashes <- function(string) {
    string <- gsub("^/*", "", string) # remove any leading slash
    string <- gsub("/*$", "", string) # remove any trailing slash
    return(string)
} # end of internal function trim_slashes ######################################



# INTERNAL FUNCTION ############################################################

sftp_log <- function(message,
                     log_file) {
    readr::write_lines(x = paste(Sys.time(), message, "\r"), path = log_file, append = T)
}
################################################################################
