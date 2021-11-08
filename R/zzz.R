# run at package attachment by library()
.onAttach <- function(libname, pkgname){
    packageStartupMessage("Package sftp v 2.0.2 by Theodor Stenevang Klemming\nUse ?sftp or sftp() for a menu of functions")
}
