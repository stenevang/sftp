# Automatically generate a package help menu
#
# Based on .Rd files found in the /man/ folder of the package source,
# this function will generate a .R-file named like the package, with roxygen
# notation that links to all files found in the /man/ folder. The effect is
# that package users will get a menu to all help documentation in the package
# by typing ?<package>
#
# The function make_help_menu is general and can be moved into any package
# without modification.
#
# WARNING! make_help_menu will generate a file named like <package>.R and
# therefore you MUST NOT put any of the package source code in a file named
# like that.
#
# The function is not exported since it is meant to be used by package
# developers only.
#
# make_help_menu.R can be obtained from
# https://github.com/stenevang/make_help_menu
#
#
# Usage
#
# start by opening the package project
# then do:
# <package>:::make_help_menu()
# # where you replace <package> with the name of your package
# # then do ctrl+shift+B or re-install the package some other way

make_help_menu <- function(man_dir = "man", R_dir = "R") {
  devtools::document() # assure all roxygen help as been created

  package_name <- environmentName(environment(make_help_menu))
  message(paste("Package name is:", package_name))
  helpfiles <- list.files(man_dir)
  helpfiles <- gsub("\\.Rd$", "", helpfiles)
  helpfiles <- paste0("#' \\code{\\link{", helpfiles, "}}\\cr")
  header <- c(paste0("#' ", package_name, " package help menu"),
              "#' ",
              "#' This help menu was automatically generated using",
              "#' the make_help_menu function available on",
              "#' \\url{https://github.com/stenevang/make_help_menu/}",
              "#'")
  footer <- c("#'",
              "#' @docType package",
              paste0("#' @name ", package_name),
              "#' @encoding UTF-8",
              "NULL")
  package_R <- data.frame(file = c(header, helpfiles, footer), stringsAsFactors = F)

  # security check
  answer <- "empty"
  while (!(answer %in% c("yes", "no")) ) {
    answer <- readline(paste0("WARNING! This will overwrite the file ", package_name, ".R - Continue? (yes / no) ") )
  }
  if (answer == "no") {
    message("OK, bye.")
    return(FALSE)
  } else if (answer == "yes") {
    write.table(package_R,
                file = paste0("R/", package_name, ".R"),
                append = F,
                quote = F,
                sep = "",
                eol = "\n",
                row.names = F,
                col.names = F)

    devtools::document() # create roxygen help based on what the code above
                         # has put together in <package_name>.R

    message(paste0(package_name, ".R and ?", package_name, " (", package_name, ".Rd) updated") )
    return(TRUE)
  } else {
    return(FALSE)
  }
}

