# sftp
R package with convenience functions for SFTP operations wrapping RCurl functions

* sftp_connect - create an object with connection credentials
* sftp_list - list files and/or folders on an SFTP URL
* sftp_listfiles - list only files on an SFTP URL
* sftp_listdirs - list only directories on an SFTP URL

* sftp_download - download one or several files to a local folder
* sftp_upload - upload one or several files from a local folder
* sftp_delete - delete one or several files on an SFTP URL
* sftp_rename - rename a file or a directory on an SFTP URL

* sftp_makedir - create a new directory below an SFTP path
* sftp_removedir - remove an existing, empty directory on an SFTP URL
* sftp_changedir - change the directory in which you are standing by modifying the object with connection credentials


### Motivation
RCurl is a very powerful and versatile package, but it is also hard to use due to the vast amount of options and alternatives.  
The sftp package serves the purpose of making it straight-forward and uncomplicated to move files and folders between a local system and an SFTP server. 

### Motto
_Cool data science is sexy, but it is also nice to get hold of the data to begin with_

### TODO

* Add support for keyfiles

### Installation

Get hold of this package by running these lines of code in R:  
```install.packages("devtools") # unless you already have it installed
devtools::install_github("stenevang/sftp")```
