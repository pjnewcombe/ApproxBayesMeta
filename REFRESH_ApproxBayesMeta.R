package.name <- 'ApproxBayesMeta'
package.location <- '/Users/pauln/Dropbox/Work Projects/R Packages'
github.username <-'pjnewcombe'
commit.message <- NULL
#commit.message <- "Edited simulation functions for 100 block design."

### --- Load Pmisc and refresh the package
library(Pmisc)
RefreshPackage(package.name=package.name,commit.message=commit.message)

do.not.run <- FALSE
if (do.not.run) {
  library(devtools)
  install_github(username="pjnewcombe", repo="ApproxBayesMeta")
}
