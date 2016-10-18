.libPaths(c(.libPaths(),"C:/new = rmodules"))
.libPaths("C:/new")
library(Rcpp)
install.packages("shiny")
require(shiny)
folder_address = 'E:/Projects/app2'
runApp(folder_address, launch.browser=TRUE)
cd()
