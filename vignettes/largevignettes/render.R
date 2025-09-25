setwd("~/projects/bioimagetools/vignettes/largevignettes/")
rmarkdown::render("readwriteplot.Rmd")
file.copy("readwriteplot.html","../")
setwd("~/projects/bioimagetools/")
