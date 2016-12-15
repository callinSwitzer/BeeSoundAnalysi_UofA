


# install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("tuneR", "ggplot2", "compiler", "manipulate", "shiny", "seewave")
ipak(packages)
