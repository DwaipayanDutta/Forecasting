rm(list=ls())
# Loading required libraries and checking the installs
# Method 1
loadlibraries <- c("tidyverse","caTools","caret","DT","export","ggplot2","scales",
                   "randomForest","dplyr","plyr","skimr","RColorBrewer","GGally")
installlib <- loadlibraries[!loadlibraries %in% installed.packages()]
for(libs in installlib) install.packages(libs, dependences = TRUE)
suppressPackageStartupMessages(sapply(loadlibraries, require, character = TRUE))

# Method 2
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages<-c("MASS", "GGally", "car", "tree", "adabag", "randomForest",
            "e1071", "neuralnet", "ggplot2","ggrepel")
check.packages(packages)