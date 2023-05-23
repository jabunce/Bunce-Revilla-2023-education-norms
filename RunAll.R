# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library(rethinking)
library(lattice)
library(MASS) #for mvrnorm function
library(graphics)
library(grid)
library(boot) #for inverse logit function
library(shape) #for arrows function

library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr) # pull function to extract data from model fit "tibbles"


# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/boarding_school_paper_2023/analysis/for_Github")
# on pc
#setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/boarding_school_paper_2023/analysis/for_Github")
# on server
#setwd("~/growth_23jun22/analysis_height_mal_21jun22")


# Load helper functions
source("./Code/Functions.R")

# Read data from the csv into R:
d.school.r <- read.csv(file="./Data/School_data.csv",header=TRUE)

# Fit model
number_samps <- 3000
number_chains <- 6

source("./Code/FitPredRolModelAx.R")


# Make plots
source("./Code/PlotPredRolResultsAx.R")
