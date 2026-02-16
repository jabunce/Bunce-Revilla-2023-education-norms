# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library(rstan)
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
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/boarding_school_paper_2023/analysis/for_Github2")
# on pc
setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/boarding_school_paper_2023/analysis/for_Github2")
# on server
#setwd("~/growth_23jun22/analysis_height_mal_21jun22")



# Load helper functions
source("./Code/Functions.R")

# Load data  
source("./Code/PrepareData.R")


samps <- 3000
num_chains <- 6


# model with just random effect for person with covariance between years
# Figs 4 and 5 in appendix
source("./Code/FitCovModel.R") 
source("./Code/PlotCovResults.R") 


# model with covarying random effects for person, school, and role, predictor for grade, different discriminations for students and teachers
# Figs 2 and 3 in main text, Figs 1, 2, 3 in appendix
source("./Code/FitPredRolModelAx.R")
source("./Code/PlotPredRolResultsAx.R")


# model with random effect fpr person, school and community data
# Fig 1 in main text
source("./Code/SchoolComm.R")
