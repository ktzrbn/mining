#################################################
########### Preprocessing the Corpus ############
#################################################

# Install the following packages 
install.packages('jsonlite') #this will parse JSON files from HathiTrust
install.packages('tidytext') # for text mining
install.packages('dplyr') # data manipulation 
install.packages('tidyr') # data manipulation 
install.packages('tm') # text preprocessing 
install.packages('ggplot2') # data visualization 

# Load the libraries into the environment 
library(jsonlite)
library(tidytext)
library(dplyr)
library(tidyr)
library(tm)
library(ggplot2)

# Begin constructing the corpus in R by loading JSON files from HathiTrust

# Loads all JSON files in the corpus directory
files <- list.files("corpus", pattern = "*.json", full.names = TRUE)

# Bind the JSON files together into a single corpus
# This can take a while depending on the number of files 
corpus_data <- lapply(files, fromJSON) %>% bind_rows()
