#########
# Packages
library(remotes)
library(dplyr)
library(data.table)
library(here)
library(lubridate)

conf = list()

#########
# RPDR-ML

# Which directory/version of PRDR-ML to use.
conf$rpdrml_version = "2024.1"
dir_rpdrml = here(paste0("data-raw/RPDRml_v", conf$rpdrml_version, "/"))
# This directory is presumably a symbolic link from within the project to
# where RPDR-ML is stored.
cat("RPDR-ML version:", conf$rpdrml_version, "Dir:", dir_rpdrml, "\n")


####
# Multithreading
# Use half of the available cores by default, but no more than 64.
conf$threads = min(64, floor(RhpcBLASctl::get_num_cores() / 2))

data.table::setDTthreads(conf$threads)
cat("Data.table threads:", data.table::getDTthreads(), "\n")

fst::threads_fst(conf$threads)
