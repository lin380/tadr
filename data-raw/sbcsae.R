
# ABOUT -------------------------------------------------------------------

# Description: Acquire and curate the Santa Barbara Corpus of Spoken American English
# Usage: Internet connection required
# Author: Jerid Francom
# Date: 2019-06-21


# SETUP -------------------------------------------------------------------

pacman::p_load(tidyverse, usethis, tadr)


# RUN ---------------------------------------------------------------------



usethis::use_data("sbcsae")


# CLEANUP -----------------------------------------------------------------

rm(list = ls()) # clean up objects
