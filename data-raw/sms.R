# ABOUT -------------------------------------------------------------------

# Description: Aquire data for SMS Spam Collection
# Usage: Source this code with an internet connection
# Author: Jerid Francom
# Date: 2021-06-18

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(tadr)

# RUN ---------------------------------------------------------------------

# Download/ decompress data
# Source: http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
get_compressed_data(url = "http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/smsspamcollection.zip", target_dir = "data-raw/original/sms/")

# Read data
sms <-
  read_tsv(file = "data-raw/original/sms/SMSSpamCollection.txt", col_names = c("sms_type", "message"), trim_ws = TRUE)

# Write data
write_csv(sms, file = "data-raw/derived/sms.csv", col_names = TRUE)

usethis::use_data(sms, overwrite = TRUE)
