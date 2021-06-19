
# ABOUT -------------------------------------------------------------------

# Description: Acquire and curate the Switchboard Dialog Act Corpus
# Usage: Internet connection required
# Author: Jerid Francom
# Date: 2018-01-28

# SETUP -------------------------------------------------------------------

pacman::p_load(tidyverse, usethis, tadr)

# _ Functions -------------------------------------------------------------

extract_swda_metadata <- function(file) {
  # Function: to read a Switchboard Corpus Dialogue file (.utt) and extract
  # various metadata features and text annotations

  cat("Reading", basename(file), "...")
  doc <- read_lines(file) # read file by lines

  # Header info -------------------------------------------------------------

  # Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
  doc_speaker_info <-
    doc[str_detect(doc, "\\d+_\\d+_\\d+")] %>% # isolate pattern
    str_extract("\\d+_\\d+_\\d+") %>% # extract the pattern
    str_split(pattern = "_") %>% # split the character vector
    unlist() # flatten the list to a character vector
  doc_id <- doc_speaker_info[1] # extract `doc_id`
  speaker_a_id <- doc_speaker_info[2] # extract `speaker_a_id`
  speaker_b_id <- doc_speaker_info[3] # extract `speaker_b_id`

  # Extract `topic_num`
  topic_num <- doc[str_detect(doc, "^TOPIC#")] %>% str_extract("\\d+") %>% as.double()

  # Extract `topicality`
  topicality <- doc[str_detect(doc, "^TOPICALITY")] %>% str_extract("\\d+")

  # Extract `naturalness`
  naturalness <- doc[str_detect(doc, "^NATURALNESS")] %>% str_extract("\\d+")

  # Text info ---------------------------------------------------------------

  # Extract `text`
  text_start_index <- # find where header info stops
    doc %>%
    str_detect(pattern = "={3,}") %>% # match 3 or more `=`
    which() # find vector index

  text_start_index <- text_start_index + 1 # increment index by 1
  text_end_index <- length(doc)

  text <- doc[text_start_index:text_end_index] # extract text
  text <- str_trim(text) # remove leading and trailing whitespace
  text <- text[text != ""] # remove blank lines

  # tidy format `doc_id`, `topic_num`, `topicality`, `naturalness` and `text`
  data <- data.frame(doc_id, topic_num, topicality, naturalness, text)

  data <- # extract column information from `text`
    data %>%
    mutate(damsl_tag = str_extract(string = text, pattern = "^.+?\\s")) %>%  # extract damsl tags
    mutate(speaker_turn = str_extract(string = text, pattern = "[AB]\\.\\d+")) %>% # extract speaker_turn pairs
    mutate(utterance_num = str_extract(string = text, pattern = "utt\\d+")) %>% # extract utterance number
    mutate(utterance_text = str_extract(string = text, pattern = ":.+$")) %>%  # extract utterance text
    select(-text)

  data <-
    data %>%
    separate(col = speaker_turn, into = c("speaker", "turn_num")) # separate speaker_turn into distinct columns

  data <- # clean up column information
    data %>%
    mutate(damsl_tag = str_trim(damsl_tag)) %>% # remove leading/ trailing whitespace
    mutate(utterance_num = str_replace(string = utterance_num, pattern = "utt", replacement = "")) %>% # remove 'utt'
    mutate(utterance_text = str_replace(string = utterance_text, pattern = ":\\s", replacement = "")) %>% # remove ': '
    mutate(utterance_text = str_trim(utterance_text)) # trim leading/ trailing whitespace

  data <- # link speaker with speaker_id
    data %>%
    mutate(speaker_id = case_when(
      speaker == "A" ~ as.numeric(speaker_a_id),
      speaker == "B" ~ as.numeric(speaker_b_id)
    ))
  cat(" done.\n")
  return(data) # return the data frame object
}

# RUN ---------------------------------------------------------------------

# Download SWitchboard Dialog Act Corpus (SWDA) ---------------------------

# Resource information: https://catalog.ldc.upenn.edu/LDC97S62
# Download corpus annotations
get_compressed_data(url = "https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz", target_dir = "data-raw/original/swda/")

# Tidy SWDA language data -------------------------------------------------

# Get the paths to the corpus files
files <-
  list.files(path = "data-raw/original/swda",
             pattern = "\\.utt",
             full.names = TRUE,
             recursive = TRUE)

# Read files and return a tidy dataset
swda <-
  files %>% # pass file names
  map(extract_swda_metadata) %>% # read and tidy iteratively
  bind_rows() # bind the results into a single data frame

### NOTE: speaker_id with value `155` is an annotation error. Checking the `swda_speaker_meta` and the `swda` (at this point) shows that there is a `1155` value in the `swda` and `swda_speaker_meta` but not a `1555`. So it appears that the annotator left off the trailing `5`.

swda <-
  swda %>%
  mutate(speaker_id = case_when(
    speaker_id == 155 ~ 1555, # change from 155 to 1555
    TRUE ~ as.numeric(speaker_id) #
  ))


# Tidy SWDA speaker meta-data ---------------------------------------------

swda_speaker_meta <-
  read_csv(file = "https://catalog.ldc.upenn.edu/docs/LDC97S62/caller_tab.csv",
           col_names = c("speaker_id", # changed from `caller_no`
                         "pin",
                         "target",
                         "sex",
                         "birth_year",
                         "dialect_area",
                         "education",
                         "ti",
                         "payment_type",
                         "amt_pd",
                         "con",
                         "remarks",
                         "calls_deleted",
                         "speaker_partition"))

swda_speaker_meta <- # remove double quotes
  swda_speaker_meta %>%
  map(str_replace_all, pattern = '"', replacement = '') %>% # iteratively replace doubled quotes
  bind_rows() %>%  # combine the results by rows
  type_convert() # return columns to orignal data types

glimpse(swda_speaker_meta) # preview the dataset

swda_speaker_meta <- # select columns of interest
  swda_speaker_meta %>%
  select(speaker_id, sex, birth_year, dialect_area, education)

# Join `sdac` with `sdac_speaker_meta` by `speaker_id`
swda <- left_join(swda, swda_speaker_meta) # join by `speaker_id`

glimpse(swda) # preview the joined dataset


# Tidy SWDA topic meta-data -----------------------------------------------

swda_topic_meta <-
  read_csv(file = "https://catalog.ldc.upenn.edu/docs/LDC97S62/topic_tab.csv",
         col_names = c("topic", "topic_num", "topic_prompt", "x", "y", "z")) %>%
  mutate(topic = str_to_title(topic)) %>% # Change case to title
  mutate(topic_prompt = str_to_sentence(topic_prompt)) %>% # Change case to sentence
  select(topic_num, topic, topic_prompt)

glimpse(swda_topic_meta) # preview the dataset

# Join `sdac` with `sdac_topic_meta` by `speaker_id`
swda <- left_join(swda, swda_topic_meta) # join by `topic_num`

# Diagnostics
swda[!complete.cases(swda), ] %>% glimpse # view incomplete cases

swda <- as_tibble(swda)

# Write data to disk ------------------------------------------------------

# Write the curated dataset to the `data-raw/derived/` directory
write_csv(x = swda, path = "data-raw/derived/swda.csv")

# Write the curated dataset to the `inst/extdata/` directory
write_csv(x = brown, path = "inst/extdata/swda.csv")

# Write the curated dataset to the `data/` directory
usethis::use_data(swda)

# CLEANUP -----------------------------------------------------------------

rm(list = ls()) # clean up objects
