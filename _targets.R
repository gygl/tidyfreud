library(targets)
library(pdftools)
library(tidyverse)
library(tidytext)
# source functions
source("./R/create-sigmund-freud-complete-work-tibble.R")
# target list
list(
  tar_target(path_pdf, "data/Freud_Complete_Works.pdf", format = "file")
  , tar_target(freud, create_sfreud_complete_work_tibble(path_pdf))
  , tar_target(freud_page, freud$by_page)
  , tar_target(freud_line, freud$by_line)
  , tar_target(freud_word, freud_line %>%
                 unnest_tokens(word, text) %>%
                 anti_join(stop_words))
  , tar_target(use_data_for_pkg, use_data(freud_page, freud_line, freud_word))
)
