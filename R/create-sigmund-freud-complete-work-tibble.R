#' Take path of the pdf with complete work of Sigmund Freud and make of it a structured dataset
#' the source of the pdf is: https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf
#'
#' @param path_pdf string, path to pdf
#'
#' @return list, contains 3 dataframes with text by page, by line and by words
#' @export
#'
create_sfreud_complete_work_tibble = function(path_pdf) {

    # read text and table of content
    txt = pdftools::pdf_text(path_pdf)
    toc = pdftools::pdf_toc(path_pdf)
    titles = unlist(unlist(unname(toc$children[[1]])))

    # create tibble with text by page (each row is 1 page)
    text_by_page = tibble(pg_all = 1:length(txt), text = map_chr(txt,1))

    # create tibble with text by line (each row is one line)
    text_by_line =
      # split by line
      map(txt, ~str_split(.x, "\n")) %>%
      # put in a tibble
      enframe(name = "page", value = "text") %>%
      # open list with text
      unnest(text) %>% unnest(text) %>%
      # remove leading and trailing whitespaces
      mutate(text = trimws(text)) %>%
      # filter empty line and page intentiionally left blank
      filter(nchar(text) > 1 & text != "Intentionally left blank") %>%
      # remove title and authorship full pdf
      slice(-c(1,2)) %>%
      mutate(
        # detect if it's likely to be a title
        ## "^[^a-z]*$" ensures that the entire string does not contain any lowercase alphabetic characters.
        ##  "[A-Z]" matches any single alphabetic character
        is_title = str_detect(text, "^[^a-z]*$") & str_detect(text, "[A-Z]"),
        # detect if the line contains only a number, likely a page number
        is_page_nb = str_detect(text, "^\\d+$"),
        # set a variable to get title
        title = NA_character_)

    # create with all titles as listed as section in the pdf
    titles_tbl = titles %>% enframe(name = "index", value = "titles") %>%
      # simplify title for pattern matching
      mutate(titles_simpl = simplify_title(titles))


    # create tmp tibble with line likely being a title
    tmp = text_by_line[text_by_line$is_title, ]
    # create index
    tmp$index = 1:nrow(tmp)
    # simplify text likely being a title for pattern matching
    tmp$title_simpl = simplify_title(tmp$text)

    # loop over each potential title
    for (i in 1:nrow(tmp)) {
      # try to match it with a known title
      ind = which(titles_tbl$titles_simpl == tmp$title_simpl[i])
      # if 1 match
      if (length(ind) == 1) {
        tmp$title[i] = titles_tbl$titles[ind]
      } else if (length(ind) > 2) {
        cli::cli_alert_warning(c(
          "multiples (n={length(ind)}) matches!"
        ))
      }
    }

    # replace tmp in tibble text by line
    text_by_line[text_by_line$is_title, ] =
      # remove 2 additional variables
      tmp[, -which(names(tmp) %in% c("index", "title_simpl"))]

    # create again a tibble with likely title, not matched yet
    tmp = text_by_line[text_by_line$is_title & is.na(text_by_line$title), ]
    # create index
    tmp$index = 1:nrow(tmp)
    # simplify text likely being a title for pattern matching
    tmp$title_simpl = simplify_title(tmp$text)

    # non-matched titles: setdiff(titles_tbl$titles, text_by_line$title %>% unique)
    # this manual index was produced by hand by searching non-matched titles in tmp
    manual_index = c(61, 63, 347, 349, 380, 382, 384, 386, 390, 392, 445, 447, 450,
            491, 529, 531, 550, 552, 737, 790, 791, 793, 817)
    # repeat process of match
    for (i in manual_index) {
      # match title by partial string matching
      ind = which(str_detect(titles_tbl$titles_simpl, tmp$title_simpl[i]))
      # it there's a hit then attribute title
      if (length(ind) == 1){
        tmp$title[i] = titles_tbl$titles[ind]
      } else if (length(ind) > 2) {
        cli::cli_alert_warning(c(
          "multiples (n={length(ind)}) matches!"
        ))
      }
    }

    # replace tmp in tibble text by line
    text_by_line[text_by_line$is_title  & is.na(text_by_line$title), ] =
      # remove 2 additional variables
      tmp[, -which(names(tmp) %in% c("index", "title_simpl"))]

    # non-matched titles: setdiff(titles_tbl$titles, text_by_line$title %>% unique)
    # perform last matching completely by hand
    text_by_line$title[text_by_line$text == "DR. SÁNDOR FERENCZI" & text_by_line$page == 4183] = "Dr. Sandor Ferenczi (On His 50th Birthday) (1923i)"
    text_by_line$title[text_by_line$text == "SOME PSYCHICAL CONSEQUENCES OF THE ANATOMICAL DISTINCTION"] = "Some Psychical Consequences Of The Anatomical Distinction Between The Sexes (1925j)"

    # non-matched titles: setdiff(titles_tbl$titles, text_by_line$title %>% unique) => perfect only Chronological left, that is expected !

    # finish
    text_by_line = text_by_line %>% mutate(
      # transform title to factor to make it lighter
      title = factor(title, levels = titles_tbl$titles)
      # extract date(s) from title
      , date = str_extract_all(title, "\\d{4}")
      # first date is always publication date
      , publi_yr= map_if(date, ~length(.x) > 0, 1)
      # if a second date, represent wiriting date
      , writ_yr = map_if(date, ~length(.x) > 1, 2)
      # unlist
    ) %>% unnest(c(publi_yr, writ_yr)) %>%
      mutate(publi_yr = as.integer(publi_yr),
             writ_yr = as.integer(writ_yr)) %>%
      select(-date) %>%
      rename("pg_all" = "page") %>%
      mutate(is_subtitle = is_title & is.na(title)) %>%
      # fill downward title name, and dates
      tidyr::fill(title, publi_yr, writ_yr) %>%
      # filter out line that only contains page number
      filter(!is_page_nb) %>% select(-is_page_nb) %>%
      mutate(line_all = as.integer(1:n())) %>%
      # create line index at the title (book or article level)
      group_by(pg_all) %>%
      mutate(line_pg = as.integer(1:n())) %>%
      ungroup() %>%
      relocate(pg_all, line_all, text)

    # find what is a subtitle or not by manual inspection
    tmp_title_not_found = text_by_line[text_by_line$is_subtitle, ]

    # ist not a title
    index_line_all_not_a_subtitle =
      c(28, 41, 53, 58, 62, 412, 416, 4395, 4399, 6033, 6037, 33535, 41936:41938,
        42012, 42016, 42020, 42021, 42221, 42223, 43130, 51254, 51273, 51254,
        51273, 51632, 56404, 56405, 56409, 93887, 93895, 93896, 93906, 104275,
        104343, 114566)
    # is a signature
    signature = c(28, 41, 53, 58, 62, 412, 416, 4395, 4399, 6033, 6037, 51254,
                  51273, 93895, 93906, 104275, 104343, 114566)
    # line to just remove
    index_line_to_remove = c(104085)

    text_by_line$is_subtitle[text_by_line$line_all %in% index_line_all_not_a_subtitle] = FALSE
    text_by_line$is_signature = FALSE
    text_by_line$is_signature[text_by_line$line_all %in% signature] = TRUE
    text_by_line = text_by_line %>% filter(line_all != index_line_to_remove)
    text_by_line = text_by_line[-index_line_to_remove,]

    # concatenation of consecutive subtitles
    indices_concat = concatenate_consecutive_subtitles(text_by_line$is_subtitle, text_by_line$text)
    text_by_line$subtitle = NA_character_
    text_by_line$subtitle[text_by_line$is_subtitle] = text_by_line$text[text_by_line$is_subtitle]
    text_by_line$subtitle[indices_concat$starting_indice] =
      indices_concat$consecutive_sequences
    text_by_line = text_by_line[-indices_concat$consecutive_indices_excluding_start, ]

    pattern_detect_date_letters =
      c("VIENNA", "BUDAPEST", "January 1", "February 1", "March 1", "April 1", "May 1", "June 1",
        "July 1", "August 1", "September 1", "October 1", "November 1", "December 1")
    text_by_line$is_date_letters =
      str_detect(text_by_line$text, paste0("^", pattern_detect_date_letters, collapse="|")) &
      nchar(text_by_line$text) < 35
    text_by_line$date_letters = NA_character_
    text_by_line$date_letters[text_by_line$is_date_letters] =
      text_by_line$text[text_by_line$is_date_letters]


    # final preparation text by line
    text_by_line = text_by_line %>%
      mutate(signature = ifelse(is_signature, text, NA_character_),
             # reformat signture
             signature = tolower(str_remove(str_remove(signature, "^\\("), "\\)$")),
             # make both signature and subtitle factors to make it lighter
             signature = factor(signature, levels = sort(unique(.data$signature))),
             subtitle = factor(subtitle, levels = sort(unique(text_by_line$subtitle)))) %>%
      group_by(title) %>%
      # fill rows with subtitle and signature
      fill(subtitle) %>%
      group_by(title, subtitle) %>% fill(signature, .direction = "up") %>%
      fill(date_letters, .direction = "up") %>%
      ungroup() %>%
      filter(line_pg != 1) %>% mutate(line_pg = line_pg -1) %>%
      filter(!is_title & !is_subtitle & !is_signature & !is_date_letters) %>%
      filter(!str_detect(text, "^(?!.*[a-zA-Z]).*$")) %>%
      select(pg_all, line_all, publi_yr, writ_yr, title,line_pg, subtitle, text, signature, date_letters)

  # put in sentences
  text_by_sntnce = text_by_line %>%
    group_by(pg_all, publi_yr, writ_yr, title, subtitle, signature, date_letters) %>%
    summarize(text = paste(text, collapse = " ")) %>%
    mutate(sentence = map(text, ~(str_split(.x, "(?<=[\\.])")))) %>%
    unnest(sentence) %>% unnest(sentence) %>%
    mutate(sentence = trimws(sentence)) %>%
    filter(nchar(sentence) != 0) %>%
    group_by(pg_all) %>% mutate(sntce_nb = 1:n()) %>%
    ungroup() %>%
    mutate(pg_title = pg_all - min(pg_all) + 1) %>%
    select(pg_all, publi_yr, writ_yr, title, pg_title, sntce_nb, subtitle, sentence, signature, date_letters)


  # by word
  text_by_word = text_by_sntnce %>%
    # tokenize to words
    unnest_tokens(word, sentence) %>%
    left_join(
      stop_words %>% select(-lexicon) %>%
        mutate(is_stop_word = TRUE) %>%
        distinct) %>%
    # remove digits only
    filter(str_detect(word, "^\\d+$", negate=TRUE)) %>%
    mutate(is_stop_word = ifelse(is.na(is_stop_word), FALSE, is_stop_word))

  # setdiff(text_by_line$title %>% unique, titles_tbl$titles)
  # setdiff(titles_tbl$titles, text_by_line$title %>% unique)

  return(list(
    by_page = text_by_page
    , by_sntnce = text_by_sntnce
    , by_word = text_by_word
    ))

}

#' Simplify title by removing last date in parenthesis, any punctuation and accent
#'
#' @param title string, title of book or article in Freud complete work
#'
#' @return string
#' @export
#'
#' @examples
#' simplify_title('Dreams In Folklore (1957a [1911])')
simplify_title = function(title) {

  tmp = trimws(tolower(stringr::str_remove(title, "\\(\\d{4}.*")))
  tmp = stringr::str_remove_all(tmp, "[[:punct:]]")
  tmp = stringi::stri_trans_general(tmp,  id = "Latin-ASCII")
  return(tmp)

}

#' write a page and the following page to a text file and open the text file
#' Note: was used to for verification purpose
#'
#' @param text_by_page data.frame with 2 columns: 1. page number 2. text
#' @param page integer, page number
#'
#' @return nothing
#' @export
#'
display_a_page = function(text_by_page, page) {

  writeLines(text = c(text_by_page$text[text_by_page$pg_all %in% c(page, page + 1)]), con = "display_page.txt")
  file.edit("display_page.txt")

}


#' Save tibble with freud complete work in tidy format to make it available in the package tidyfreud
#'
#' @param freud_page tibble, freud complete work by page
#' @param freud_line tibble, freud complete work by line
#' @param freud_word tibble, freud complete work by word
#'
#' @return nothing
#' @export
#'
use_data = function(freud_page, freud_sntnce, freud_word) {

  dfile = list.files(path = "./data/", pattern = "^freud_.*.rda$", full.names = TRUE)
  if (length(dfile) > 0) map(dfile, file.remove)

  usethis::use_data(freud_page)
  usethis::use_data(freud_sntnce)
  usethis::use_data(freud_word)


}

#' Take av vector of logical, detect when 2 consecutive TRUE, and concatenate the
#' character at the corresponding position.
#'
#' @param is_subtitle logical
#' @param text character
#'
#' @return list, with concat character, starting indices, ending indice and list with what was concatenated for check purpose
#' @export
#'
concatenate_consecutive_subtitles = function(is_subtitle, text) {

  # Sample logical vector
  logical_vector <- is_subtitle

  # Corresponding text vector
  text_vector <- text

  # Identify positions of TRUE values
  positions <- which(logical_vector)

  # Pre-allocate lists based on the maximum possible number of sequences
  max_sequences <- floor(length(positions) / 2)

  consecutive_sequences <- vector("list", max_sequences)
  starting_indices <- vector("list", max_sequences)
  consecutive_indices_excluding_start <- vector("list", max_sequences)
  concatenated_elements <- vector("list", max_sequences)

  # Initialize variables to keep track of current sequence and index for storing results
  current_sequence <- c()
  result_index <- 1

  for (i in seq_along(positions)) {
    if (i > 1 && positions[i] == positions[i - 1] + 1) {
      # If current position is consecutive to the previous one, add to the sequence
      current_sequence <- c(current_sequence, positions[i])
    } else {
      # If not, check the length of the current sequence
      if (length(current_sequence) == 2) {
        consecutive_sequences[[result_index]] <- paste(text_vector[current_sequence], collapse = " ")
        starting_indices[[result_index]] <- current_sequence[1]
        consecutive_indices_excluding_start[[result_index]] <- current_sequence[-1]
        concatenated_elements[[result_index]] <- text_vector[current_sequence]
        result_index <- result_index + 1
      }
      # Start a new sequence
      current_sequence <- positions[i]
    }
  }

  # Check the last sequence
  if (length(current_sequence) == 2) {
    consecutive_sequences[[result_index]] <- paste(text_vector[current_sequence], collapse = " ")
    starting_indices[[result_index]] <- current_sequence[1]
    consecutive_indices_excluding_start[[result_index]] <- current_sequence[-1]
    concatenated_elements[[result_index]] <- text_vector[current_sequence]
    result_index <- result_index + 1
  }

  # Remove unused list elements (those that are still NULL)
  consecutive_sequences <- consecutive_sequences[!sapply(consecutive_sequences, is.null)]
  starting_indices <- starting_indices[!sapply(starting_indices, is.null)]
  consecutive_indices_excluding_start <- consecutive_indices_excluding_start[!sapply(consecutive_indices_excluding_start, is.null)]
  concatenated_elements <- concatenated_elements[!sapply(concatenated_elements, is.null)]

  return(list(
    consecutive_sequences = unlist(consecutive_sequences)
    , starting_indices = unlist(starting_indices)
    , consecutive_indices_excluding_start = unlist(consecutive_indices_excluding_start)
    , concatenated_elements = concatenated_elements
  ))

}
