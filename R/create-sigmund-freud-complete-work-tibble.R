#' Take path of the pdf with complete work of Sigmund Freud and make of it a structured dataset
#' the source of the pdf is: https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf
#'
#' @param path_pdf string, path to pdf
#'
#' @return list, contains 2 dataframe with text by page and text by line
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
    # fill downward title name, and dates
    tidyr::fill(title, publi_yr, writ_yr) %>%
    # filter out line that only contains page number
    filter(!is_page_nb) %>% select(-is_page_nb)%>%
    rename("pg_all" = "page") %>%
    group_by(title) %>% mutate(pg_title = pg_all - min(pg_all) + 1) %>%
    ungroup() %>%
    select(pg_all, publi_yr, writ_yr, title, pg_title, text)

  # setdiff(text_by_line$title %>% unique, titles_tbl$titles)
  # setdiff(titles_tbl$titles, text_by_line$title %>% unique)

  return(list(
    by_page = text_by_page
    , by_line = text_by_line
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
use_data = function(freud_page, freud_line, freud_word) {

  dfile = list.files(path = "./data/", pattern = "^freud_.*.rda$", full.names = TRUE)
  if (length(dfile) > 0) map(dfile, file.remove)

  usethis::use_data(freud_page)
  usethis::use_data(freud_line)
  usethis::use_data(freud_word)


}
