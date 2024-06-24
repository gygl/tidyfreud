#' Tibble with Sigmund Freud's complete work by page
#'
#' Sigmund Freud complete work by pages
#'
#' @format
#' `freud_page` a data frame with 5,102 rows and 2 columns:
#' \describe{
#'   \item{pg_all}{integer, page of original PDF file}
#'   \item{text}{character, raw text from each page}
#'   }
#'
#' @source \href{https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf}{Text source}
"freud_page"


#' Tibble with Sigmund Freud's complete work by sentences
#'
#' @format `freud_sntnce` a data frame with 125,571 rows and 6 columns:
#'
#' \describe{
#'   \item{pg_all}{integer, page number in original PDF file}
#'   \item{publi_yr}{integer, year of book / article publication}
#'   \item{writ_yr}{integer, year of book / article writing}
#'   \item{title}{factor, title of book / article}
#'   \item{pg_title}{integer, page number in book / article}
#'   \item{sentence}{character, raw text from each sentence}
#' }
#' @source \href{https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf}{Text source}
"freud_sntnce"

#' Tibble with Sigmund Freud's complete work by words
#'
#' Sigmund Freud complete work by word, stop words have been removed using
#' tidytext::stop_words according to lexicon onix, SMART, and snowball
#'
#' @format `freud_word` a data frame with 125,571 rows and 6 columns:
#'
#' \describe{
#'   \item{pg_all}{integer, page number in original PDF file}
#'   \item{publi_yr}{integer, year of book / article publication}
#'   \item{writ_yr}{integer, year of book / article writing}
#'   \item{title}{factor, title of book / article}
#'   \item{pg_title}{integer, page number in book / article}
#'   \item{word}{character, word from each page, without stop words}
#'   \item{is_stop_word}{logical, TRUE if word is a stop word (as define in [tidytext::stop_word()])}
#' }
#' @source \href{https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf}{Text source}
"freud_word"
