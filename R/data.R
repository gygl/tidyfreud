#' Sigmund Freud data
#'
#' Sigmund Freud complete work by pages
#'
#' @format # `freud_page`
#' A data frame with 5,102 rows and 2 columns:
#' \describe{
#'   \item{pg_all}{page of original PDF file}
#'   \item{text}{raw text from each page}
#'   }
#'
#' @source <https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf>
"freud_page"


#' Sigmund Freud complete work by lines
#'
#' @format # `freud_line`
#' A data frame with 125,571 rows and 6 columns:
#'
#' \describe{
#'   \item{pg_all}{page number in original PDF file}
#'   \item{publi_yr}{year of book / article publication}
#'   \item{writ_yr}{year of book / article writing}
#'   \item{title}{title of book / article}
#'   \item{pg_title}{page number in book / article}
#'   \item{text}{raw text from each page}
#' }
#' @source <https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf>
"freud_line"

#' #' Sigmund Freud complete work by words
#'
#' Sigmund Freud complete work by word, stop words have been removed using
#' tidytext::stop_words according to lexicon onix, SMART, and snowball
#'
#' @format ## `freud_word`
#' A data frame with 125,571 rows and 6 columns:
#'
#' \describe{
#'   \item{pg_all}{page number in original PDF file}
#'   \item{publi_yr}{year of book / article publication}
#'   \item{writ_yr}{year of book / article writing}
#'   \item{title}{title of book / article}
#'   \item{pg_title}{page number in book / article}
#'   \item{wor}{word from each page, without stop words}
#'   ...
#' }
#' @source <https://www.valas.fr/IMG/pdf/Freud_Complete_Works.pdf>
"freud_word"
