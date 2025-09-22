#' Book List
#'
#' Get Best Sellers list. If no date is provided returns the latest list.
#' 
#' @param list The name of the Best Sellers list.
#' @param published_date The date the best sellers list was published on NYTimes.com. 
#' Use "current" for latest list.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' list <- ny_book_list(list = "hardcover-fiction")
#' }
#' 
#' @name book_list
#' @export
ny_book_list <- function(list, published_date = "current"){

  assert_that(!missing(list), msg = "Missing list")
  
  parsed_url <- parse_url(BASE_URL)
  
  if(is.null(published_date)){
    published_date <- "current"
  } else {
    published_date <- .process_book_date(published_date)
  }
  
  parsed_url$path <- c("svc", "books", "v3", "lists", published_date, paste0(list, ".json"))
  opts <- list(
    `api-key` = .get_key()
  )

  parsed_url$query <- opts
  url <- build_url(parsed_url)
  response <- GET(url)
  stop_for_status(response)
  page_content <- content(response)
  
  cat(crayon::blue(cli::symbol$info), page_content$num_results, "results returned\n")
  
  page_content$results
}

#' Overview
#' 
#' Get all the Best Sellers lists for a given week.
#' 
#' @param published_date The best-seller list publication date. 
#' You do not have to specify the exact date the list was published. 
#' The service will search forward (into the future) for the closest publication date to the date you specify. 
#' For example, a request for \code{as.Date("2013-05-22")} will retrieve the list that was published on \code{05-26}.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' list <- ny_book_overview(Sys.Date() - 365)
#' }
#' 
#' @export
ny_book_overview <- function(published_date = NULL){

  url <- parse_url(BASE_URL)
  url$path <- c("svc", "books", "v3", "lists", "overview.json")
  url$query <- list(
    published_date = .process_book_date(published_date),
    `api-key` = .get_key()
  )
  url <- build_url(url)
  
  response <- GET(url)
  stop_for_status(response)
  content <- content(response)
  cat(crayon::blue(cli::symbol$info), content$num_results, "results returned\n")
  
  content$results 
}
