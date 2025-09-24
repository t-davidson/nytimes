#' Search
#' 
#' Search for NYT articles by keywords and filters.
#' 
#' @param q Search query.
#' @param since,until Begin and start \code{Date} objects.
#' @param pages Number of pages of results to return. Set to \code{Inf} to retrieve all pages (up to 100).
#' @param sort Sort order \code{newest}, \code{oldest}.
#' @param fq Query filter. See \url{https://developer.nytimes.com/docs/articlesearch-product/1/overview} for details.
#' 
#' @examples
#' \dontrun{
#' nytimes_key("xXXxxXxXxXXx")
#' trump <- ny_search("Trump", since = Sys.Date() - 3)
#' }
#' 
#' @export
ny_search <- function(q, since = NULL, until = NULL, pages = 1, sort = c("newest", "oldest"), fq = NULL){

  assert_that(!missing(q))
  assert_that(pages > 0)

  if(is.infinite(pages)) pages <- 100
  if(pages > 100) pages <- 100

  opts <- list(
    q = q,
    begin_date = .process_search_date(since), 
    end_date = .process_search_date(until), 
    sort = match.arg(sort),
    fq = fq,
    `api-key` = .get_key()
  )

  parsed_url <- parse_url(BASE_URL)
  parsed_url$path <- c("svc", "search", "v2", "articlesearch.json")

  content <- list()
  for(p in 0:(pages-1)){
    opts$page <- p
    parsed_url$query <- opts
    url <- build_url(parsed_url)
    response <- GET(url)
    stop_for_status(response)
    page_content <- content(response)
    content <- append(content, page_content$response$docs)

    # check if results left
    hits <- page_content$response$meta$hits
    offset <- page_content$response$meta$offset
    if(p == 0){
      cat(crayon::blue(cli::symbol$info), hits, "results available\n")
      pages = min(pages, floor(hits / 10))
      pb <- progress::progress_bar$new(
          format = "  downloading [:bar] page :current/:total (:percent) :eta",
          total = pages, clear = FALSE, width = 60
      )
    } else {
      if (!pb$finished) pb$tick()
    }
    
    if(p >= pages)
      break

    Sys.sleep(13)
  }
  pb$terminate()

  content
}
