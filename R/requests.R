# request.R is a slightly lower level API wrapper, that handles the parsing
# and httr for individual requests. This would be used as callbacks by higher
# level functions such as the paginator and bema


.parse_results <- function(res) {
  # result parser that parses JSON to R friendly things...
  if (httr::http_type(res) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  return(jsonlite::fromJSON(content(res, "text")))
}


.clean_hyperlinked_bdbids <- function(df){
  # this will clean up the link for the bdbid in buildings
  if ('oper_agency_acronym' %in% colnames(df)){
    df[['bdbid']] = unlist(lapply(stringr::str_split(df[['bdbid']],'/'), function(row){
      return(as.numeric(row[length(row)]))
    }))
  }
  return(df)
}

.http_error_check <- function(parsed) {
  # checks for any status 400 and above and raises
  if (http_error(parsed)) {
    stop(
      sprintf(
        "bplservices API request failed... [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$detail
      ),
      call. = FALSE
    )
  }
}

#' fetch_request
#'
#' fetch's from one of bpl's read-only list views using GET and
#' returns 1:n records as a dataframe or raises an http_error
#'
#' @param endpoint_url
#' @param params
#' @param with_caching
#'
#' @return dataframe of result of fetch
#' @export
#' @import httr stringr
#'
#' @examples
fetch_request <- function(endpoint_url, query_params=NULL) {

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(endpoint_url, base_url)

  res <- httr::GET(url, add_headers(Authorization=token), query=query_params, encode='json', timeout=20)
  parsed <- .parsed_results(res)
  .http_error_check(parsed)

  return(parsed)
}


#' post_request
#'
#' @param endpoint_url
#' @param payload
#'
#' @return
#' @export
#' @import httr
post_request <- function(endpoint_url, payload=list()){
  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(endpoint_url, base_url)

  res <- httr::POST(url, add_headers(Authorization=token), body=payload, encode='json', timeout=20)

  parsed <- .parsed_results(res)
  .http_error_check(parsed)
  return(parsed)
}


#' polling_request
#'
#' Polling endpoint for urls to fetch completed work from bema
#'
#' @param endpoint_url the endpoint to fetch results
#' @param tries the number of GET requests to send before giving up
#' @param polling_interval the timeout interval between requests
#'
#' @return
#' @export
#'
#' @examples
polling_request <- function(endpoint_url, task_id, tries=10, polling_interval=2){

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url, task_id)

  for (i in 1:tries) {
    sprintf('Polling... request: %d out of %d', i, tries)
    res = httr::GET(url, add_headers(Authorization=token),)

    res <- httr::POST(url, add_headers(Authorization=token), body=payload, encode='json', timeout=20)
    parsed <- .parsed_results(res)
    .http_error_check(parsed)

    if (parsed$status == 'SUCCESS'){ # failure
      return(parsed)
    } else if (parsed$status != 'FAILURE'){ # not-failure then sleep and try again
      Sys.sleep(polling_interval)
    } else {  # task failed
      sprintf(
        "bplservices API request failed... [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$detail,
        task_id
      ),
      call. = FALSE
    }
  }
}
