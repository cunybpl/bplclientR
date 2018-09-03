# request.R -- create requests for different endpoints


.clean_hyperlinked_bdbids <- function(df){
  # this will clean up the link for the bdbid in buildings
  if ('oper_agency_acronym' %in% colnames(df)){
    df[['bdbid']] = unlist(lapply(stringr::str_split(df[['bdbid']],'/'), function(row){
      return(as.numeric(row[length(row)]))
    }))
  }
  return(df)
}


.http_error_check <- function (res){
  # error checking the response
  if (httr::http_type(res) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (httr::http_error(res)) {
    stop(
      sprintf(
        "bplservices API request failed... [%s]",
        httr::status_code(res),
      ),
      call. = FALSE
    )
  }
}

#' .parse_contents
#'
#' parses the contents of a fetch request. Will raise errors if response is above 400. Will also
#' clean any hyperlinked bdbids. Other clean code for specific requests can get added to this method.
#'
#' @param res the response object from the httr::GET method
#'
#' @return the parsed contents object
#' @export
.parse_contents <- function(res) {
  .http_error_check(res) # check for errors... if they pass then parse contents

  parsed_contents <- (jsonlite::fromJSON(content(res, "text"))) # result parser that parses JSON to R friendly things...

  if (exists('results', parsed_contents))  # check for results and clean up (NOTE more cleaning can be added here if needed)
    parsed_contents$results <- .clean_hyperlinked_bdbids(parsed_contents$results)

  return(parsed_contents)
}


#' fetch_request
#'
#' fetch's from one of bpl's read-only list views using GET and
#' returns 1:n records as a dataframe or raises an http_error
#'
#' TODO create a paginating endpoint similar to this for large responses
#'
#' @param endpoint_url
#' @param params
#' @param with_caching
#'
#' @return parsed contents from the fetch request
#' @export
#' @import httr stringr
#'
#' @examples
fetch_request <- function(endpoint_url, query_params=NULL) {

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url)

  res <- httr::GET(url, add_headers(Authorization=token), query=query_params, encode='json', timeout=20)
  return(.parse_contents(res))  # parse result contents
}


#' post_request
#'
#' A post request route used for sending a data request to bema endpoints. Takes
#' a payload argument with a list for various settings documented in the service
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
  url <- paste0(base_url, endpoint_url)

  res <- httr::POST(url, add_headers(Authorization=token), body=payload, encode='json', timeout=20)
  return(.parse_contents(res))  # parse result contents
}


#' fetch auth token
#'
#' given a list of username/password will fetch a jwt auth token and set it in the cache.
#' This must be called at the beginning of any session so that credentials can be placed in future
#' requests
#'
#' @param credentials a username and password provided as a list
#'
#' @return
#' @export
#' @import httr
fetch_auth_token <- function(credentials=list(username='', password='')) {

  url <- cache_get_obtain_route() # get the obtain url

  res <- httr::POST(url,body=credentials, encode='json')
  parsed <- .parse_contents(res)

  # set the token in the cache
  cache_set_token(parsed$token)
  sprintf('Token received')
}




#' polling_request
#'
#' Polling endpoint for urls to fetch completed work from bema
#'
#' @param endpoint_url the endpoint to fetch results
#' @param tries the number of GET requests to send before giving up
#' @param polling_interval the timeout interval between requests
#'
#' @return parsed result or failure
#' @export
#' @import httr
polling_request <- function(endpoint_url, task_id, tries=10, polling_interval=3){

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url, task_id) # add endpoint to url

  for (i in 1:tries) {  # begin polling here
    sprintf('Polling... request: %d out of %d', i, tries)
    res = httr::GET(url, add_headers(Authorization=token)) # hit the result endpoint
    parsed <- .parse_contents(res)

    if (parsed$status == 'SUCCESS'){ # success
      return(parsed)
    } else if (parsed$status != 'FAILURE'){ # not-failure then sleep and try again
      Sys.sleep(polling_interval)
    } else {  # task failed
      sprintf(
        "bplservices API request failed... [%s]\n%s\n<%s>",
        httr::status_code(res),
        parsed$detail,  # TODO need to check that this is appropriate
        task_id,
        call. = FALSE
      )
      return(parsed)
    }
  }
}
