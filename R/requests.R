# request.R -- create requests for different endpoints

.http_error_check <- function (res){
  # error checking the response
  if (httr::http_error(res)) {
    print(res)
    stop(
      sprintf(
        "bplservices API request failed... [%s]",
        httr::status_code(res)
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

  return(parsed_contents)
}


#' fetch_request
#'
#' fetch's from one of bpl's read-only list views using GET and
#' returns 1:n records as a dataframe or raises an http_error
#'
#' TODO create a paginating endpoint similar to this for large responses
#'
#' @param endpoint_url the api endpoint excluding the base
#' @param query_params filters to use for querying data
#' @param use_full_endpoint when set to true uses this as the full url
#'
#' @return parsed contents from the fetch request
#' @export
#' @import httr stringr
fetch_request <- function(endpoint_url, query_params=NULL, use_full_endpoint=F) {

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url)

  res <- httr::GET(url, add_headers(Authorization=token), query=query_params, encode='json', timeout=20, config = httr::config(ssl_verifypeer = .cache$ssl_verify))
  return(.parse_contents(res))  # parse result contents
}


#' paginator_fetch_request
#'
#' For larger requests this will handle paginated responses. Results are stored in a list.
#'
#' @param endpoint_url intial url endpoint
#' @param query_params initial query params
#' @param max_pages max number of pages to collect. Default is 25 or 2500 records.
#' @param sleep_interval Defaults to 0.5.
#'
#' @return list of result contents
#' @export
paginator_fetch_request <- function(endpoint_url, query_params=NULL, max_pages=25, sleep_interval = 0.5){

  current_page <- 1
  output_list <- NULL
  next_url <- NULL

  # make initial request and check length of result... if it doesn't need pagination we return it
  print(paste0('Fetching initial data for request: ', endpoint_url))
  initial_contents <- fetch_request(endpoint_url, query_params=query_params)
  #print(initial_contents)

  if (is.null(initial_contents$`next`))
    return(initial_contents)

  output_list <- list()
  output_list[[current_page]] <- initial_contents

  while (current_page <= max_pages){
    # set up the initial request - we need the first result to generate output vec as well
    if(is.null(output_list[[current_page]]$`next`))  # if next is NULL then we're done... output the current_contents and return the output_list
      return(output_list)
    current_page <- current_page + 1
    query_params$page <- current_page

    next_contents <- fetch_request(endpoint_url, query_params=query_params)

    print(paste0('next_contents$next: ', next_contents$`next`))
    output_list[[current_page]] <- next_contents

    Sys.sleep(sleep_interval) # sleep a little so we don't blow up the API
  }

  print('max_pages reached for request...')
  return(output_list)
}


#' post_request
#'
#' A post request route used for sending a data request to bema endpoints. Takes
#' a payload argument with a list for various settings documented in the service
#'
#' @param endpoint_url the url for the endpoint
#' @param payload the parameters needed for the post request
#'
#' @return  the contents of the response
#' @export
#' @import httr
post_request <- function(endpoint_url, payload=list()){
  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url)

  res <- httr::POST(url, add_headers(Authorization=token), body=payload, encode='json', timeout=20, config = httr::config(ssl_verifypeer = .cache$ssl_verify))
  return(.parse_contents(res))  # parse result contents
}


#' fetch auth token
#'
#' given a list of username/password will fetch a jwt auth token and set it in the cache.
#' This must be called at the beginning of any session so that credentials can be placed in future
#' requests
#'
#' @param username username.
#' @param password password.
#'
#' @return
#' @export
#' @import httr
fetch_auth_token <- function(username, password) {

  credentials = list(username=username, password=password)
  url <- cache_get_obtain_route() # get the obtain url

  res <- httr::POST(url,body=credentials, encode='json', config = httr::config(ssl_verifypeer = .cache$ssl_verify))
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
#' @param task_id task_id string.
#' @param tries the number of GET requests to send before giving up
#' @param polling_interval the timeout interval between requests
#'
#' @return parsed result or failure
#' @export
#' @import httr
polling_request <- function(endpoint_url, task_id, tries=10, polling_interval=1){

  # get base url from cache
  base_url <- cache_get_base_url()
  token <- cache_get_token()
  url <- paste0(base_url, endpoint_url, task_id) # add endpoint to url

  for (i in 1:tries) {  # begin polling here
    sprintf('Polling... request: %d out of %d', i, tries)
    res = httr::GET(url, add_headers(Authorization=token), config = httr::config(ssl_verifypeer = .cache$ssl_verify)) # hit the result endpoint
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
