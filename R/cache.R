#
# a simple caching interface for requests.
# The cache is treated as private and should only be accessed by the developer
# through the methods in the cache interface.
#
# The base_url and token must be set before sending a request to bplservices
# data for successful requests can be cached by setting a key=value in the
# response_data list. These could be anything from json responses to user

.cache <- new.env()  # used for storage only... access using the public methods

#' cache_init
#'
#' configures the cache environemnt. An optional base_url name can be passed in for convenience
#'
#' @param base_url either 'staging' or 'production for convenience settings. defaults to staging
#'
#' @return
#' @export
cache_init = function (base_url='staging') {

  .cache$base_url <- NULL
  .cache$token <- NULL
  .cache$response <- list()

  if (base_url=='staging') {
    cache_set_base_url('https://bpl-services-staging.com/api/v1/')
  } else if(base_url=='production') {
    cache_set_base_url('https://some-production-url.com') #configure this when production url is ready
  }
}


#' cache_set_base_url
#'
#' Set the base url. Will override configuration set by the cache_config_base_url setting
#'
#' @param url the base url for the project
#'
#' @return the set url string or NULL
#' @export
cache_set_base_url <- function(url){ .cache$base_url <- url }


#' cache_get_base_url
#'
#' returns the current base_url
#'
#' @param url
#'
#' @return the current_base_url as a string
#' @export
cache_get_base_url <- function(url){ .cache$base_url }


#' cache_set_token
#'
#' Sets the current api token. Called after authentication succeeds
#'
#' @param token the jwt token sent back from the auth
#' @export
cache_set_token <- function(token){ .cache$token <- token }


#' cache_get_token
#'
#' Retrieves the current api token for use in constructing request headers for API
#' endpoints
#'
#' @param token
#'
#' @return the jwt token used for request headers
#' @export
cache_get_token <- function(token){ .cache$token }


#' cache_set_response
#'
#' Used to store the results of requests. This could be json objects from the response
#' body or dataframes/vectors and other R objects for use by apps.
#'
#' @param key the key to store the data object
#' @param data the data object
#'
#' @export
cache_set_response <- function(key, data){
  .cache$response$key <- data
}


#' cache_get_response
#'
#' @param key key in the response store
#'
#' @return the
#' @export
cache_get_response <- function (key){
  .cache_$response$key
}


