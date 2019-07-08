#
# a simple caching interface for requests.
# The cache is treated as private and should only be accessed by the developer
# through the methods in the cache interface.
#
# The base url and token must be set before any fetch requests can be made


.cache <- new.env()  # used for storage only... access using the public methods

#' cache_init
#'
#' configures the cache environemnt. An optional base_url name can be passed in for convenience.
#' This MUST be called first in order to configure the cache with the correct auth routes and base_url
#'
#' @param base_url either 'staging' or 'production for convenience settings. defaults to staging
#' @param api_url defaults to 'https://api.cunybplservices.net/'
#' @param ssl_verify defaults to TRUE.
#'
#' @export
cache_init = function (base_url='staging', api_url = 'https://api.cunybplservices.net/', ssl_verify = TRUE) {

  .cache$base_url <- NULL  # setup base and token
  .cache$token <- NULL
  .cache$ssl_verify <- ssl_verify
  cache_set_base_url(api_url)

  # set up token routes
  .cache$obtain = paste0(.cache$base_url, "auth/obtain-jwt-token/")
  .cache$refresh = paste0(.cache$base_url, "auth/refresh-jwt-token/")
  .cache$verify = paste0(.cache$base_url, "auth/verify-jwt-token/")

  sprintf('Cache initialized... \n base_url: %s', .cache$base_url)


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
#' @return the current_base_url as a string
#' @export
cache_get_base_url <- function(){ .cache$base_url }


#' cache_set_token
#'
#' Sets the current api token. Called after authentication succeeds.
#' Should be passed a string from the "token" key in the response content.
#' JWT header is added here
#'
#' @param token the jwt token sent back from the auth
#' @export
cache_set_token <- function(token){ .cache$token <- paste0('JWT ',token) }


#' cache_get_token
#'
#' Retrieves the current api token for use in constructing request headers for API
#' endpoints
#' @return the jwt token used for request headers
#' @export
cache_get_token <- function(){ .cache$token }



#' cache_get_obtain_route
#'
#' @return the jwt-token-obtain route
#' @export
cache_get_obtain_route <- function() { .cache$obtain }


#' cache_get_refresh_route
#'
#' @return the jwt-token refresh route
#' @export
cache_get_refresh_route <- function() { .cache$refresh }


#' cache_get_verify_route
#'
#' @return the jwt-token verify route
#' @export
cache_get_verify_route <- function() { .cache$verify }

#' cache_get_ssl_verify
#'
#' @return ssl_verify flag
#' @export
cache_get_ssl_verify <- function(){.cache$ssl_verify}
