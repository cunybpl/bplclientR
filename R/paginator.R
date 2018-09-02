# The paginator takes a request through the results
# with a small timeout between requests. If a network error occurs during pagination the
# function will raise an error that can be handled in the calling route and not return the
# completed request


paginator <- function(request_callback, ...){
  kwargs <- list(...) # get kwargs
}
