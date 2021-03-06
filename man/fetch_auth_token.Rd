% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requests.R
\name{fetch_auth_token}
\alias{fetch_auth_token}
\title{fetch auth token}
\usage{
fetch_auth_token(username, password)
}
\arguments{
\item{username}{username.}

\item{password}{password.}
}
\description{
given a list of username/password will fetch a jwt auth token and set it in the cache.
This must be called at the beginning of any session so that credentials can be placed in future
requests
}
