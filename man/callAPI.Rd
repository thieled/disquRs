% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callAPI.R
\name{callAPI}
\alias{callAPI}
\title{Call the Disqus API.}
\usage{
callAPI(url, key, verbose = TRUE)
}
\arguments{
\item{url}{A character vector representing the complete URL for the Disqus API call.}

\item{key}{A character vector containing the API key to authenticate the API call.}

\item{verbose}{Logical value indicating whether to display informative messages (default is TRUE).}
}
\value{
The response from the Disqus API call, parsed as a list.
}
\description{
This function makes a call to the Disqus API using the provided URL and API key.
It replaces the API key in the URL with the one provided and retrieves the data from the API.
The response is parsed and returned as an R object.
}
\examples{
# Call the API to retrieve the data from the specified URL with the provided API key
# result <- callAPI(url = url, key = "YOURAPPKEY")

}
