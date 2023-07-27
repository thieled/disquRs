#' Get data from the Disqus API for a specific thread.
#'
#' This function retrieves data from the Disqus API for a specific thread identified by its thread identifier. The API call can be customized by specifying the desired 'option' to perform different operations on the thread. The data is returned in either a 'tidy', unnested format or 'raw' (JSON) format.
#'
#' @param thread Character vector containing the thread identifier.
#' @param option Character vector specifying the specific API operation to perform on the thread:
#' \itemize{
#'   \item \code{"list"}: List threads.
#'   \item \code{"listHot"}: List hot threads.
#'   \item \code{"listPopular"}: List popular threads.
#'   \item \code{"listPosts"}: List posts for the thread.
#'   \item \code{"details"}: Get details about the thread.
#' }
#' @param forum Character vector containing the forum identifier or shortname (default is NULL).
#' @param key Character vector containing the primary API key to authenticate the API call(s).
#' @param second_key Character vector containing a second API key to use if the hourly API limit is reached with the primary key (default is NULL).
#' @param verbose Logical value indicating whether to display informative messages during the API calls (default is TRUE).
#' @param n The maximum number of observations to retrieve (default is 100).
#' @param limit Numeric value specifying the maximum number of items to return in each API response (default is 25).
#' @param format Character vector specifying the desired output format: 'tidy' (unnested) or 'raw' (JSON) (default is 'tidy').
#' @param ... Additional arguments to be passed to the `generateURL` function for URL generation.
#'
#' @return The retrieved data from the Disqus API for the specific thread in the specified format.
#'
#' @seealso `generateURL`, `paginateAPI`, `get_generic`
#'
#' @examples
#' # Retrieve a list of threads in 'tidy' format using the primary key
#' # tidy_threads <- get_threads(thread = "thread_id", option = "list", key = "YOURAPPKEY")
#'
#' @export
get_threads <- function(thread,
                        option = c("list",
                                   "listHot",
                                   "listPopular",
                                   "listPosts",
                                   "details"
                        ),
                        forum = NULL,
                        key,
                        second_key = NULL,
                        verbose = TRUE,
                        n = 100,
                        limit = 25,
                        format = c("tidy", "raw"),
                        ...){

  format <- match.arg(format, c("tidy", "raw"))

  res <- get_generic(ressource = "threads",
                     option = option,
                     thread = thread,
                     forum = forum,
                     key = key,
                     second_key = second_key,
                     verbose = verbose,
                     n = n,
                     limit = limit,
                     format = format,
                     ...)

  return(res)
}



