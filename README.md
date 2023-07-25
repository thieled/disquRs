
<!-- README.md is generated from README.Rmd. Please edit that file -->

# disquRs <img src="man/figures/logo.png" align="right" height="282" />

<!-- badges: start -->
<!-- badges: end -->

disquRs is an R package for collecting data from the Disqus API. It
extends the functionality of
[disqusR](https://github.com/JanMarvin/disqusR) and provides the data in
tidy format, or optionally in raw JSON.

Disqus is a user comments platform, that is or has been used on a number
of websites.

## Installation

You can install the development version of disquRs from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thieled/disquRs")
```

## Prerequesites

To access the Disqus API, you must register and create an app on the
[Disqus developer site](https://disqus.com/api/applications/).

Use your ‘public key’ to authenticate from R.

## Example

This is a example that shows how to collect 100 comments from a
‘Breitbart’ article page. The code is commented out, as I cannot share
my own API keys here.

``` r
library(disquRs)

# Use the 'public key' of your registered app:
key <- "YOURKEYHERE"

# Define thread (also works with thread IDs) 
thread <- "https://www.breitbart.com/2024-election/2023/07/18/trump-says-deranged-special-counsel-told-him-hes-a-target-in-january-6-probe/"

# Get data.frame with 100 comment from this thread, in descending order. To get all, set n=Inf.
# res_comments <- get_generic(ressource = "posts",
#                  option = "list",
#                  thread = thread, 
#                  forum = "breitbartproduction",
#                  key = key,
#                  n = 100
#             )

# dplyr::glimpse(res_comments)


# Rows: 100
# Columns: 58
# $ editableUntil                  <chr> "2023-08-01T19:25:56", "2023-08-01T18:15:09", "2023-08-01T16:56:21", "202…
# $ dislikes                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
# $ thread                         <chr> "9778851886", "9778851886", "9778851886", "9778851886", "9778851886", "97…
# $ numReports                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ likes                          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ message                        <chr> "<p>You are grasping. Listen to the recording - it is available in full, …
# $ id                             <chr> "6239697387", "6239644275", "6239583167", "6239574509", "6239265529", "62…
# $ createdAt                      <chr> "2023-07-25T19:25:56", "2023-07-25T18:15:09", "2023-07-25T16:56:21", "202…
# $ media                          <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, …
# $ isSpam                         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ isDeletedByAuthor              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ isHighlighted                  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ parent                         <dbl> 6239197122, 6239208477, 6239265529, 6239213261, 6238697408, 6239214319, 6…
# $ isApproved                     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
# $ isNewUserNeedsApproval         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ isDeleted                      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ isFlagged                      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ raw_message                    <chr> "You are grasping. Listen to the recording - it is available in full, and…
# $ isAtFlagLimit                  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ canVote                        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ forum                          <chr> "breitbartproduction", "breitbartproduction", "breitbartproduction", "bre…
# $ points                         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0…
# $ isEdited                       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRU…
# $ sb                             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ author_username                <chr> "disqus_dahBMo4QL3", "disqus_dahBMo4QL3", "mr2017", "disqus_dahBMo4QL3", …
# $ author_about                   <chr> "I was born, then I died.", "I was born, then I died.", "", "I was born, …
# $ author_name                    <chr> "Joe Dokes", "Joe Dokes", "RB", "Joe Dokes", "threehundredthousand", "jas…
# $ author_disable3rdPartyTrackers <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, …
# $ author_isPowerContributor      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ author_joinedAt                <chr> "2023-01-24T06:08:10", "2023-01-24T06:08:10", "2013-01-16T17:22:15", "202…
# $ author_profileUrl              <chr> "https://disqus.com/by/disqus_dahBMo4QL3/", "https://disqus.com/by/disqus…
# $ author_url                     <chr> "http://www.get-a-life.com", "http://www.get-a-life.com", "", "http://www…
# $ author_location                <chr> "Outer Space", "Outer Space", "", "Outer Space", "In England's green & pl…
# $ author_isPrivate               <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRU…
# $ author_signedUrl               <chr> "http://disq.us/?url=http%3A%2F%2Fwww.get-a-life.com&key=0-rLdFB2uFWAK7e0…
# $ author_isPrimary               <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
# $ author_isAnonymous             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
# $ author_id                      <chr> "391534285", "391534285", "41404468", "391534285", "51704110", "387897208…
# $ author_rep                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_reputation              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_reputationLabel         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ moderationLabels_1             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, "badWords", NA, NA, NA, NA, NA, NA, N…
# $ moderationLabels_2             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_avatar_permalink        <chr> "https://disqus.com/api/users/avatars/disqus_dahBMo4QL3.jpg", "https://di…
# $ author_avatar_cache            <chr> "https://c.disquscdn.com/uploads/users/39153/4285/avatar92.jpg?1683400498…
# $ author_avatar_isCustom         <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
# $ author_badges_1                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_badges_2                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_badges_3                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_badges_4                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_badges_5                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_badges_6                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ author_avatar_xlarge_permalink <chr> "https://disqus.com/api/users/avatars/disqus_dahBMo4QL3.jpg", "https://di…
# $ author_avatar_xlarge_cache     <chr> "https://c.disquscdn.com/uploads/users/39153/4285/avatar200.jpg?168340049…
# $ author_avatar_large_permalink  <chr> "https://disqus.com/api/users/avatars/disqus_dahBMo4QL3.jpg", "https://di…
# $ author_avatar_large_cache      <chr> "https://c.disquscdn.com/uploads/users/39153/4285/avatar92.jpg?1683400498…
# $ author_avatar_small_permalink  <chr> "https://disqus.com/api/users/avatars/disqus_dahBMo4QL3.jpg", "https://di…
# $ author_avatar_small_cache      <chr> "https://c.disquscdn.com/uploads/users/39153/4285/avatar32.jpg?1683400498…
```

DisquRs takes care of pagination, provides a set of pre-defined calls,
as well as functions that make custom API calls very easy. The package
returns all variables fetched through the API.

For full API documentation, see [here](https://disqus.com/api/docs/).

At the moment, this package is still work in progress (2023-07-25).