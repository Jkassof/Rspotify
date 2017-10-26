searchSpotify <- function(search, type, market = "US", limit = 20, offset = 0)
{
  stopifnot(type %in% c("album", "artist", "playlist", "track"),
            limit >= 1 & limit <= 50,
            offset >= 0 & offset <=100000)
  token <- get(".token", envir = .GlobalEnv)
  stub <- "https://api.spotify.com/v1/search?q="
  q <- gsub(pattern = " ", replacement = "+", x = search)
  url <- paste0(stub, q,
                "&type=", type,
                "&market=", market,
                "&limit=", limit,
                "&offset=", offset)
  req <- httr::GET(url, httr::config(token = token))
  httr::content(req)
}

#' Search for albums
#'
#' Function for searching for albums by album name.
#'
#' @param search search string
#' @param market market of interest, defaults to US
#' @param limit integer number of records to return, 1-50
#' @param offset integer records to offset results by, 0-100,00. Use to page through results.
#'
#' @family search functions
#' @return tibble of album info
#' @export
#'
#' @examples
searchAlbums <- function(search, market = "US", limit = 20, offset = 0)
{
  alb_search <- searchSpotify(search, "album", market, limit, offset)$albums$items
  alb_ids <- sapply(alb_search, function(x) x[['id']])
  alb_ids_cut <- chunk(alb_ids, 20)
  purrr::map_df(alb_ids_cut, getAlbum)
}

#' Search for artists
#'
#' @param search
#' @param market
#' @param limit
#' @param offset
#'
#' @family search functions
#' @return
#' @export
#'
#' @examples
searchArtists <- function(search, market = "US", limit = 20, offset = 0)
{
  art_search <- searchSpotify(search, "artist", market, limit, offset)$artists$items
  purrr::map_df(art_search, parseArtist)
}

#' Search for playlists
#'
#' @param search
#' @param market
#' @param limit
#' @param offset
#'
#' @family search functions
#' @return
#' @export
#'
#' @examples
searchPlaylist <- function(search, market = "US", limit = 20, offset = 0, tracks = FALSE)
{
  stopifnot(is.logical(tracks))
  pl_search <- searchSpotify(search, "playlist", market, limit, offset)$playlists$items
  purrr::map_df(pl_search, parsePlaylist, tracks = tracks)
}


#' Vector chunking
#'
#' Function for splitting a vector into a list of smaller vectors. The final item in
#' the list will be shorter in length than the rest of the vector length isn't evenly
#' divisible by the chunk size.
#' @param vector
#' @param chunk.size integer, length of output vectors
#'
#' @return
#' @export
#'
#' @examples
chunk <- function(vector, chunk.size)
{
  split(vector, ceiling(seq_along(vector)/chunk.size))
}
