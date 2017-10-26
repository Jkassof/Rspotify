searchSpotify <- function(search, type, market = "US", limit = 20, offset = 0)
{
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
#' @param search
#' @param market
#' @param limit
#' @param offset
#'
#' @return
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
#' @param vector
#' @param chunk.size
#'
#' @return
#' @export
#'
#' @examples
chunk <- function(vector, chunk.size)
{
  split(vector, ceiling(seq_along(vector)/chunk.size))
}
