#' Parse playlist object
#'
#' Helper function to parse useful fields from the playlist object. Mostly used
#' internally.
#'
#' @param pl_obj A json-like list, playlist object returned by Spotify API
#'
#' @return
#' @export
#'
#' @examples
parsePlaylist <- function(pl_obj, tracks = FALSE)
{
  out <- tibble::tibble(
    name        = pl_obj[["name"]],
    id          = pl_obj[["id"]],
    owner       = pl_obj[["owner"]][["id"]],
    type        = pl_obj[["type"]],
    track_count = pl_obj[["tracks"]][["total"]]
  )
  if (tracks == TRUE) {
    out <-
      tibble::add_column(out, tracks = list(getPlaylistTracks(pl_obj[["owner"]][["id"]], pl_obj[["id"]])))
  }

  out
}

#' Get tracks from a playlist
#'
#' @param user.id character string
#' @param playlist.id character string
#'
#' @return
#' @export
#'
#' @examples

getPlaylistTracks <- function(user.id, playlist.id, limit, offset)
{
  token <- get(".token", envir = parent.env(environment()))
  stub <- "https://api.spotify.com/v1/users/"
  url <- paste0(stub, user.id, "/playlists/", playlist.id,
                "/tracks?limit=", limit,
                "&offset=", offset)
  req <- httr::GET(url, httr::config(token = token))
  req_items <- httr::content(req)[["items"]]
  tracks <- purrr::map(req_items, function(x) x[["track"]])
  purrr::map_df(tracks, parseFullTrack)
}

#' Title
#'
#' @param country
#' @param limit
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
getFeaturedPlaylists <- function(country = "us", limit = 20, offset = 0, tracks = FALSE)
{
  token <- get(".token", envir = parent.env(environment()))
  stub <- "https://api.spotify.com/v1/browse/featured-playlists?"
  url <- paste0(stub,
                "country=", country,
                "&limit=", limit,
                "&offset=", offset)
  req <- httr::GET(url, httr::config(token = token))
  playlists <- httr::content(req)$playlists$items
  purrr::map_df(playlists, parsePlaylist, tracks = tracks)
}

