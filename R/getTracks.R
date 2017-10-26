#' Parse full tracks object
#'
#' Helper function to parse useful fields from a full track object. Mostly used
#' internally.
#'
#' @param tracks_obj A json-like list, full track object returned by Spotify API
#'
#' @return A tibble
#' @export
#'
#' @examples
parseFullTrack <- function(track_obj)
{
  tibble::tibble(
    track      = track_obj[["name"]],
    artist     = paste(sapply(track_obj[["artists"]], function(x) x[["name"]]), collapse = ", "),
    popularity = track_obj[["popularity"]],
    album      = track_obj[["album"]][["name"]],
    track_id   = track_obj[["id"]],
    album_id   = track_obj[["album"]][["id"]],
    artist_id  = paste(sapply(track_obj[["artist"]], function(x) x[["id"]]), collapse = ", ")
  )
}

#' Parse simplified track object
#'
#' Helper function to parse useful fields from a simplified track obect. Mostly used
#' internally.
#'
#' @param track_obj A json-like list, simplified track object returned by Spotify API
#'
#' @return A tibble
#' @export
#'
#' @examples
parseShortTrack <- function(track_obj)
{
  tibble::tibble(
    artist      = paste(sapply(track_obj[["artists"]], function(x) x[['name']]), collapse = ", "),
    track       = track_obj[["name"]],
    duration_ms = track_obj[["duration_ms"]],
    track_no    = track_obj[["track_number"]],
    track_id    = track_obj[["id"]]
  )
}

#' Get track information
#'
#' Retrieves tracks information. See \link{\code{parseFullTrack}} for details on frields retrieved.
#'
#' @param track.id Character vector, Spotify track id. Max 20.
#'
#' @return A tibble
#' @export
#'
#' @examples
getTrack <- function(track.id = NULL)
{
  if (length(track.id) > 20) stop("Too many track IDs, max 20")
  token <- get(".token", envir = .GlobalEnv)

  if(is.null(track.id)) {
    stop("No track ID provided")
  } else if (length(track.id) == 1) {
    req_url <- paste0("https://api.spotify.com/v1/tracks/", track.id)
  } else {
    req_url <- paste0("https://api.spotify.com/v1/tracks?ids=",
                      paste(track.id, collapse = ","))
  }

  req <- httr::GET(req_url, httr::config(token = token))

  tracks <- httr::content(req)

  if (length(tracks) == 1) {
    to_parse <- tracks$tracks
    purrr::map_df(to_parse, parseFullTrack)
  } else {
    parseFullTrack(tracks)
  }

}

#' Get track features
#'
#' @param track.id Character vector, spotify track id. Max 20.
#'
#' @return A tibble
#' @export
#'
#' @examples
getTrackFeatures <- function(track.id = NULL)
{
  if (length(track.id) > 20) stop("Too many track IDs, max 20")
  token <- get(".token", envir = .GlobalEnv)

  if(is.null(track.id)) {
    stop("No track ID provided")
  } else if (length(track.id) == 1) {
    req_url <- paste0("https://api.spotify.com/v1/audio-features/", track.id)
  } else {
    req_url <- paste0("https://api.spotify.com/v1/audio-features?ids=",
                      paste(track.id, collapse = ","))
  }

  req <- httr::GET(req_url, httr::config(token = token))

  features <- httr::content(req)

  if (length(features) == 1) {
    to_parse <- features$audio_features
    purrr::map_df(to_parse, parseFeatures)
  } else {
    parseFeatures(features)
  }
}

#' Parse audio features object
#'
#' Helper function to parse useful fields from an audio features object. Mostly used
#' internally.
#'
#' @param feature_obj A json-like list, audio features object returned by Spotify API
#'
#' @return A tibble
#' @export
parseFeatures <- function(feature_obj)
{
  tibble::as_tibble(feature_obj)
}

