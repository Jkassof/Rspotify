% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTracks.R
\name{parseFullTrack}
\alias{parseFullTrack}
\title{Parse full tracks object}
\usage{
parseFullTrack(track_obj)
}
\arguments{
\item{tracks_obj}{A json-like list, full track object returned by Spotify API}
}
\value{
A tibble
}
\description{
Helper function to parse useful fields from a full track object. Mostly used
internally.
}
