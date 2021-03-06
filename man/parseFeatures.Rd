% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTracks.R
\name{parseFeatures}
\alias{parseFeatures}
\title{Parse audio features object}
\usage{
parseFeatures(feature_obj)
}
\arguments{
\item{feature_obj}{A json-like list, audio features object returned by Spotify API}
}
\value{
A tibble
}
\description{
Helper function to parse useful fields from an audio features object. Mostly used
internally.
}
