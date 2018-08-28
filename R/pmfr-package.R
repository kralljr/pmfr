#' R with Positive Matrix Factorization (PMF)
#' 
#' pmfr allows users to easily input and analyze results from
#' US EPA Positive Matrix Factorization (PMF) version 5.0.  
#' Specifically, this package reads in relevant output,
#' formats the output, and provides data and visual summaries.
#' 
#' Profiles
#' 
#' To read in profile information, users need to have the original data
#' and pmf output
#' \code{\link{pmfprof}}
#' 
#' To read in source contributions, users need to have the original data
#' and pmf output
#' \code{\link{pmfcont}}
#' 
#' @references Pentti Paatero and Unto Tapper (1994).  Positive matrix 
#' factorization: A non‐negative factor model with optimal utilization 
#' of error estimates of data values.  Environmetrics, 5(2), 111-126.
#' @references Gary Norris, Rachelle Duvall, Steve Brown, Song Bai (2014).
#' EPA Positive Matrix Factorization (PMF) 5.0 Fundamentals 
#' and User Guide
#' @docType package
#' @name pmfr
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import RColorBrewer
NULL