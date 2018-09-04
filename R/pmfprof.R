#' Reading in and formatting profile data
#' 
#' \code{pmfprof} Read in profile data
#' 
#' These are functions to read in and format PMF output profiles
#' 
#' @title pmfprof
#' @param dat full path to original constituent dataset
#' @param dir path to PMF result
#' @param prefix prefix of PMF result
#' @param constrain was constrained PMF run? (default = F)
#' @param cn optional column names for sources
#' @param scale whether to scale profiles (default = F)
#' @param rms character vector of constituents removed from PMF run
#' @export
#' @examples 
#' # Save nycdat.csv and PMF output to working directory
#' prof <- pmfprof("nycdat.csv", prefix = "pmfdatex")
#' print(prof)
#' plot(prof)
#' summary(prof)
pmfprof <- function(x, ...) UseMethod("pmfprof")

#' @rdname pmfprof
#' @export
pmfprof.default <- function(dat, dir = "./", rms = NULL, prefix, constrain = F, 
                             cn = NULL, scale = F) {
  # read in constituent data
  dat <- read.table(dat, header = T)
  
  # remove constituents from original data
  if(!is.null(rms)) {
    dat <- dat[, -which(colnames(dat) %in% rms)]
  }
  
  # set dimensions of constituent data
  P <- ncol(dat) - 1
  T <- nrow(dat)
  
  # read in PMF profile data
  base <- read.table(file.path(dir, paste0(prefix, "_profiles.txt")), 
                     skip = 4, nrows = P)[, -1]
  
  # if constrained PMF was run
  if(constrain) {
    stop("not tested!")
    
    # read constrained diagnostics, get profiles
    pathcon <- file.path(dir1, paste0(prefix, "_Constrained_diagnostics.txt"))
    constr <- readLines(pathcon)
    skips <- which(substr(constr, 1, 21) == "Factor Profiles (conc")
    constr <- read.table(pathcon, skip = skips + 1, nrows = P)[, -1]
    
  # else constr is missing
  } else{
    constr <- NULL
  }
  
  # format profile output
  profs <- formatprof(base, cn, constr, scale)  
  
  profs$call <- match.call()
  x <- dplyr::filter(profs$long, Type == "base") %>% select(., -Type)
  profs$wide <- spread(x, Source, value)
  
  class(profs) <- "pmfprof"
  
  # output profiles
  return(profs)
}

#' \code{formatprof} Formats PMF profile output
#' 
#' @title formatprof
#' @param base base profile data read from *_profiles.txt
#' @param constr constrained output
#' @param cn optional column names
#' @param scale whether to row standardize (default = F)
formatprof <- function(base, cn = NULL, constr = NULL, scale = F) {
  # Set number of sources
  L <- ncol(base) - 1
  
  # If no column names, name as factor
  if(is.null(cn)) {
    cn <- paste0("Factor", seq(1 : L))
  }
  
  # Create column names and apply
  cn1 <- c("cons", cn)
  colnames(base) <- cn1

  # Add type
  base <- mutate(base, Type = "base")
  
  # If constrained also, name columns
  if(!is.null(constr)) { 
    stop("not tested!")
    
    colnames(constr) <- cn1
    
    # Join with base
    constr <- mutate(constr, Type = "constraint") 
    prof <- full_join(base, constr)
    
  } else{
    prof <- base
  }

  # If scale, then row standardize
  if(scale) {
    # Get row sums
    pr1 <- select(prof, -cons, -Type)
    rs <- rowSums(pr1)

    prof <- mutate_at(prof, vars(-cons, -Type), funs(div(., rs = rs)))


  }
  
  # Reshape
  prof <- gather(prof, Source, value, -cons, -Type)

  # Reorder
  prof$Source <- factor(prof$Source, levels = cn)

  
  # Return formatted profiles
  return(list(long = prof))
}

#' @title div
#' @param vec vector of source
#' @param rs row sums
div <- function(vec, rs) {vec / rs}

#' @rdname pmfprof
#' @export
print.pmfprof <- function(x) {
  cat("Call:\n")
  print(x$call)
  cat("Profiles:\n")
  
  print(x$wide)
}

#' @rdname pmfprof
#' @export
plot.pmfprof <- function(x, scale = T, type = F, size = 12) {
  # type is whether to color by type, scale is whether proportion or units,
  # size is size of text on x axis
  # Create y label
  ylab1 <- ifelse(scale, "Proportion of species", "Concentration units")

  # If multiple types
  prof <- x$long
  if(type) {
    g1 <- ggplot(prof, aes(x = cons, y = value, fill = Type))

  } else{
    
    # Get number of constituents
    P <- length(unique(prof$cons))
    # Set color
    cols <- rep(brewer.pal(8, "Dark2"),length =  P)
    
    # Plot by constituent
    g1 <- ggplot(prof, aes(x = cons, y = value, fill = cons)) +
      scale_fill_manual(values = cols) 

  }

  # Add bars, formatting
  g1 <- g1 + geom_bar(stat = "identity", position = "dodge") +
    theme_bw() + xlab("") + ylab(ylab1) +
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(size = size, angle = 90, hjust = 1, vjust = 0.5)) +
    facet_wrap(~ Source, ncol = 2)  

  # if(!type) {
   g1 <- g1 + theme(legend.position = "top")
  # }
  g1
}




#' @export
summary.pmfprof <- function(x, ...) {
  nsource <- ncol(x$wide) - 1
  ncons <- nrow(x$wide) 
  
  
  res <- list(nsource = nsource, ncons = ncons, 
              call = match.call())
  class(res) <- "summary.pmfprof"
  
  return(res)
}


#' @export
print.summary.pmfprof <- function(x) {
  cat("Call:\n")
  x$call
  cat("\n")
  cat("Number of sources:", x$nsource, "\n")
  cat("Number of constituents:", x$ncons, "\b")
}

