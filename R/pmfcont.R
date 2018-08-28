#' Reading in and formatting contributions data
#' 
#' \code{pmfcont} Read in contribution data
#' 
#' These are functions to read in and format PMF output contributions
#' 
#' @title pmfcont
#' @param dat full path to original constituent dataset
#' @param dir path to PMF result
#' @param prefix prefix of PMF result
#' @param constrain was constrained PMF run? (default = F)
#' @param cn optional column names for sources
#' @param scale whether to scale by total (default = F)
#' @param rms character vector of constituents removed from PMF run
#' @param formatdate date format if first column is date
#' @export
#' @examples 
#' # Save nycdat.csv and PMF output to working directory
#' cont <- pmfcont("nycdat.csv", prefix = "pmfdatex", formatdate = "%m/%d/%y")
#' print(cont)
#' plot(cont)
#' plot(cont, class = "hist")
#' summary(cont)
pmfcont <- function(x, ...) UseMethod("pmfcont")

#' @rdname pmfcont
#' @export
pmfcont.default <- function(dat, dir = "./", rms = NULL, prefix, constrain = F, 
                             cn = NULL, scale = F, formatdate = "%m/%d/%Y") {
  
  # read in constituent data
  dat <- read.table(dat, header = T)
  
  # remove constituents from original data
  if(!is.null(rms)) {
    dat <- dat[, -which(colnames(dat) %in% rms)]
  }
  
  # get dimensions
  P <- ncol(dat) - 1
  T <- nrow(dat)
  
  # Where to start: mean 1
  skips1 <- ifelse(!scale, 4, 4 + T + 5)

  # read in PMF contribution data
  base <- read.table(file.path(dir, paste0(prefix, "_contributions.txt")), 
                     skip = skips1, nrows = T,
                     stringsAsFactors = F)[, -1]
  
  # if constrained PMF was run
  if(constrain) {
    stop("not tested!")
    
    # read constrained diagnostics, get contributions
    pathcon <- file.path(dir1, paste0(prefix, "_Constrained_diagnostics.txt"))
    constr <- readLines(pathcon)
    skips <- which(substr(constr, 1, 30) == "Factor Contributions (avg = 1)")
    constr <- read.table(pathcon, skip = skips + 1, nrows = T)[, -1]
    
  # else constr is missing
  } else {
    constr <- NULL
  }
  
  # If scale
  if(scale) {
     # Get profiles
    stop("not tested!")
     pr1 <- pmfprof(dat, dir, prefix, scale = F, rms = rms)[[1]]
     # Find PM total
     pr1 <- dplyr::filter(pr1, cons == "FPM.Total")
     pr1 <- spread(pr1, Source, value)
  
  # Else no total
  } else {
    pr1 <- NULL
  }

  # Format contributions
  contr <- formatcontr(base, constr, cn, scale, tots = pr1, formatdate)  

  class(contr) <- "pmfcont"
  contr$call <- match.call()
  contr$wide <- filter(contr$contrib, Type == "base") %>%
    spread(., Source, value) %>% select(., -Type)
  contr$dat <- dat
  
  # output profiles
  return(contr)
}


#' \code{formatcontr} Formats PMF contributions output
#' 
#' @title formatcontr
#' @param base base contribution data read from *_contributions.txt
#' @param constr constrained output
#' @param cn optional column names
#' @param scale whether to row standardize (default = F)
#' @param tots total PM to scale by
#' @param formatdate date format if first column is date
formatcontr <- function(base, constr = NULL, cn = NULL, scale = F, 
                        tots = NULL, formatdate = "%m/%d/%Y") {
  
  # Set number of sources
  L <- ncol(base) - 1
  
  # If no column names, name as factor
  if(is.null(cn)) {
    cn <- paste0("Factor", seq(1 : L))
  }
  
  # Create column names and apply
  cn1 <- c("ID", cn)
  colnames(base) <- cn1
  
  # Add type
  base <- mutate(base, Type = "base")
  
  # If constrained also, name columns
  if(!is.null(constr)) {
    stop("not tested!")
    
    colnames(constr) <- cn1
    constr <- mutate(constr, Type = "constraint")
  }


  if(scale) {
    stop("Not tested!")
    # Which columns to ignore
    wh1 <- which(colnames(base) %in% c("ID", "Type"))
    # Multiply by total PM mass on average
    prbase <- dplyr::filter(tots, Type == "base")[, -c(1, 2)] %>% as.numeric()
    base[, -wh1] <- sweep(base[, -wh1], 2, prbase, "*")
    
    # If constrain
    if(!is.null(constr)) {
      stop("not tested!")
      
      prconstr <- dplyr::filter(tots, Type == "constraint")[, -c(1, 2)] %>% as.numeric()
      constr[, -wh1] <- sweep(constr[, -wh1], 2, prconstr, "*")
    }
  }

  # Join
  if(!is.null(constr)) {
    contrib <- full_join(base, constr)
  }else {
    contrib <- base
  }
  
  # Reshape
  contrib <- gather(contrib, Source, value, -ID, -Type)

  # Reorder
  contrib$Source <- factor(contrib$Source, levels = cn)
  
  # Firstdate
  if(!is.null(formatdate)) {
    contrib$ID <- as.Date(contrib$ID, format = formatdate)
  }
  
  # Return formatted contributions
  return(list(contrib = contrib))
}

#' @rdname pmfcont
#' @export
print.pmfcont <- function(x) {
  cat("Call:\n")
  print(x$call)
  cat("\n")
  print(x$contrib)
}

#' @rdname pmfcont
#' @export
plot.pmfcont <- function(x, class = "scatter", type = F, 
                             scale = F, size1 = 12, size2 = 1, ncol1 = 3) {
  # type is whether to color by type, scale is whether units or scale,
  # size2 is size of points, size1 is size of text on x axis
  # Create y label
  mlab1 <- ifelse(scale, "Concentration units", "Average = 1")

  contrib <- x$contrib
  # If multiple types
  if(type) {
    scatter <- ggplot(contrib, aes(x = ID, y = value, colour = Type)) 
    hist <- ggplot(contrib, aes(value, fill = Type), alpha = 0.3)
  }else {
    scatter <- ggplot(contrib, aes(x = ID, y = value, colour = Source))
    hist <- ggplot(contrib, aes(value, fill = Source), alpha = 0.3)
  }
  
  # plot
  scatter <- scatter + geom_point(size = size2) +
    theme_bw() + 
    xlab("ID") + 
    ylab(paste0("Contributions, ", mlab1)) + 
    theme(text = element_text(size = size1), axis.text.x = element_blank()) +
    facet_wrap(~ Source, ncol = ncol1, scales = "free")  
  
  hist <- hist + geom_histogram(position = "identity", alpha = 0.4) +
    scale_x_log10() +
    theme_bw() + 
    xlab(paste0("Contributions,", mlab1, " (log 10 scale)")) + 
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
    facet_wrap(~ Source, ncol = 3)  
  
  if(!type) {
    hist <- hist + theme(legend.position = "none")
    scatter <- scatter +  theme(legend.position = "none")
  }
  
  # return plot
  if(class == "scatter") {
    return(scatter)
  } else {
    return(hist)
  }


}

#' @title plotconstrain
#' @param contrib pmf contributions
#' @param scale whether to scale
#' @export
plotconstrain <- function(contrib, scale = F) {
  stop("not tested!")
  # Create y label
  mlab1 <- ifelse(scale, "Concentration units", "Average = 1")

  # Spread by type
  contrib <- spread(contrib, Type, value)

  # Plot scatter by type
  g1 <- ggplot(contrib, aes(x = base, y = constraint)) +
    geom_point() +  geom_abline(intercept = 0, slope = 1) +
    theme_bw() + 
    xlab("Base") + ylab("Constrained") +
    ggtitle(paste0("Contributions,", mlab1)) + 
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
    facet_wrap(~ Source, ncol = 3)  

  g1
}

#' @export
summary.pmfcont <- function(x, ...) {
  nobs <- nrow(x$wide)
  nsource <- ncol(x$wide) - 1
  ncons <- ncol(x$dat) - 1
  
  summ <- group_by(x$contrib, Source, Type) %>%
    summarize(., mn = mean(value), sd = sd(value),
              min = min(value), max = max(value))
  
  res <- list(nobs = nobs, nsource = nsource, summ = summ,
              call = match.call())
  class(res) <- "summary.pmfcont"
  
  return(res)
}


#' @export
print.summary.pmfcont <- function(x) {
  cat("Call:\n")
  x$call
  cat("\n")
  cat("Number of sources:", x$nsource, "\n")
  cat("Number of observations:", x$nobs, "\n")
  cat("Number of constituents:", x$ncons, "\b")
  cat("Summary of source contributions:\n")
  
  print(x$summ)
}
