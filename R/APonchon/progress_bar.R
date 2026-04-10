progress_bar<-function(n_iter) {
  library(cli)
  
# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 75,   # Progress bar width. Defaults to getOption("width")
                     char = cli::col_cyan(cli::symbol$heart))    # Character used to create the bar

return(pb)

}
