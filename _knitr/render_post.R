#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)


# Source:https://chepec.se/2014/07/16/knitr-jekyll/

# inspiration sources:
# http://www.jonzelner.net/jekyll/knitr/r/2014/07/02/autogen-knitr/
# http://gtog.github.io/workflow/2013/06/12/rmarkdown-to-rbloggers/

KnitPost <- function(bashwd = "", convert_file = "", overwrite = FALSE) {
  
  # Run KnitPost(overwrite = T) to update all .rmd files.
  
  # CONVERT ALL RMD FILES TO MARKDOWN?
  #    REQUIRED: overwrite
  # CONVERT A SPECIFIC RMD FILE TO MARKDOWN?
  #    REQUIRED: bashwd, convert_file
  
  # bashwd: working directory (passed on from bash, only used below
  #         if a specific Rmd file is to be converted)
  # convert_file: name/path to specific Rmd file to convert
  # overwrite: flag that tells us whether to overwrite md files
  #            when converting all Rmd files
  
  
  # old working directory (added by Luca on 2021.01.31)
  old.wd <- getwd()
  # directory of jekyll blog (including trailing slash)
  site.path <- "/Users/lucabutikofer/Documents/GitHub/lucabutikofer.github.io/"
  # directory where your Rmd-files reside (relative to base)
  rmd.path <- paste0(site.path, "_knitr")
  # directory to save figures
  fig.dir <- "figures/"
  # directory for converted markdown files
  posts.path <- paste0(site.path, "_posts")
  # cache
  cache.path <- paste0(site.path, "_cache")
  
  require(knitr)
  render_jekyll(highlight = "none") # was "pygments" (Luca 2021.01.31)
  # "base.dir is never used when composing the URL of the figures; it is
  # only used to save the figures to a different directory, which can
  # be useful when you do not want the figures to be saved under the
  # current working directory.
  # The URL of an image is always base.url + fig.path"
  # https://groups.google.com/forum/#!topic/knitr/18aXpOmsumQ
  opts_knit$set(
    base.url = "/",
    base.dir = site.path)
  opts_chunk$set(
    fig.path   = fig.dir,
    fig.width  = 8.5,
    fig.height = 5.25,
    dev        = 'svg',
    cache      = FALSE,
    warning    = FALSE,
    message    = FALSE,
    cache.path = cache.path,
    tidy       = FALSE)
  
  
  if (convert_file == "") {
    # convert all Rmd files in {site-url}/_knitr/ to markdown files
    # contingent on whether overwrite requested and if md exists
    
    # setwd to Rmd folder
    setwd(rmd.path)
    
    files.rmd <-
      data.frame(rmd = list.files(
        path = rmd.path,
        full.names = TRUE,
        pattern = "\\.Rmd$",
        ignore.case = TRUE,
        recursive = FALSE))
    
    files.rmd$corresponding.md.file <-
      paste0(posts.path, "/",
             basename(gsub(pattern = "\\.Rmd$",
                           replacement = ".md",
                           x = files.rmd$rmd)))
    
    files.rmd$corresponding.md.exists <-
      file.exists(files.rmd$corresponding.md.file)
    files.rmd$md.overwrite <- overwrite
    files.rmd$md.render <- FALSE
    # check if corresponding md file exists for each Rmd file,
    # if not, set flag to convert to markdown
    # (also consider the overwrite flag set by user)
    for (i in 1:dim(files.rmd)[1]) {
      if (files.rmd$corresponding.md.exists[i] == FALSE) {
        files.rmd$md.render[i] <- TRUE
      }
      if ((files.rmd$corresponding.md.exists[i] == TRUE) &&
          (files.rmd$md.overwrite[i] == TRUE)) {
        files.rmd$md.render[i] <- TRUE
      }
    }
    
    # For each Rmd file, render markdown (contingent on the flags set above)
    for (i in 1:dim(files.rmd)[1]) {
      # only re-knit if overwrite==TRUE or .md not already existing
      if (files.rmd$md.render[i] == TRUE) {
        # KNITTING ====
        message(paste0("=== KnitPost(overwrite=", overwrite, "): ",
                       basename(files.rmd$rmd[i])))
        out.file <-
          knit(files.rmd$rmd[i],
               output = files.rmd$corresponding.md.file[i],
               envir = parent.frame(),
               quiet = TRUE)
      }
    }
  } else {
    # convert a single Rmd file to markdown
    # setwd to bash pwd
    setwd(bashwd)
    convert.path <- paste0(bashwd, "/", convert_file)
    md.path <-
      paste0(posts.path, "/",
             basename(gsub(pattern = "\\.Rmd$",
                           replacement = ".md",
                           x = convert_file)))
    # KNITTING ====
    message(paste0("=== KnitPost(", convert.path, ")"))
    out.file <-
      knit(convert.path,
           output = md.path,
           envir = parent.frame(),
           quiet = TRUE)
  }
  # return working directory to original folder
  # (added by Luca 2021.01.31)
  setwd(old.wd)
}