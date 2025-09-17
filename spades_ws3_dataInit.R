defineModule(sim, list(
  name = "spades_ws3_dataInit",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email = "email@example.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", spades_ws3_dataInit = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spades_ws3_dataInit.Rmd"),
  reqdPkgs = list("reticulate", "raster", 'dplyr', 'magrittr', 'googledrive'),
  parameters = rbind(
    defineParameter("basenames", "character", NA, NA, NA,'vector of MU baseneames to load, beginning with tsa, e.g. "tsa40"'),
    defineParameter("base.year", 'numeric', 2015, NA, NA, "base year of forest inventory data"),
    defineParameter("tif.path", "character", "tif", NA, NA, "Path to TIF raster inventory files"),
    defineParameter("hdtPath", "character", "hdt", NA, NA, "Path to pickled hdt files"),
    defineParameter("hdtPrefix", "character", "hdt_", NA, NA, "HDT filename prefix"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatVector", desc = "study area in BC - made of TSAs", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "landscape", objectClass = "RasterStack", desc = "landscape layers"),
    createsOutput(objectName = "hdt", objectClass = "list", desc = "stand development type hashcode decoder")
  )
))

## event types

doEvent.spades_ws3_dataInit = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spades_ws3_dataInit", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spades_ws3_dataInit", "save")
    },
    plot = {},
    save = {
      sim <- Save(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "spades_ws3_dataInit", "save")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {

  return(invisible(sim))
}


Save <- function(sim) {

  return(invisible(sim))
}


plotFun <- function(sim) {
  return(invisible(sim))
}


.inputObjects <- function(sim) {

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #Python
  #TODO: make this a function
  needed <- c("numba>=0.58", "ws3", "datalad[full]", "geopandas", "git-annex")

  browser()
  reticulate::install_python(version = '3.12')

  venv <- "r-reticulate"
  if (reticulate::virtualenv_exists(venv)) {
    reticulate::py_install(needed)
  } else {
    reticulate::virtualenv_create(venv, packages = needed)
  }
  reticulate::use_virtualenv(venv)

  Sys.setenv(RETICULATE_PYTHON = paste0("~/.virtualenvs/", venv, "bin/python3.12"))
  system("cd modules/cccandies_demo_input && datalad get input . -r")

  # make sure that datalad-managed input files have all been downloaded from the cloud
  system("datalad get input -r")

  #if (!file.exists(file.path(inputPath(sim), Par$tif.path))) {
  #  dataTarGz <- "/srv/shared-data/cccandies-demo-202503-input.tar.gz"
  #  if (!dir.exists(dirname(dataTarGz)))
  #    stop("This module currently only works with untarred data from:\n", basename(dataTarGz))
  #  localTarGz <- file.path(inputPath(sim), basename(dataTarGz))
  #  file.copy(dataTarGz, localTarGz)
  #  untar(localTarGz, exdir = dirname(inputPath(sim)))
  #}

  if (!suppliedElsewhere("hdt", sim)) {
    py <- import_builtins()
    pickle <- import("pickle")
    #TODO: explore cloning cccandies_demo_input into a subfolder,
    # get the data, and then copy it to a folder inside this module
    # which replaces use of inputPath below
    hdt.list <- lapply(P(sim)$basenames,
                       function(bn,
                                input = inputPath(sim),
                                hdtPath = P(sim)$hdtPath,
                                hdtPrefix = P(sim)$hdtPrefix) {
                         pklPath <- file.path(input, hdtPath, paste0(hdtPrefix, bn, ".pkl"))
                       }
    ) %>%
      lapply(., FUN = function(path) {pklPath <- (pickle$load(py$open(path, "rb")))})
    names(hdt.list) <- P(sim)$basenames
    sim$hdt <- hdt.list
  }

  if (!suppliedElsewhere("landscape", sim)) {
    rs.list <- lapply(P(sim)$basenames,
                      function(bn) {
                        file.path(inputPath(sim), P(sim)$tif.path, bn, "inventory_init.tif")
                      }
    ) %>%
      lapply(., raster::stack)
    names(rs.list) <- P(sim)$basenames
    recompile.rs <- function(name, rsList = rs.list) {
      mu.id = as.integer(substr(name, 4, 50))
      rs <- raster::stack(rs.list[name])
      df <- as.data.frame(lapply(data.frame(do.call(rbind, hdt.list[[name]])), unlist)) # attributes as data.frame
      df$key <- as.double(rownames(df)) # add hashcode (index) as double column
      df <- df[, c(5, 1, 2, 3, 4)]# reorder so new key column in pos 1
      #Need raster or it collides with pryr::subs
      # RasterBrick of substituted values (default compiled as factors... not sure how to avoid this)
      rb <- raster::subs(rs[[1]], df, which=2:5)
      r.thlb <- deratify(rb, layer=2)
      r.muid <- raster(rs[[1]])
      r.muid[!is.na(r.thlb)] <- mu.id
      r.au <- deratify(rb, layer=3)
      r.blockid <- (1000000000 * r.muid) + rs[[3]]
      # r.age <- rs[[2]]
      ###############################################################
      # temporary solution to stop age from being file-backed
      ageValues <- getValues(rs[[2]])
      r.age <- raster(rs[[2]]) %>% setValues(., ageValues)
      ###############################################################
      return(raster::stack(r.muid, r.thlb, r.au, r.blockid, r.age))
    }
    rs.list <- lapply(names(rs.list), recompile.rs)
    # prep rs for use as arg in do.call wrapper to raster::mosaic function
    names(rs.list) <- NULL # else TSA names will be interpreted as arg names by raster::mosaic
    if (length(P(sim)$basenames) > 1) {
      rs.list$fun <- mean
      rs.list$na.rm <- TRUE
      rb <- do.call(mosaic, rs.list)
      sim$landscape <- raster::stack(rb)
    } else {
      sim$landscape <- raster::stack(rs.list)
    }
    names(sim$landscape) <- c('fmuid', 'thlb', 'au', 'blockid', 'age')
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    #TODO: use the bcdata package instead of this googledrive file
    tsas <- prepInputs(url = "https://drive.google.com/file/d/1niq3Ms7mCPsnbRhbSqzThPUA0-Xfifmz/view?usp=drive_link",
                       destinationPath = dPath,
                       projectTo = sim$landscape,
                       fun = "terra::vect")
    tsas$charTSA <- paste0("tsa", tsas$TSA_NUMBER)
    tsas <- tsas[tsas$charTSA %in% unlist(P(sim)$basenames),]
    tsas$foo <- 1
    #study area must be a single polygon
    tsas <- aggregate(tsas, field = "foo", fun = mean)
    sim$studyArea <- tsas
  }

  return(invisible(sim))
}
