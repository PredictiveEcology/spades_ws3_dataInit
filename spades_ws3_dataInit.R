defineModule(sim, list(
  name = "spades_ws3_dataInit",
  description = paste(
    "This module prepares data for input to spades_ws3 module family. Currently this works with datalad repository prepared by the UBC-FRESH lab"),
  keywords = c("harvesting","dataInit","WS3"),
  authors = c(
    person(c("Ian", "Middle"), "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Allen", "Thomas"), "Larocque", email = "allen.larocque@gmail.com", role = c("aut", "ctb"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", spades_ws3_dataInit = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spades_ws3_dataInit.Rmd"),
  reqdPkgs = list("reticulate", "raster", 'dplyr', 'magrittr', 'googledrive','SpaDES.core'),
  parameters = rbind(
    defineParameter("GithubURL", "character", NA, NA, NA,'URL of default data datalad repo'),
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



  ## Prepare Python Environment
  py_packages <- c("numba>=0.58", "ws3", "datalad[full]", "geopandas", "git-annex","seaborn", "folium", "debugpy","pulp")
  py_version<-'3.12'
  venv<-'r-reticulate'

  install_python_env(py_version= py_version,
                     py_packages= py_packages,
                     venv= venv)

  ## Prepare demo defaults:
  git_submodule_add_in_SpaDES_module(GithubURL=P(sim)$GithubURL,
                                     module.path=modulePath(sim),
                                     current.module.name=currentModule(sim))

  # Load demo default data via datalad:
  datalad<-import("datalad.api")           # load datalad module into reticulate
  this.module.path<-modulePath(sim)[grep(currentModule(sim), lapply(modulePath(sim), list.files))] # This is just modulePath, but adapted to be safe for multiple modulePaths. It just picks the directory that the current module is in
  datalad.dir<-file.path(this.module.path,currentModule(sim),"cccandies_demo_input")   # The directory to put the datalad files
  datalad$get(path = datalad.dir, recursive = TRUE)   # get the datalad files

  # Create softlinks between inputPath(sim) and the datalad directory:
  create_link_tree(
    source_dir=datalad.dir,
    target_dir=inputPath(sim)
  )


  ## Import the datalad input files prepared by the UBC-FRESH lab to work with SpaDES:
  # In this implementation, the ONLY interaction is through the 'age' attribute

  # Import the hdt tables:
  if (!SpaDES.core::suppliedElsewhere("hdt", sim)) {
    py <- import_builtins()
    pickle <- import("pickle")
    hdt.list <- lapply(SpaDES.core::P(sim)$basenames,
                       function(bn,
                                input = inputPath(sim),
                                hdtPath = SpaDES.core::P(sim)$hdtPath,
                                hdtPrefix = SpaDES.core::P(sim)$hdtPrefix) {
                         pklPath <- file.path(input, hdtPath, paste0(hdtPrefix, bn, ".pkl"))
                       }
    ) %>%
      lapply(., FUN = function(path) {pklPath <- (pickle$load(py$open(path, "rb")))})
    names(hdt.list) <- SpaDES.core::P(sim)$basenames
    sim$hdt <- hdt.list
  }

  # Convert the datalad tifs and inventory to 'landscape' object:
  if (!SpaDES.core::suppliedElsewhere("landscape", sim)) {
    rs.list <- lapply(P(sim)$basenames,  # read in all the FSA inputs as a list of RasterStacks #TODO: update to Terra?
                      function(bn) {
                        file.path(inputPath(sim), P(sim)$tif.path, bn, "inventory_init.tif")
                      }
    ) %>%
      lapply(., raster::stack)
    names(rs.list) <- P(sim)$basenames                      # Rename the list the TSA names

    # "Recompile rasterstack" function:
    # This takes the raster stack in rs.list restructures the raster stack and merges it with attribute data
    recompile.rs <- function(name, rsList = rs.list) {
      mu.id = as.integer(substr(name, 4, 50))    # The first 3 characters are presumed to be "TSA"; 50 is just a big number
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

    # Now, apply function 'recompile.rs' to each element of rs.list:
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


  ## Get the studyArea if we don't already have it. Defaults to BC Timber Supply Areas from bcdata
    # Set dPath directory
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # Use bcdata package to get the TSA boundary map
  if (!SpaDES.core::suppliedElsewhere("studyArea", sim)) {

    tsa_id <- "8daa29da-d7f4-401c-83ae-d962e3a28980"  # Timber Supply Areas map

    # Define where the local copy should be saved
    local_gpkg <- file.path(dPath, "tsa_bcdata.gpkg")

    # 1. Download from BC Data Catalogue using bcdata
    if (!file.exists(local_gpkg)) {
      message("Downloading TSA polygons from BC Data Catalogue ...")
      tsa_sf <- bcdata::bcdc_get_data(tsa_id)
      sf::st_write(tsa_sf, local_gpkg, delete_dsn = TRUE)
    }

    # 2. Use prepInputs for reprojection, masking, and caching
    tsas <- reproducible::prepInputs(
      targetFile = local_gpkg,
      destinationPath = dPath,
      fun = "terra::vect",
      projectTo = sim$landscape,   # ensure CRS match
      cachePath = dPath
    )

    # 3. Filter and aggregate to create studyArea
    tsas$charTSA <- paste0("tsa", tsas$TSA_NUMBER)                   # Extract vector of TSA number and append 'tsa' such that it matches the 'basenames'
    tsas <- tsas[tsas$charTSA %in% unlist(P(sim)$basenames), ]
    tsas$foo <- 1                                                    # Dummy variable
    tsas <- terra::aggregate(tsas, by = tsas$foo, fun = mean)        # Aggregate into a single polygon. Take the mean

    sim$studyArea <- tsas
  }

  return(invisible(sim))
}

#####################
# Functions:

## "Install Python Environment" function:
# This installs pyton of a given version, installs packages, and creates a virtual environment
install_python_env <- function(py_version, py_packages, venv) {
  reticulate::install_python(version = py_version)

  if (reticulate::virtualenv_exists(venv)) {
    reticulate::py_install(py_packages)
  } else {
    reticulate::virtualenv_create(venv, packages = py_packages)
  }
  reticulate::use_virtualenv(venv)
}

## 'Add Git Submodule to a SpaDES Module' function:
# This pulls a github module and installs it as a github submodule of your SpaDES module
git_submodule_add_in_SpaDES_module <- function(module.path, current.module.name,GithubURL) {

  # Move into the module directory
  module.path.currentmodule<-module.path[grep(current.module.name, lapply(module.path, list.files))]
  install.path = file.path(module.path.currentmodule,current.module.name)

  origDir <- getwd()
  setwd(install.path)
  on.exit(setwd(origDir))

  # Normalize relative path
  submodule_path <- normalizePath(install.path, mustWork = FALSE)

  # Check if already in submodule list
  existing <- tryCatch(
    basename(gert::git_submodule_list()$path),
    error = function(e) character(0) # if no submodules exist yet
  )

  already_there <- dir.exists(submodule_path) ||
    basename(submodule_path) %in% existing

  if (already_there) {
    message("Submodule already exists at ", submodule_path, ". Skipping add.")
  } else {
    message("Adding submodule from ", GithubURL, " into ", submodule_path)
    gert::git_submodule_add(url = GithubURL, path = submodule_path)
  }
}


## 'Create Link Tree' function:
# This creates symlinks between source_dir and target_dir:
create_link_tree <- function(source_dir, target_dir) {
  # Normalize paths for consistency
  source_dir <- normalizePath(source_dir, mustWork = TRUE)
  target_dir <- normalizePath(target_dir, mustWork = TRUE)

  # List all files in source directory recursively (exclude directories)
  files <- list.files(source_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)

  # Compute relative paths (portable across OS)
  rel_paths <- substring(files, nchar(source_dir) + 2)

  # Ensure all needed directories exist in target
  rel_dirs <- unique(dirname(rel_paths))
  lapply(file.path(target_dir, rel_dirs), dir.create, recursive = TRUE, showWarnings = FALSE)

  # Create symlinks
  for (i in seq_along(files)) {
    target_path <- file.path(target_dir, rel_paths[i])
    if (!file.exists(target_path)) {
      ok <- file.symlink(from = files[i], to = target_path)
      if (!ok) warning("Failed to create symlink: ", target_path)
    } else {
      message("Symlink already exists: ", target_path)
    }
  }

  message("Symlink tree created from ", source_dir, " -> ", target_dir)
}


