defineModule(sim, list(
  name = "spades_ws3_dataInit",
  description = "This module prepares data for input to spades_ws3 module family. Currently this works with datalad repository prepared by the UBC-FRESH lab",
  keywords = c("harvesting","dataInit","WS3"),
  authors = list(
    person(given = "Ian", family = "Eddy",
           email = "ian.eddy@nrcan-rncan.gc.ca",
           role = c("aut", "cre")),
    person(given = "Allen Thomas", family = "Larocque",
           email = "allen.larocque@gmail.com",
           role = c("aut", "ctb"))
  ),
  childModules = character(0),
  version = list(spades_ws3_dataInit = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spades_ws3_dataInit.Rmd"),
  reqdPkgs = list("reticulate", "raster", 'dplyr', 'magrittr', 'googledrive','SpaDES.core','terra','bcdata'),
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
    createsOutput(objectName = "landscape", objectClass = "SpatRaster", desc = "landscape layers"),
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

  # Import the hdt tables:
  if (!SpaDES.core::suppliedElsewhere("hdt", sim)) {
    py <- import_builtins()
    pickle <- import("pickle")

    hdt.list <- lapply(P(sim)$basenames, function(bn) {
      path <- file.path(inputPath(sim), P(sim)$hdtPath, paste0(P(sim)$hdtPrefix, bn, ".pkl"))
      if (!file.exists(path)) stop("HDT file missing: ", path)
      pickle$load(py$open(path, "rb"))
    })
    names(hdt.list) <- P(sim)$basenames
    sim$hdt <- hdt.list
  }

  # Convert the datalad tifs and inventory to 'landscape' object:
  if (!SpaDES.core::suppliedElsewhere("landscape", sim)) {
    # Build full paths to all inventory rasters
    tif_files <- file.path(inputPath(sim), P(sim)$tif.path, P(sim)$basenames, "inventory_init.tif")

    rs.list <- lapply(tif_files, function(f) {
      r <- terra::rast(f)
      r <- terra::deepcopy(r)  # ensures a memory copy, not linked to disk
      r
    })
    names(rs.list) <- P(sim)$basenames  # Rename the list members their respective TSA names


    recompile.rs <- function(name, rsList = rs.list, hdtList = hdt.list) {
      mu.id <- as.integer(sub("^[A-Za-z]+", "", name))
      rs <-rsList[[name]]

      df <- as.data.frame(lapply(data.frame(do.call(rbind, hdtList[[name]])), unlist))
      df$key <- as.numeric(rownames(df))
      df[, 1] <- mu.id
      df <- df[, c(5, 1, 2, 3, 4)]
      colnames(df) <- c("key", "fmuid", "thlb", "au", "blockid")

      # Make sure key and raster cell values are both integers
      df$key <- as.integer(round(df$key))
      rs[[1]] <- round(rs[[1]])

      # Apply terra::subst() one column at a time
      r.fmuid   <- terra::subst(rs[[1]], df$key, df$fmuid)
      r.thlb    <- terra::subst(rs[[1]], df$key, df$thlb)
      r.au      <- terra::subst(rs[[1]], df$key, df$au)
      r.blockid <- terra::subst(rs[[1]], df$key, df$blockid)

      # Fix 'age' (force in memory)
      r.age <- rs[[2]]
      terra::values(r.age) <- terra::values(rs[[2]])

      # Fill in muid
      r.muid <- r.fmuid
      r.muid[!is.na(r.thlb)] <- mu.id

      # Combine all layers into one SpatRaster
      rb <- c(r.fmuid, r.thlb, r.au, r.blockid, r.age)
      names(rb) <- c("fmuid", "thlb", "au", "blockid", "age")

      return(rb)
    }


    # Use function 'recompile.rs' to recompile each TSA raster set:
    rs.list <- lapply(names(rs.list), function(nm) recompile.rs(nm, rs.list, hdt.list))

    # Remove TSA names to avoid mosaic() argument naming issues (delete this?)
    names(rs.list) <- NULL

    # If more than one TSA, mosaic them together (TODO: This doesn't work. Do I want a SpatRasterCollection?)
    if (length(rs.list) > 1) {
      #r_merged <- do.call(terra::mosaic, c(rs.list, fun = "mean"))  # Merge by using `mosaic`, which is slower but handles overlapping cells
      r_merged <- do.call(terra::merge, rs.list) # merge by using `merge`, which is faster but may break with overlapping cells
      sim$landscape <- r_merged
    } else {
      # If only one TSA — just stack its layers
      sim$landscape <- rs.list[[1]]
    }

    # Assign standard layer names
    names(sim$landscape) <- c("fmuid", "thlb", "au", "blockid", "age")

  }



  ## Get the studyArea if we don't already have it. Defaults to BC Timber Supply Areas from bcdata
  # Make a message if it is suppliedElsewhere:
  if (SpaDES.core::suppliedElsewhere("studyArea", sim)) {
    message("studyArea supplied elsewhere, skipping creation.")
  }
  # Set dPath directory
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #Use bcdata package to get the TSA boundary map
  if (!SpaDES.core::suppliedElsewhere("studyArea", sim)) {
    message("studyArea not supplied elsewhere; creating")
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

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- terra::rast(sim$landscape[[1]])
    sim$rasterToMatch[] <- 1  # give it an attribute, otherwise mask won't work
    sim$rasterToMatch <- terra::mask(sim$rasterToMatch, sim$studyArea)
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


