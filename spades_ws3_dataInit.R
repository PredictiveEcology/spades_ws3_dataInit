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

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # Prepare Python
  #TODO: make this a function
  needed <- c("numba>=0.58", "ws3", "datalad[full]", "geopandas", "git-annex","seaborn", "folium", "debugpy")
  reticulate::install_python(version = '3.12')

  # Setup virtual environment:
  venv <- "r-reticulate"
  if (reticulate::virtualenv_exists(venv)) {
    reticulate::py_install(needed)
  } else {
    reticulate::virtualenv_create(venv, packages = needed)
  }
  reticulate::use_virtualenv(venv)

  browser()

  git_submodule_add_in_SpaDES_module <- function(GithubURL,
                                                 install.path = file.path(modulePath(sim), currentModule(sim))) {
    # Move into the module directory
    origDir <- getwd()
    setwd(file.path(modulePath(sim), currentModule(sim)))
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

  # Run it:
  git_submodule_add_in_SpaDES_module(GithubURL=P(sim)$GithubURL,
                                     install.path=file.path("cccandies-demo-202503-input"))

  ## Prepare defaults:
  # Load default data via datalad:
  datalad<-import("datalad.api")           # load datalad module into reticulate

  # use datalad to fetch the actual files in the datalad repo, replacing the datalad placeholders.
  datalad$get(path = file.path(modulePath(sim), currentModule(sim),"cccandies-demo-202503-input"), recursive = TRUE)

  # Create hardlink between "modules/spades_ws3_dataInit/cccandies-demo-202503-input" and "input/cccandies-demo-202503-input"
  # CURRENTLY BROKEN SINCE THE FILES DON'T EXIST BECAUSE DATALAD HASN"T FETCHED THEM YET
  create_hardlink_tree <- function(source_dir, target_dir) {
    # List all files in source directory recursively
    files <- list.files(source_dir, recursive = TRUE, full.names = TRUE)

    # Filter out directories
    files <- files[file.info(files)$isdir == FALSE]

    # Create directories in target
    allDirs <- unique(dirname(files))
    relDirs <- sub(paste0("^", normalizePath(source_dir), "/?"), "", allDirs)
    lapply(file.path(target_dir, relDirs), dir.create, recursive = TRUE, showWarnings = FALSE)

    # Create hardlinks
    for (f in files) {
      rel_path <- sub(paste0("^", normalizePath(source_dir), "/?"), "", f)
      target_path <- file.path(target_dir, rel_path)
      if (!file.exists(target_path)) {
        file.link(f, target_path)
      } else {
        message("Hardlink already exists: ", target_path)
      }
    }

    message("Hardlink tree created from ", source_dir, " -> ", target_dir)
  }


  source_dir<-"modules/spades_ws3_dataInit/cccandies-demo-202503-input"
  target_dir<-"input/cccandies-demo-202503-input"

  # Example usage:
  create_hardlinks_tree(
    "modules/spades_ws3_dataInit/cccandies-demo-202503-input",
    "input/cccandies-demo-202503-input"
  )


  # Make directory if necessary (not needed until the input directory is swapped above)
  #if (!dir.exists(file.path(SpaDES.core::inputPath(sim),"cccandies_demo_input"))) {
  #  dir.create(file.path(SpaDES.core::inputPath(sim),"cccandies_demo_input"))
  #}





  file.path("")
  if (!SpaDES.core::suppliedElsewhere("hdt", sim)) {
    py <- import_builtins()
    pickle <- import("pickle")
    #TODO: explore cloning cccandies_demo_input into a subfolder,
    # get the data, and then copy it to a folder inside this module
    # which replaces use of inputPath below
    #browser()
    hdt.list <- lapply(SpaDES.core::P(sim)$basenames,
                       function(bn,
                                input = "modules/cccandies_demo_input",
                                hdtPath = SpaDES.core::P(sim)$hdtPath,
                                hdtPrefix = SpaDES.core::P(sim)$hdtPrefix) {
                         pklPath <- file.path(input, hdtPath, paste0(hdtPrefix, bn, ".pkl"))
                       }
    ) %>%
      lapply(., FUN = function(path) {pklPath <- (pickle$load(py$open(path, "rb")))})
    names(hdt.list) <- SpaDES.core::P(sim)$basenames
    sim$hdt <- hdt.list
  }

  if (!SpaDES.core::suppliedElsewhere("landscape", sim)) {
    rs.list <- lapply(P(sim)$basenames,
                      function(bn) {
                        file.path("modules/cccandies_demo_input", P(sim)$tif.path, bn, "inventory_init.tif")
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

  if (!SpaDES.core::suppliedElsewhere("studyArea", sim)) {
    #TODO: use the bcdata package instead of this googledrive file
    tsas <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1niq3Ms7mCPsnbRhbSqzThPUA0-Xfifmz/view?usp=drive_link",
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
