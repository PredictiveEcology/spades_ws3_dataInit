# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spades_ws3_dataInit",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email = "email@example.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", spades_ws3_dataInit = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spades_ws3_dataInit.Rmd"),
  reqdPkgs = list("reticulate", "raster", 'dplyr', 'magrittr'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    #defineParameter("", NA, NA, NA, ""),
    defineParameter("basenames", "character", NA, NA, NA, "MU baseneames to load"),
    defineParameter("base.year", 'numeric', 2015, NA, NA, "base year of forest inventory data"),
    defineParameter("tifPath", "character", "tif", NA, NA, "Path to TIF raster inventory files"),
    defineParameter("hdtPath", "character", "hdt", NA, NA, "Path to pickled hdt files"),
    defineParameter("hdtPrefix", "character", "hdt_", NA, NA, "HDT filename prefix"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "landscape", objectClass = "RasterStack", desc = "landscape layers"),
    createsOutput(objectName = "hdt", objectClass = "list", desc = "stand development type hashcode decoder")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.spades_ws3_dataInit = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      #message("hello world!")
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spades_ws3_dataInit", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spades_ws3_dataInit", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spades_ws3_dataInit", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      sim <- Save(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "spades_ws3_dataInit", "save")
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spades_ws3_dataInit", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spades_ws3_dataInit", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.


### template sim$ages1ialization
Init <- function(sim) {

  py <- import_builtins()
  pickle <- import("pickle")

  hdt.list <- lapply(P(sim)$basenames, function(bn, input = inputPath(sim),
                                                hdtPath = P(sim)$hdtPath,
                                                hdtPrefix = P(sim)$hdtPrefix) {
    pklPath <- file.path(input, hdtPath, paste0(hdtPrefix, bn, ".pkl"))
  }) %>%
   lapply(., FUN = function(path) {
     pklPath <- (pickle$load(py$open(path, "rb")))
    })
  names(hdt.list) <- P(sim)$basenames

  rs.list <- lapply(P(sim)$basenames, function(bn) {
    file.path(inputPath(sim), P(sim)$tifPath, bn, "inventory_init.tif")
  }) %>%
   lapply(., raster::stack)
  names(rs.list) <- P(sim)$basenames

  # define a local function that recompiles RasterStack objects (yuck)
  recompile.rs <- function(name, rsList = rs.list) {
    mu.id = as.integer(substr(name, 4, 50))
    rs <- stack(rs.list[name])
    df <- as.data.frame(lapply(data.frame(do.call(rbind, hdt.list[[name]])), unlist)) # attributes as data.frame
    df$key <- as.double(rownames(df)) # add hashcode (index) as double column
    df <- df[, c(5, 1, 2, 3, 4)]# reorder so new key column in pos 1
    #Need raster:: or it collides with pryr::subs
    rb <- raster::subs(rs[[1]], df, which=2:5) # RasterBrick of substituted values (default compiled as factors... not sure how to avoid this)
    r.thlb <- deratify(rb, layer=2)
    r.muid <- raster(rs[[1]])
    r.muid[!is.na(r.thlb)] <- mu.id
    r.au <- deratify(rb, layer=3)
    r.blockid <- (1000000000 * r.muid) + rs[[3]]
    # r.age <- rs[[2]]
    #Ians temporary solution to stop age from being file-backed
    #browser()
    ageValues <- getValues(rs[[2]])
    r.age <- raster(rs[[2]]) %>%
    setValues(., ageValues)
    #r.age <- 10 * rs[[2]] # convert to age unit to years
    return(stack(r.muid, r.thlb, r.au, r.blockid, r.age))
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
  sim$hdt <- hdt.list
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  sim <- saveFiles(sim)
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$landscape)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
