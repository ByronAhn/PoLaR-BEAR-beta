## THIS IS ONLY A PARTIAL VERSION OF POLAR BEAR (AS OF JAN 6 '22)
## OTHER FUNCTIONALITY IS BEING WRITTEN NOW, AND WILL BE A SIGNIFICANT PART OF WHAT POLAR BEAR DOES

###############################################################
###############################################################
###############################################################
###                                                         ###
###  PoLaR-Based Extraction and Analysis in R (PoLaR BEAR)  ###
###                                                         ###
###############################################################
###############################################################
###############################################################

if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"PraatR" %in% installed.packages()) devtools:::install_github("usagi5886/PraatR")
if (!"rPraat" %in% installed.packages()) install.packages("rPraat")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(dygraphs)

# PraatR information: http://www.aaronalbin.com/praatr/index.html
library(PraatR)

# rPraat manual: https://github.com/bbTomas/rPraat/blob/master/rPraat.pdf
# rPraat demo: https://fu.ff.cuni.cz/praat/rDemo.html
library(rPraat)


##############################################################
##############################################################
##  BELOW ARE THE FUNCTIONS USED ACROSS ANALYSIS FUNCTIONS  ##
##############################################################
##############################################################


# function:   FullPath
# purpose:    writes out the directory path to the 
# arguments:  FileName (the name of the file) and Directory (a Directory which, if not specified in the call, refers to WorkingDirectoryFullPath; if that's missing, it defaults to a subfolder of the current working directory called "data")
# return:     a directory path to a file as a string
FullPath <- function(FileName, Directory){
  if(missing(Directory)){
    if (exists("WorkingDirectoryFullPath", envir = .GlobalEnv)){
      Directory <- WorkingDirectoryFullPath
    }
    else {
      Directory <- file.path(getwd(),"data")
    }
  }
  
  return( file.path(Directory,FileName) )
}


# function:   setPoLaRvars
# purpose:    sets values for variables used in analysis
# arguments:  all the "advanced pitch settings" variables -- all set here with defaults from PoLaR guidelines document
# return:     n/a
setPoLaRvars <- function(time_step = 0.0025, number_of_candidates = 15, very_accurate = "no", silence_threshold = 0.03, voicing_threshold = 0.5, octave_cost = 0.05, octave_jump_cost = 0.5, voice_unvoiced_cost = 0.2, use_Raw_Hz = 1, use_Semitones = 0)
{
  assign("time_step", time_step, envir = .GlobalEnv)
  assign("number_of_candidates", number_of_candidates, envir = .GlobalEnv)
  assign("very_accurate", very_accurate, envir = .GlobalEnv)
  assign("silence_threshold", silence_threshold, envir = .GlobalEnv)
  assign("voicing_threshold", voicing_threshold, envir = .GlobalEnv)
  assign("octave_cost", octave_cost, envir = .GlobalEnv)
  assign("octave_jump_cost", octave_jump_cost, envir = .GlobalEnv)
  assign("voice_unvoiced_cost", voice_unvoiced_cost, envir = .GlobalEnv)
  assign("use_Raw_Hz", use_Raw_Hz, envir = .GlobalEnv)
  assign("use_Semitones", use_Semitones, envir = .GlobalEnv)
}


# function:   rmPoLaRvariables
# purpose:    delete variables from the global environment that are set by setPoLaRvars
# arguments:  n/a
# return:     n/a
rmPoLaRvariables <- function(){
  # top-level-specified variables:
  if (exists("time_step", envir=.GlobalEnv)){
    rm(list = c("time_step", 
                "number_of_candidates", 
                "very_accurate", 
                "silence_threshold", 
                "voicing_threshold", 
                "octave_cost", 
                "octave_jump_cost", 
                "voice_unvoiced_cost", 
                "use_Raw_Hz", 
                "use_Semitones"), envir = .GlobalEnv)
  }
}


# function:   gatherPraatFiles
# purpose:    creates R data / variables, based on Praat files
# arguments:  BaseName (the base name used by the .TextGrids / .wav Files in the folder specified by the variable "WorkingDirectoryFullPath")
# return:     n/a
gatherPraatFiles <- function(BaseName){
  # set the value for the global variable, theName
  assign("theName", BaseName, envir = .GlobalEnv)
  
  # create TextGrid data, using the rPraat function tg.read(), and save it as global data named "theTg"
  assign("theTg", tg.read(FullPath(paste0(BaseName,".TextGrid")), encoding = "UTF-8"), envir = .GlobalEnv)
  
  # create data about where the PoLaR tiers are, using the function defined in this file, findPolaRTiers(), and save it as global data named "theTiers"
  assign("theTiers", findPoLaRTiers(theTg), envir = .GlobalEnv)
  
  # create Sound data, using the rPraat function snd.read(), and save it as global data named "theSound"
  assign("theSound", snd.read(FullPath(paste0(BaseName,".wav"))), envir = .GlobalEnv)
  
  # get the global F0 min/max, using the function defined in this file, findGlobalMinMax(), and save it as global variables
  globalminmax <- findGlobalMinMax(BaseName, theTg)
  assign("globalF0Min", globalminmax[1], envir = .GlobalEnv)
  assign("globalF0Max", globalminmax[2], envir = .GlobalEnv)
  
  # create a PitchTier file, if it doesn't exist:
  if (!file.exists(FullPath(paste0(BaseName,".PitchTier")))){
    # use the PraatR function, praat(), to have the Praat binaries create a .Pitch file:
    praat("To Pitch (ac)...", 
          arguments=list(time_step, globalF0Min, number_of_candidates, very_accurate, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voice_unvoiced_cost, globalF0Max),
          input=FullPath(paste0(BaseName,".wav")),
          output=FullPath(paste0(BaseName,".Pitch")),
          filetype="short",
          overwrite=TRUE)
    
    # use the PraatR function, praat(), to have the Praat binaries convert the .Pitch file to a .PitchTeir file:
    praat("Down to PitchTier",
          input=FullPath(paste0(BaseName,".Pitch")),
          output=FullPath(paste0(BaseName,".PitchTier")),
          filetype="short",
          overwrite=TRUE)
  }
  
  # create Pitch Tier  data, using the rPraat function pt.read(), and save it as global data named "thePitch"
  assign("thePitch", pt.read(FullPath(paste0(BaseName,".PitchTier"))), envir = .GlobalEnv)
}


# function:   rmPraatFiles
# purpose:    delete data/variables from the global environment that are set by gatherPraatFiles
# arguments:  n/a
# return:     n/a
rmPraatFiles <- function(){
  # from gatherPraatFiles:
  if (exists("theSound", envir=.GlobalEnv)){
    rm(list = c("theName",
                "theTg",
                "theTiers",
                "theSound",
                "thePitch",
                "globalF0Min",
                "globalF0Max"), envir = .GlobalEnv)
  }
}


# function:   parseRanges
# purpose:    extract F0 min/max information encoded in the Ranges tier, if it exists (works on basic and advance PoLaR labels)
# arguments:  intervalLabel (a string with a Ranges tier label (e.g. "200-345"))
# return:     an array whose first element is the min value and a whose second is the max value
parseRanges <- function(intervalLabel){
  localMin <- NA
  localMax <- NA
  # Parse the Ranges label into two strings: one for either side of the dash(es)
  splitlabels <- str_split(intervalLabel, '[-–—]+', 2, simplify = TRUE)
  leftOfSep <- splitlabels[1]
  rightOfSep <- splitlabels[2]
  
  # For the left side of the dash (i.e., the min):
  # Check if there are parentheses labels. If so, extract the number that comes after the open-parens.
  # If not, extract the number from the left-side of the dash.
  llParen <- grep('\\(', leftOfSep)
  if (length(llParen) > 0) {
    x <- str_split(leftOfSep, '\\(', 2, simplify = TRUE)
    localMin <- as.integer(str_match(x[1,2], '\\d+')[1,1])
  } else {
    localMin <- as.integer(str_match(leftOfSep, '\\d+')[1,1])
  }
  
  # For the right side of the dash (i.e., the max):
  # Check if there are parentheses labels. If so, extract the number that comes after the open-parens.
  # If not, extract the number from the right-side of the dash.
  rlParen <- grep('\\(', rightOfSep)
  if (length(rlParen) > 0) {
    x <- str_split(rightOfSep, '\\(', 2, simplify = TRUE)
    localMax <- as.integer(str_match(x[1,2], '\\d+')[1,1])
  } else {
    localMax <- as.integer(str_match(rightOfSep, '\\d+')[1,1])
  }
  
  # return an array with the min as the first item and the max as the second
  return(c(as.integer(localMin), as.integer(localMax)))
}


# function:   findGlobalMinMax
# purpose:    extract the global F0 min (i.e. the lowest min) and the global F0 max (i.e. the highest max), as encoded in the Ranges tier, if it exists
# arguments:  BaseName (the base name of the files)
#             TextGrid (the TextGrid data created by rPraat)
# return:     an array whose first element is the global min value and a whose second is the global max value
findGlobalMinMax <- function(BaseName, TextGrid){
  # check if the Ranges tier was identified by findPoLaRTiers()
  if (theTiers["Ranges", "tier.num"] > 0){
    # set some baseline min and max values:
    globalMin <- 10000
    globalMax <- 0
    
    # gets the number of intervals on the Ranges tier of the textgrid:
    numRanges <- tg.getNumberOfIntervals(TextGrid, theTiers["Ranges", "tier.name"])
    
    # a counter variable that will be stepped every time there is a properly labelled Ranges tier
    numLabeledRanges <- 0
    
    # a for loop to step through every Ranges tier label, to look for min/max values that are lower/higher than previously found
    for (x in 1:numRanges) {
      # gets the text name of the xth interval on the Ranges tier of the textgrid
      intervalLabel <- tg.getLabel(TextGrid, theTiers["Ranges", "tier.name"], index = x)
      
      # check if there are at least two numbers in the string intervalLabel (which is necessary for a proper PoLaR label)
      if (length(unlist(str_match_all(intervalLabel, "\\d"))) >= 2){
        
        # sends the label to parseRanges(), which returns the min/max as integers (if it can)
        localMinMax <- parseRanges(intervalLabel)
        
        # checks to see that praseRanges() has returned numbers > 0
        if (length(localMinMax[1]) != 0 && length(localMinMax[2]) != 0){
          # if it has, step the counter:
          numLabeledRanges <- numLabeledRanges+1
        }
        
        # checks whether the xth interval has a min lower than the current value of globalMin; if so, set it as the new value of globalMin
        if (localMinMax[1] < globalMin){globalMin <- localMinMax[1]}
        
        # checks whether the xth interval has a max higher than the current value of globalMax; if so, set it as the new value of globalMax
        if (localMinMax[2] > globalMax){globalMax <- localMinMax[2]}
      }
    } #endfor
    
    # if there were no PoLaR-labelled ranges in the Ranges tier…
    if (numLabeledRanges == 0){
      # … tell the user:
      print(paste('The file,', BaseName, ', lacks any PoLaR-labelled Ranges'))
      
      # … and use a very wide pitch range:
      print('A global min/max of 50/550 has been assumed')
      return(c(50,550))
    }
    # if there is at least one PoLaR-labelled range…
    else 
    {
      # return its an array…
      return(c(as.integer(globalMin), as.integer(globalMax)))
    } 
  }
  # if there is no Ranges tier…
  else {
    # … tell the user:
    print(paste('The file,', BaseName, ', lacks a Ranges tier'))
    # … and use a very wide pitch range:
    print('A global min/max of 50/550 has been assumed')
    return(c(50,550))
  }
}


# function:   tierNum
# purpose:    searches TextGrid data for a tier whose name contains a search string (case-insensitive)
# arguments:  SearchString (the string being searched for in the names of the TextGrid's tiers)
#             TextGrid (the TextGrid data created by rPraat)
# return:     an integer of the tier number -- the first match if there are multiple matches; NA if there is no match
tierNum <- function(SearchString, TextGrid){
  TFlist <- sapply(names(TextGrid), function(x) {any(grepl(SearchString, tolower(x)))})
  if (TRUE %in% TFlist) {
    return(as.integer(which(TFlist==TRUE)[1]))
  } else {
    return(NA)
  }
}


# function:   tierName
# purpose:    searches TextGrid data for a tier whose name contains a search string (case-insensitive)
# arguments:  SearchString (the string being searched for in the names of the TextGrid's tiers)
#             TextGrid (the TextGrid data created by rPraat)
# return:     the name of the tier -- the first match if there are multiple matches; NA if there is no match
tierName <- function(SearchString, TextGrid){
  TFlist <- sapply(names(TextGrid), function(x) {any(grepl(SearchString, tolower(x)))})
  if (TRUE %in% TFlist) {
    return(names(which(TFlist==TRUE)[1]))
  } else {
    return(NA)
  }
}


# function:   findPoLaRTiers
# purpose:    creates a dataframe, where rows are official PoLaR tier names, and the following columns contain the tier names as used in the TextGrid, and the tier number that they are in that textgrid
# arguments:  TextGrid (the TextGrid data created by rPraat)
# return:     a dataframe that lists  where to find each PoLaR tier name in the TextGrid data
findPoLaRTiers <- function(TextGrid){
  # a vector of all the official PoLaR tier names:
  rowsvector <- c("Words", "Phones", "PrStr", "Points", "Levels", "ST.Levels", "Ranges", "Misc", "Pseudo")
  
  # an empty vector, for all the tiers' names, as they occur in the TextGrid data:
  namesvector <- c()
  # an empty vector, for all the tiers' numbers, as they occur in the TextGrid data:
  numvector <- c()
  # an empty vector, for all the tiers' lengths, as they occur in the TextGrid data:
  lengthvector <- c()
  
  # a for loop that looks for tiers in the TextGrid data, for each of the official PoLaR tier names:
  for (name in rowsvector){
    # call tierNum() and tierName() to get the tier names/numbers for each of these names
    tiernum <- tierNum(tolower(name), TextGrid)
    tiername <- tierName(tolower(name), TextGrid)
    tierlength <- length(TextGrid[[tiername]]$label)
    
    # add the results of those functions to the vectors created above
    numvector <- append(numvector,tiernum)
    namesvector <- append(namesvector, tiername)
    lengthvector <- append(lengthvector, tierlength)
  }
  
  # return a dataframe, where each row is an official PoLaR tier name, and the columns are the tier number  and tier name:
  return(data.frame(numvector,namesvector,lengthvector, row.names = rowsvector) %>% `colnames<-`(c("tier.num", "tier.name", "tier.length")))
}


# function:   getF0Here
# purpose:    determine what the script should consider as the f0 value, at a specific point in time
#             ALGORITHM: Get the f0 at the time 'PointTime' by either (in order of priority)
#                 - extracting the number from a "comma label" in the Points tier object
#                 - measuring the f0 using Praat's tracking
# arguments:  PointTime (the time of the Point being considered
#             PointLabel (the label of the Point being considered)
#             optional: PitchTier (the Pitch Tier data, as previously generated by rPraat)
# return:     a numerical value, in Hz, for the f0 measured for the time
getF0Here <- function(PointTime, PointLabel, PitchTier){
  # if no argument for PitchTier is passed, use the data created by gatherPraatFiles()
  if(missing(PitchTier)) {
    PitchTier <- thePitch
  }
  
  # Start by initiating a variable with no value:
  thisF0 <- NA
  
  # look for a comma in the label:
  comma_placement <- unlist(gregexpr("[,(]", PointLabel))
  
  # if there is a comma, return that value
  if (comma_placement > -1){
    # call readCommaValue() to read the f0 value after the comma:
    commavalue <- readCommaValue(PointTime, PointLabel)
    
    # if readCommaValue() failed to parse a comma, look up the f0 directly, in the Pitch Tier data, using :
    if (is.na(commavalue)){
      thisF0 <- getF0fromPitchTier(PointTime, PitchTier)
    }
    # if readCommaValue() succeeded, use the comma value:
    else {
      thisF0 <- commavalue
    }
  }
  # if there is no comma, mesure the f0 directly, look up the f0 directly, in the Pitch Tier data:
  else{
    thisF0 <- getF0fromPitchTier(PointTime, PitchTier)
  }
  
  return(thisF0)
}


# function:   readCommaValue
# purpose:    extract a value from a "comma override" label in a Points tier object
# arguments:  the label of the Point being considered (thePointLabel)
#             the time of the Point being considered (thePointTime)
# return:     a numerical value, corresponding to the numerical value after the comma in the label of the Point
readCommaValue <- function(PointTime, PointLabel){
  # get the string after the first comma:
  afterTheComma <- unlist(str_split(PointLabel, "[,()]"))[2]  # note: as a sort of error-proofing, parentheses used instead of commas are also parsed here
  
  # Parse out a number from after the comma
  commaOverride <- as.integer(str_match(afterTheComma, '\\d+')[1,1])
  
  # Check to see if the parse was successful
  if (is.na(commaOverride)){
    # If parse was unsuccessful, first tell the user this happened:
    print(paste0('>> ALERT <<   (file: ', theName,')'))
    print(paste('A Points tier label had a “comma label” that could not be parsed, at time', PointTime))
    if (exists("thePitch")){
      print('The F0 was measured directly at this time, instead of using a comma override label.')
      # …and then measure the f0 directly:
      commaOverride <- getF0fromPitchTier(PointTime, thePitch) 
    }
  }
  
  return(commaOverride)
}


# function:   getF0fromPitchTier
# purpose:    get an f0 value for a time, averaged over 5 ms window centered at a the time for this Point (averages used to help avoid problems with f0 tracking)
# arguments:  PointTime (the time of the Point being considered)
#             optional: PitchTier (the Pitch Tier data, as previously generated by rPraat)
# return:     a numerical value, in Hz, representing the f0 at a given time
getF0fromPitchTier <- function(PointTime, PitchTier){
  # if no argument for PitchTier is passed, use the data created by gatherPraatFiles()
  if(missing(PitchTier)) {
    PitchTier <- thePitch
  }
  
  # define a 5ms window, centered on the time of the Point
  windowStart <- PointTime - 0.0025
  windowEnd <- PointTime + 0.0025
  
  # set to vectors, based on the Pitch Tier object
  pitchTimes <- PitchTier$t
  pitchValues <- PitchTier$f
  
  # create a vector containing all the f0 values in the time span defined by windowStart and windowEnd
  rangeOfValues <- pitchValues[which.min(abs(pitchTimes-windowStart)):which.min(abs(pitchTimes-windowEnd))]
  
  # average all non-zero/non-NA values in that vector
  resultF0 <- mean(rangeOfValues[rangeOfValues!=0 & !is.na(rangeOfValues)])
  
  # In the off chance that there is no f0 in that time span…
  #   …step back in time until an f0 value is found
  #   …step forward in time until an f0 value is found 
  #   _and average those two
  if (resultF0==0 | is.nan(resultF0)){
    # get the pitch from preceding measure
    xstart <- which.min(abs(pitchTimes-PointTime))
    while ((pitchValues[xstart] == 0 | is.na(pitchValues[xstart])) & (xstart > 0)){
      xstart <- xstart - 1
    }
    if (xstart == 0){
      startF0 <- NA
    } else {
      startF0 <- pitchValues[xstart]
    }
    
    #get the pitch from following measure
    xend <- which.min(abs(pitchTimes-PointTime)) + 1
    while ((pitchValues[xend] == 0 | is.na(pitchValues[xend])) & (xend > 0)){
      xend <- xend + 1
    }
    if (xend == 0){
      endF0 <- NA
    } else {
      endF0 <- pitchValues[xend]
    }
    
    # check to see if there are no measures preceding…
    if (is.na(startF0)){
      # …and if there are no following measures
      if(is.na(endF0)){
        # in this cases, there is no f0 anywhere in the file; return NA as the measured f0 and display an error
        resultF0 <- NA
        print(paste0('>> ALERT <<   (file: ', theName,')'))
        print('There was no f0 detected in the file')
      } else {
        # in this case there was no preceding measure, but there was a following one. return that measure, but display a message to the user
        resultF0 <- endF0 
        print(paste0('>> ALERT <<   (file: ', theName,')'))
        print(paste('There was no f0 detected in a 5 ms window centered on the Points label at time:', PointTime))
        print(paste('The f0 detected at', pitchTimes[xend], 'was used instead'))
        print('This may lead to inaccuracies in the analysis')
      }
    } else 
    # if we got here, there is a preceding measure, but now check if there is a following one
    if (is.na(endF0)){
      # in this case, there is no following measure, so return the preceding measure, but display an message to the user
      resultF0 <- startF0 
      print(paste0('>> ALERT <<   (file: ', theName,')'))
      print(paste('There was no f0 detected in a 5 ms window centered on the Points label at time:', PointTime))
      print(paste('The f0 detected at', pitchTimes[xstart], 'was used instead'))
      print('This may lead to inaccuracies in the analysis')
    } else{
      # the following happens in what is probably the normal case: there is a preceding measure and there is also a following one
      # in this case, return the average of those two
      # but also tell the user that the actual time doesn't have a measured f0
      resultF0 <- mean(startF0, endF0)
      print(paste0('>> ALERT <<   (file: ', theName,')'))
      print(paste('There was no f0 detected in a 5 ms window centered on the Points label at time:', PointTime))
      print(paste('The average of the f0s detected at', pitchTimes[xstart], 'and', pitchTimes[xend],'was used instead'))
      print('This may lead to inaccuracies in the analysis')
    }
  }
  
  return(resultF0)
}


# function:   intervalAtTime
# purpose:    extract information about an interval that occurs at a particular time
# arguments:  a point in Time (Time)
#             a single tier list, from a textgrid data object, from rPraat (TextGridTier)
# return:     a list, including the label, start time, end time, and position in the interval
intervalAtTime <- function(Time, TextGridTier){
  #NOTE: when Time points to a time that is precisely at the boundary between two intervals,
  #      the code above will return the information about the earlier interval
  
  #initialize all the variables as NA
  start <- NA
  end <- NA
  label <- NA
  pos <- NA
  
  # check if the Time is at the very end of one of the intervals (the $t2 vector contains all the end-times for each interval):
  if(Time %in% TextGridTier$"t2"){
    # if so, use "which(…)" to find which interval that it occurs at the end of:
    index <- which(Time==TextGridTier$t2)
    #get the label, start time, and end time of that interval:
    label <- TextGridTier$label[index]
    start <- TextGridTier$t1[index]
    end <- TextGridTier$t2[index]
    # save the position as "end"
    pos <- "end"
  }
  else # if it's not at the end of an interval…
    # …check to see if it's the beginning of the first interval:
    if(Time==TextGridTier$"t1"[1]){
      # if so, then get the label, start time, and end time of the 1st interval:
      label <- TextGridTier$label[1]
      start <- TextGridTier$t1[1]
      end <- TextGridTier$t2[1]
      # save the position as "beginning"
      pos <- "beginning"
    }
  else {
    # so it's not at the end or beginning of any interval…
    # find out which interval has a start time (t1) that is closest to Time:
    index <- which.min(lapply(Time-TextGridTier$t1, FUN = function(x){x[x>=0]}))
    
    # check that such a Word exists (this conditional works because which() returns something without a length when nothing satisfies what it's looking for):
    if (length(index) > 0){
      # check that Time occurs before the end of the last interval:
      if (Time <= last(TextGridTier$t2)) {
        #get the label, start time, and end time of that interval:
        label <- TextGridTier$label[index]
        start <- TextGridTier$t1[index]
        end <- TextGridTier$t2[index]
        # save the position as "mid"
        pos <- "mid"
      }
    }
  }
  
  return(list(Label = label, Start = start, End = end, Position = pos)) 
}





#########################################################
#########################################################
##  FUNCTIONS FOR GATHERING POINTS/LEVELS/RANGES INFO  ##
#########################################################
#########################################################


# function:   pointslevelsrangesLooper
# purpose:    this looper function goes through an entire directory, and creates a data frame about all the .wav/.textgrid pairs in that directory
#             that data frame will contain information related to each of the points in that file, from PoLaR labels / other measures
#             that data frame is then saved as a .csv in the output directory
# arguments:  optional: outputDir (a subfolder within the folder specified by WorkingDirectoryFullPath)
# return:     
pointslevelsrangesLooper <- function(outputDir = "PoLaR-BEAR-output"){
  #create the directory specified by outputDir, if it doesn't already exist
  dir.create(FullPath(outputDir), showWarnings=FALSE)

  # set up various variables according to PoLaR defaults (as described in the guidelines)
  setPoLaRvars()
  
  # create a blank data.frame, to be filled in with data by analyzing the .TextGrid/.wav files
  PointsLevelsRanges <- data.frame()
  
  # a for loop, going through all the .TextGrid files in WorkingDirectoryFullPath
  for (tg_file in list.files(path=WorkingDirectoryFullPath, pattern=".+\\.TextGrid")){
    # first clear out any data that may be lingering in the global environment
    rmPraatFiles()
    
    # take the file name, drop the ".TextGrid" extension, and save it as a global variable, "theName"
    assign("theName", str_split(tg_file, "\\.TextGrid", 2, simplify = TRUE)[1,1], envir = .GlobalEnv)
    
    # gather all the Praat files associated with "theName", and store them as R Data, to be accessed in all these functions
    gatherPraatFiles(theName)
    
    # run buildPLRdf() and append its output (a dataframe) to the end of the PointsLevelsRanges dataframe housed in this function
    PointsLevelsRanges <- rbind(PointsLevelsRanges, buildPLRdf(theTg))
  }
  
  # save the data frame as a .csv
  write.table(PointsLevelsRanges, file = file.path(FullPath(outputDir), "PointsLevelsRanges.csv"), sep=",", append = FALSE, col.names=TRUE, row.names=FALSE)
  
  # save the PointsLevelsRanges dataframe housed in this function to the global environment, so it can be accessed after this function is complete
  assign("PointsLevelsRanges",PointsLevelsRanges, envir = .GlobalEnv)
  
  # inform the user that it is finished
  print("DONE!")
  print(paste("The results have been saved as", file.path(FullPath(outputDir), "PointsLevelsRanges.csv") ) )
  print("The results have ALSO been saved in the R environment as the data.frame \'PointsLevelsRanges\'")
  
  # clean up variables / data from the environment
  rmPoLaRvariables()
  rmPraatFiles()
}


# function:   buildPLRdf
# purpose:    creates a data.frame that contains information about the file, relativized to to each of the Points tier labels
# arguments:  a textgrid data object from rPraat (TextGrid)
# return:     the data.frame
buildPLRdf <- function(TextGrid){
  # pull the names of the Points and Levels tiers as they're used in the current textgrid
  # (the dataframe theTiers is the lookkup table for this info: rows are PoLaR offfical names and columns are info about those tiers)
  tgPoints <- theTiers["Points", "tier.name"]
  tgLevels <- theTiers["Levels", "tier.name"]
  
  # pull the index of the Phones and Words tiers as they're used in the current textgrid
  tgPhones <- theTiers["Phones", "tier.name"]
  tgWords <- theTiers["Words", "tier.name"]
  
  
  # create a data frame centered on the Points tier info in the TextGrid data create by rPraat:
  PLR <- as.data.frame(TextGrid[[tgPoints]]) %>% 
    # get rid of some extraneous columns, that come with the rPraat object:
    mutate(index = row_number(), .before = name) %>% 
    # create a column for the Filename
    mutate(Filename = theName, .before=index) %>%
    # create a column of levels labels for each point, pulling directly from the Levels tier:
    subset(select = -c(name,type)) %>%
    # create a column "index" for enumerating the Points:
    mutate(Levels.label = TextGrid[[tgLevels]]$label[index]) %>%
    # rename columns to be more legible:
    rename(Points.label = label, Time=t) %>%
    # create a column for the F0 values, as determined by the getF0Here() function, based on the Time and Points.label columns:
    mutate(F0.Hz = purrr::pmap_dbl(list(Time, Points.label), function(t, l) getF0Here(t, l, thePitch)), .after=Time) %>%
    # create a column for the Ranges label, based on the Time column:
    mutate(Ranges.label = map_chr(Time, getRangeAtTime)) %>% 
    # create columns for the Ranges min and Ranges max, based on the Ranges.label column:
    mutate(Ranges.min = map_int(Ranges.label, function(x) parseRanges(x)[1]), Ranges.max = map_int(Ranges.label, function(x) parseRanges(x)[2])) %>%
    # create blank columns for Phones info and Words info:
    mutate(Phone = NA, Phone.start = NA, Phone.end = NA, Word = NA, Word.start = NA, Word.end = NA)
  
  #start with Phones, by making sure there is a Phones tier:
  if (!is.na(tgPhones)){
    # isolate the Phones tier from the TextGrid data
    phonesTier <- theTg[[tgPhones]]
    
    # go through each row in the PLR data frame (i.e., each Points label):
    for (pt in 1:dim(PLR)[1]){
      # send the Time and the Phones tier to a function defined in this file: intervalAtTime()
      # save the list that the function returns as phoneInterval:
      phoneInterval <- intervalAtTime (PLR[pt,]$Time, phonesTier)
      
      if (phoneInterval$Position == "end"){
        # then we write that the Points label occurs at the "end of «X»" where X is the label of the relevant Words interval
        PLR[pt,]$Phone <- paste0('end of /', phoneInterval$Label, '/')
      } else {
        # in a case where the Points label is not at the same time as a Words boundary, write that the Points label occurs in the "«X»":
        PLR[pt,]$Phone <- paste0('/', phoneInterval$Label, '/')
      }
      
      #now write the the times of the beginning/end of the Words interval, using the tg.getIntervalEndTime() function from rPraat:
      PLR[pt,]$Phone.start <- phoneInterval$Start
      PLR[pt,]$Phone.end <- phoneInterval$End
    }
  }
  
  #now onto the Words, again making sure there is a Words tier:
  if (!is.na(tgWords)){
    # isolate the Phones tier from the TextGrid data
    wordsTier <- theTg[[tgWords]]
    
    # go through each row in the PLR data frame (i.e., each Points label):
    for (pt in 1:dim(PLR)[1]){
      # send the Time and the Words tier to a function defined in this file: intervalAtTime()
      # save the list that the function returns as wordInterval:
      wordInterval <- intervalAtTime (PLR[pt,]$Time, wordsTier)
      
      if (wordInterval$Position == "end"){
        # then we write that the Points label occurs at the "end of «X»" where X is the label of the relevant Words interval
        PLR[pt,]$Word <- paste0('end of «', wordInterval$Label, '»')
      } else {
        # in a case where the Points label is not at the same time as a Words boundary, write that the Points label occurs in the "«X»":
        PLR[pt,]$Word <- paste0('«', wordInterval$Label, '»')
      }
      
      #now write the the times of the beginning/end of the Words interval, using the tg.getIntervalEndTime() function from rPraat:
      PLR[pt,]$Word.start <- wordInterval$Start
      PLR[pt,]$Word.end <- wordInterval$End
    }
  }
  return(PLR)
}


# function:   getRangeAtTime
# purpose:    return the label of the Ranges tier interval that contains a particular time
# arguments:  the particular time in question (Time)
#             (optional) textgrid data from rPraat (TextGrid)
# return:     a character string matching the label (if one exists; otherwise, it returns "--undefined--")
getRangeAtTime <- function(Time, TextGrid){
  # if no TextGrid argument is passed, use the variable theTg (which should be set as a global variable before this is callled)
  if(missing(TextGrid)) {
    if (exists("theTg", envir = .GlobalEnv)){
      TextGrid <- theTg
    }
    else {
      print('>> ALERT <<')
      print("You must either pass a value to the TextGrid variable of the function, OR")
      print("already have loaded a TextGrid into the global environment as theTg")
      stop("See alert(s) above!")
    }
  }
  # this variable will store the label of the interval that contains the Time; initialize it with "--undefined--"
  theRange <- "--undefined--"
  
  # check to make sure there is a Ranges tier in the 
  if (!is.na(theTiers["Ranges", "tier.num"])){
    # extract the Ranges tier list, as embedded in the textgrid data created by rProat:
    rangesTier <- theTg[[theTiers["Ranges", "tier.name"]]]
    
    # send the Time and the Ranges tier to a function defined in this file: intervalAtTime()
    # save the list that the function returns as rangesInterval:
    rangesInterval <- intervalAtTime(Time, rangesTier)
    
    # gwt the label of the Ranges interval
    theRange <- rangesInterval$Label
  }
  
  return(theRange)
}


# function:   getPhoneAtTime
# purpose:    return the label of the Phones tier interval that contains a particular time
# arguments:  the particular time in question (Time)
#             (optional) textgrid data from rPraat (TextGrid)
# return:     a character string matching the label (if one exists; otherwise, it returns "--undefined--")
getPhoneAtTime <- function(Time, TextGrid){
  # if no TextGrid argument is passed, use the variable theTg (which should be set as a global variable before this is callled)
  if(missing(TextGrid)) {
    if (exists("theTg", envir = .GlobalEnv)){
      TextGrid <- theTg
    }
    else {
      print('>> ALERT <<')
      print("You must either pass a value to the TextGrid variable of the function, OR")
      print("already have loaded a TextGrid into the global environment as theTg")
      stop("See alert(s) above!")
    }
  }
  # this variable will store the label of the interval that contains the Time; initialize it with "--undefined--"
  thePhone <- "--undefined--"
  
  # check to make sure there is a Phones tier in the 
  if (!is.na(theTiers["Phones", "tier.num"])){
    # extract the Phones tier list, as embedded in the textgrid data created by rProat:
    phonesTier <- theTg[[theTiers["Phones", "tier.name"]]]
    
    # send the Time and the Phones tier to a function defined in this file: intervalAtTime()
    # save the list that the function returns as phonesInterval:
    phonesInterval <- intervalAtTime(Time, phonesTier)
    
    # get the label of the Phones interval
    if (phonesInterval$Position == "end"){
      # then we write that the Points label occurs at the "end of /X/" where X is the label of the relevant Phones interval
      thePhone <- paste0('end of /', phonesInterval$Label, '/')
    } else {
      # in a case where the Points label is not at the same time as a Phones boundary, write that the Points label occurs in the "/X/":
      thePhone <- paste0('/', phonesInterval$Label, '/')
    }
  }
  
  return(thePhone)
}


# function:   getWordAtTime
# purpose:    return the label of the Words tier interval that contains a particular time
# arguments:  the particular time in question (Time)
#             (optional) textgrid data from rPraat (TextGrid)
# return:     a character string matching the label (if one exists; otherwise, it returns "--undefined--")
getWordAtTime <- function(Time, TextGrid){
  # if no TextGrid argument is passed, use the variable theTg (which should be set as a global variable before this is callled)
  if(missing(TextGrid)) {
    if (exists("theTg", envir = .GlobalEnv)){
      TextGrid <- theTg
    }
    else {
      print('>> ALERT <<')
      print("You must either pass a value to the TextGrid variable of the function, OR")
      print("already have loaded a TextGrid into the global environment as theTg")
      stop("See alert(s) above!")
    }
  }
  # this variable will store the label of the interval that contains the Time; initialize it with "--undefined--"
  theWord <- "--undefined--"
  
  # check to make sure there is a Words tier in the 
  if (!is.na(theTiers["Words", "tier.num"])){
    # extract the Words tier list, as embedded in the textgrid data created by rProat:
    wordsTier <- theTg[[theTiers["Words", "tier.name"]]]
    
    # send the Time and the Words tier to a function defined in this file: intervalAtTime()
    # save the list that the function returns as wordsInterval:
    wordsInterval <- intervalAtTime(Time, wordsTier)
    
    # get the label of the Words interval
    if (wordsInterval$Position == "end"){
      # then we write that the Points label occurs at the "end of «X»" where X is the label of the relevant Words interval
      theWord <- paste0('end of «', wordsInterval$Label, '»')
    } else {
      # in a case where the Points label is not at the same time as a Words boundary, write that the Points label occurs in the "«X»":
      theWord <- paste0('«', wordsInterval$Label, '»')
    }
  }
  
  return(theWord)
}