## DiscussionGrouper
#===============================================================================
# This script takes in a file of students in a discussion section, and outputs a file
# containing groups optimized for peer learning. Optimized groups are determined by
# the Non-dominated Sorting Genetic Algorithm (NSGA-II) (see Deb, et al. 2002).

# 2017-2019 Robert Paul and Elise Nishikawa
# Corresponding author: robert.f.paul [[at]] gmail [[dot]] com
# Licensed under GPL v3, see https://www.gnu.org/licenses/gpl.html

# Input data formatting (see examples in test_data):
# ID	  Score_Cat   Sex  Ethnicity  First_Gen  EOP_Status  Personality
# A, A         -1     1          0          0           1            1
# B, Q          0     1          4          0           0            0
# C, F          0     1          0          1           1            0
# D, V          1     1          4          1           0            0
# F, C          1     0          3          0           0            0
# ...
# File type: CSV file
# Last,First/NetID/hash value or other unique identifier as ID column
# Sex coded as: male=0, female or other=1
# Ethnic background coded as an integer,
# 0=White, 1=Asian, 2=Latinx, 3=African American, 4=Native American,
# 5 = Pacific Islander
# First generation student coded as a binary, 1 = yes, 0 = no
# EOP status coded as a binary, 1 = yes, 0 = no
# Raw math/test scores are categorized as: middle = 0, high = 1, low = -1
# Personality coded as binary, Leader = 1, other = 0

# IMPORTANT! AL1 and AL2/AL3 sections use slightly different parameters and
# function definitions. Find these areas in the code where the asterisk *
# character is repeated 5 times in a row

# !!!!!
# Optimization criteria (roughly by order of decreasing importance):
# Group composition:
# All groups are the maximum group size or one less than the max
# Per group:
# Sum of sexes is 0 or at least group size - 2 (no more than 2 men in group)
# No minority ethnicity alone in a group, and ideally at least two minorities
# present. (If possible, match ethnicity. If not possible 2 individuals of any
# underrepresented ethnicity in group)
# No first generation college students alone in a group
# No EOP students alone in a group
# For AL1: Score categories of -1 and 1 do not appear in the same group
# For Merit: No more than 2 score categories of 1 or -1 in the same group
# Sum of personality types is 0 or 1 (maximum 1 leader)

# If you're adding or removing objectives, search the code for where the
# exclamation point character, !, is repeated five times to indicate the
# critical parts of the code that need to be changed.

# For the generated sets of groups included in the Hall of Fame:
# Maximize uniqueness between grouping solutions; this is currently done in a
# fairly "weak" way that only removes duplicate solutions, and isn't a criteria
# fed into the optimization algorithm.

# Some notes on internals and data processing...

# The core data structure used for computation is the R list. The ecr package
# stores fitness values as matrices, where individuals are the columns and the 
# rows are the objectives. Temporary data storage and access uses data frames.
# Functions are vectorized whenever possible because this stuff is
# computationally intensive and any optimization really pays off. If you want
# to start rewriting or adding things, it's EXTREMELY important that you
# familiarize yourself with the syntax of constructing and manipulating lists
# and especially the lapply function.

# I've set up pretty much everything as global variables in scope. I know that's
# bad practice and can quickly devolve into spaghetti code, but it also greatly
# reduces the number of parameters I have to pass into functions, makes adding/
# dropping criteria fairly painless, and makes debugging just slightly easier.
# It's totally cool if you want to refactor the code to reduce the number of
# globals, I simply picked this side of the trade-offs to speed up
# development time. --RFP

# Also I severely exploit the automatic casting of boolean values (TRUE and
# FALSE) into integer values of 1 and 0 in many vectorized operations. --RFP

## Package imports
#===============================================================================
library(tcltk)
# Require these packages--install and load if they're not available
if (!require(ecr)) {
  install.packages("ecr")
  library(ecr)
}

if (!require(foreach)) {
  install.packages("foreach")
  library(foreach)
}

# if (!require(doParallel)) {
#   install.packages("doParallel")
#   library(doParallel)
# }

## Function definitions
#===============================================================================

# Encode scores based on percentile rank
# Deprecated, these data are pre-processed
# CatScores <-function(scores)
# {
#   # Initialize everyone to category 0 (middle 80%)
#   score_cats = rep(0, length(scores))
#   # Get the top and bottom 10% scores
#   cutoffs <- quantile(scores, c(0.1, 0.9))
#   # Everyone below the 10th and above the 90th percentiles is coded to 1
#   # Everyone else (middle 80%) is coded to 0
#   # 10th percentile or below cutoff
#   score_cats[which(scores <= cutoffs[[1]])] <- -1
#   # 90th percentile or above cutoff
#   score_cats[which(scores >= cutoffs[[2]])] <- 1
# 
#   # Return vector of categorized scores
#   return(score_cats)
# }

# Attach a GroupID column to the class data frame
AttachCol <- function(indiv){
  df <- classData
  df$GroupID <- indiv
  return(df)
}

# Given the data frame and column, return a list of dataframes with only the
# specified column and split by group
SplitExtracted <- function(df, c)
{
  result <- df[c]
  result <- split(result, df$GroupID)
  return(result)
}

# Evaluate fitness for size and number of groups
# All groups have 3-4 members (AL1) or 5-6 members (AL2/AL3)
EvalGroupSize <- function(critVals) {
  # Check if all the groups have the appropriate number of members and we
  # have the right number of groups
  # Get the size of each group
  sizeVec <- as.numeric(lapply(critVals, nrow))
  sizesOK <- prod(sizeVec == maxGroupSize | sizeVec == maxGroupSize - 1)
  if(sizesOK) {
    # Number of groups and sizes ok, fitness +1
    return(1)
  } else { # Fitness penalty when groups improperly sized or wrong number of groups
    return(0)
  }
}

# Evaluate fitness for Sex compositions
# Coded as Male = 0, Female = 1
# No more than 2 men (in general) or 1 male in a group of 3
EvalSex <- function(critVals)
{
  # Sum the Sex composition of each group
  sumVec <- as.numeric(lapply(critVals, sum))
  # Get the size of each group
  sizeVec <- as.numeric(lapply(critVals, nrow))
  # +partialFit to fitness per group with a sum of 0 or sum at least
  # group size - 2
  # But first we need to redefine the values in the vector
  sizeVec[sizeVec == 3] <- 4 # For 3-person groups, add a nonexistant member to
  # prevent a 2-male 1-female group from being a valid solution
  sumVec[sumVec == 0 | sumVec >= (sizeVec - 2)] <- partialFit
  sumVec[sumVec != partialFit] <- 0
  fitnessVal <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (fitnessVal == partialFit * length(critVals)) fitnessVal <- 1
  # return fitness value
  return(fitnessVal)
}

# ***** IMPORTANT! Comment out and uncomment the appropriate EvalScore function 
# definitions for AL1 vs AL2/AL3 section runs

# Evaluate "fitness" for score distributions (AL1)
# Score categories of -1 and 1 do not appear in the same group
EvalScore <- function(critVals)
{
  # Do -1 and 1 appear together in a group?
  scoreVec <- as.numeric(
    lapply(critVals, function(x) any(x == 1) & any(x == -1)))
  # If all the groups are OK, +1 fitness
  if(sum(scoreVec) == 0) {
    return(1)
  } else { # +Partial fitness for each group meeting criteria
    fitnessVal <- sum(as.numeric(!scoreVec)) * partialFit
    return(fitnessVal)
  }
}

# Evaluate "fitness" for score distributions (AL2/AL3)
# Score categories of -1 or 1 do not appear more than twice in the same group
# EvalScore <- function(critVals)
# {
#   # How often do -1 and 1 appear together in a group?
#   scoreVec <- as.numeric(lapply(critVals, function(x) sum(abs(x))))
#   # If all the groups are OK, +1 fitness
#   if(sum(unique(scoreVec)) <= 1) {
#     return(1)
#   } else { # +Partial fitness for each group meeting criteria
#     fitnessVal <- sum(scoreVec <= 1) * partialFit
#     return(fitnessVal)
#   }
# }

# Evaluate "fitness" for diversity
# No minority ethnicity alone in a group (if possible, match ethnicity.
# If not possible at least 2 individuals of any underrepresented ethnicity in group)
EvalDiversity <- function(critVals) {
  # Get the size of each group
  sizeVec <- as.numeric(lapply(critVals, nrow))
  # List of vectors with non-minorities dropped
  ethOnly <- lapply(critVals, function(x) x[x > 0])
  # Number of minorities in each group
  numEth <- as.numeric(lapply(ethOnly, length))
  # Number of unique values in the ethOnly list
  numUniques <- as.numeric(lapply(ethOnly, function(x) length(unique(x))))
  # Ideally every group will have at least 2 minorities of the same ethnicity
  # but doesn't exclusively contain one underrepresented group; these are the
  # indices of these fit values
  fitIndices <- which(numEth >= 2 & numEth < sizeVec & numUniques == 1)
  # Every group is fit, +1 fitness value
  if (length(fitIndices) == nGroups) {
    return(1)
  } else {
    # Partial fitness for each group meeting criteria
    fitnessVal <- length(fitIndices) * partialFit
    # Secondary fit values; at least 2 minorities of different ethnicities
    # This criterion is worth 90% of the partial fitness for matching ethnicity
    fitIndices <- which(numEth >= 2 & numEth < sizeVec & numUniques >= 2)
    fitnessVal <- fitnessVal + (length(fitIndices) * partialFit  * 0.90)
    return(fitnessVal)
  }
}

# Evaluate "fitness" that the groups have either no first generation students
# or at least least two; same criteria for EOP status
EvalFirstGensEOP <- function(critVals)
{
  # Sum the first generation student or EOP category and cast the list as
  # a vector
  sumVec <- as.numeric(lapply(critVals, sum))
  # +partialFit to fitness per group with a sum of 0 or greater than 1
  # But first we need to redefine the values in the vector
  sumVec[sumVec == 0 | sumVec > 1] <- partialFit
  sumVec[sumVec != partialFit] <- 0
  fitnessVal <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (fitnessVal == partialFit * length(critVals)) fitnessVal <- 1
  # return fitness value
  return(fitnessVal)
}

# Evaluate "fitness" for only including at most one "leader" personality
EvalLeader <- function(critVals)
{
  # Sum the leader category and cast the list as a vector
  sumVec <- as.numeric(lapply(critVals, sum))
  # +partialFit to fitness per group with a sum of 1 or 0
  # But first we need to redefine the values in the vector
  sumVec[sumVec <= 1] <- partialFit
  sumVec[sumVec > partialFit] <- 0
  fitnessVal <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (fitnessVal == partialFit * length(critVals)) fitnessVal <- 1
  # return fitness value
  return(fitnessVal)
}

# All the fitness functions wrapped up to return a vector of fitness values
MetaFitness <- function(indiv)
{
  currGroup <- AttachCol(indiv)
  # Get a vector of all the fitness values
  # !!!!! 
  result <- c(EvalGroupSize(SplitExtracted(currGroup, "ID")),
              EvalSex(SplitExtracted(currGroup, "Sex")),
              EvalDiversity(SplitExtracted(currGroup, "Ethnicity")),
              EvalFirstGensEOP(SplitExtracted(currGroup, "First_Gen")),
              EvalFirstGensEOP(SplitExtracted(currGroup, "EOP_Status")),
              EvalScore(SplitExtracted(currGroup, "Score_Cat")),
              EvalLeader(SplitExtracted(currGroup, "Personality")))
  # Return the weighted results
  return(result * weighting)
}

# Save the unique values that meet or exceed a critical value into a Hall of Fame
# The critical value is the sum of weighting values * scaling factor
UpdateHoF <- function(hof, pop, fit, scaling) {
  # Get the indices where all the fitnesses are at least greater than or equal to
  # weighting * scale factors
  optIndices <- which(colSums(fit >= weighting * scaling) == length(weighting))
  if (length(optIndices) > 0) {
    # Get the unique individual solutions from the population
    candidates <- unique(pop[optIndices])
    
    # Make a list of candidate arrangements, with each group sorted by ID
    candArrangs <-VecsToLists(candidates) 
    
    # Make sure they're not duplicate solutions just with different group numbers;
    # if there are, update the candidates list and candArrangs
    arrangOK <- !duplicated(candArrangs)
    # At least one duplicated value found
    if (prod(arrangOK) == 0) {
      candidates <- candidates[arrangOK]
    }
    
    # Merge HoF with candidates
    hof <- c(hof, candidates)
    # Make sure they're not duplicate solutions to anything in HoF
    hof <- unique(hof)
    hofArrangs <- VecsToLists(hof)
    arrangOK <- !duplicated(hofArrangs)
    # At least one duplicated value found
    if (prod(arrangOK) == 0) {
      hof <- hof[arrangOK]
    }
  }
  return(hof)
}

printFitnessStats <- function(fitness) {
  cat("Ideal values:", sprintf('%.2f', weighting), '\n', sep = '\t')
  cat("Cutoff values:", sprintf('%.2f', weighting * honMentionBench), '\n', sep = '\t')
  cat("Fitness max:", sprintf('%.2f', apply(fitness, 1, max)), '\n', sep = '\t')
  cat("Fitness mean:", sprintf('%.2f', apply(fitness, 1, mean)), '\n', sep = '\t')
  cat("Fitness SD:", sprintf('%.2f', apply(fitness, 1, sd)), '\n', sep = '\t')
  cat("Fitness min:", sprintf('%.2f', apply(fitness, 1, min)), '\n\n', sep = '\t')
}

# Make a list of arrangements, with each group sorted by ID
VecsToLists <- function(vecList) {
  arrangList <- foreach(i = 1:length(vecList)) %do% {
    # Make a grouping with the current candidate arrangement
    curr <- AttachCol(vecList[[i]])
    curr <- SplitExtracted(curr, "ID")
    # Sort the dataframes of each group by ID
    curr <- lapply(curr, sortByCol, col = "ID")
    # Sort the arrangement by the first ID of the group
    curr <- curr[order(sapply(curr, function(x) x$ID[1]))]
    # Rename the arrangement list
    names(curr) <- 1:length(curr)
    curr
  }
  return(arrangList)
}

## Main script run
#===============================================================================
# Register parallelization of operations
# registerDoParallel(cores = 7)

# ***** IMPORTANT! Change the maxGroupSize parameter for AL1 vs Merit runs!
# What is the maximum size groups should be? (AL1)
maxGroupSize <- 4
# What is the maximum size groups should be? (Merit/AL2/AL3)
# maxGroupSize <- 6
# Alternative group size
# maxGroupSize <- 5

# Genetic algorithm variable values
# n.b., you can probably get away with a faster run with lower values, the
# default parameters are for a more exhaustive search
MU <- 600L # Number of individuals # default: 900
LAMBDA <- 300L # Number of offspring # default: 400
MAX.ITER <- 500L # Max number of generations # default: 1200
# !!!!!
OBJS <- 7L # Number of objectives
P.RECOMBINE <- 0.8 # Crossover probability, default = 0.8
P.MUTATION <- 0.6 # Mutate probability, default = 0.3

# Relative weighting of each objective
# !!!!!
weighting <-  c(20, # Correct number and sizes of groups
                10, # No more than 2 men in the group
                8, # Within group ethnic diversity
                8, # First generation students are not alone in a group
                8, # EOP students are not alone in a group
                5, # No high and low score individuals together
                1) # At most 1 leader
ref.point <- weighting # Ideal fitness values
# Text string tags of the objectives
# !!!!!
weightTags <- c("Group size:",
                "Gender:",
                "Diversity:",
                "First generation:",
                "EOP:",
                "Math scores:",
                "Personality:")

# Ask for input folder
inputDir <- tk_choose.dir(
  caption = "Open and select folder containing ONLY input files")
classFiles <- list.files(inputDir)
# This is a simple filter to include only csv files
classFiles <- classFiles[grep("*.csv", classFiles)]
# Ask for output folder
outputDir <- tk_choose.dir(
  caption = "Open and select folder for saving results")

# # Ask for file
# inputPath <- tclvalue(tkgetOpenFile(filetypes = "{ {CSV Files} {.csv} }"))
# # Open the file
# classData  <- read.csv(inputPath, header=TRUE)

# Loop through all available input files in inputDir
for (j in seq_len(length(classFiles))) {
  # Progress message
  print(paste("Processing", classFiles[j]))
  # Read input
  classData <- read.csv(paste(inputDir, classFiles[j], sep = '/'), header=TRUE)
  
  # Process input
  # Number of students in the data file
  nStudents <- nrow(classData)
  # Number of groups
  nGroups <- ceiling(nStudents/maxGroupSize)
  # Initial group arrangement of 1,2,3,4,5,1,2,3 ... etc.
  initArrang <- rep(1:nGroups, ceiling(nStudents/nGroups))[1:nStudents]
  
  # Categorize the scores into a new column named Score_Cat
  # classData$Score_Cat <- CatScores(classData$Score)
  
  # Set up and run genetic algorithm
  # What should be the additive value of partial (per group) fitness?
  partialFit <- 0.5 / nGroups
  
  # Toolbox initialization
  # MetaFitness gives our vector of fitnesses, 7 objectives, maximize objectives
  control <- initECRControl(MetaFitness, n.objectives = OBJS, minimize = FALSE)
  # Initialize the mutation, survival, and reproduction operations
  control <- registerECROperator(control, "mutate", mutScramble)
  control <- registerECROperator(control, "recombine", recUnifCrossover)
  control <- registerECROperator(control, "selectForMating", selSimple)
  control <- registerECROperator(control, "selectForSurvival", selNondom)
  
  # Initialize population of random arrangements
  population <- initPopulation(mu = MU,
                               gen.fun = gen,
                               expr = sample(initArrang))
  # And get the initial fitness
  fitness <- evaluateFitness(control, population)
  
  # Initialize logger
  log <- initLogger(control,
                    log.stats = list(fitness = list(
                      "min",
                      "mean",
                      "max",
                      "HV" = list(fun = computeHV,
                                  pars = list(ref.point = ref.point))
                    )),
                    init.size = MAX.ITER + 1)
  updateLogger(log,
               population = population,
               fitness = fitness,
               n.evals = MU)
  
  # Initialize Pareto archive
  parchive <- initParetoArchive(control)
  updateParetoArchive(parchive, population, fitness)
  
  # Initialize Hall of Fame and Honorable Mention as empty lists
  hof <- list()
  honMention <- list()
  
  # Benchmark values between 0-1 used as the cutoff of Honorable Mention solutions
  # i.e. fitness values greater than or equal to weighting * honMentionBench 
  # are added to Honorable Mention solutions
  # N.B., in current implementation, these values are overwritten halfway
  # through the run (at generation MAX.ITER / 2)
  # !!!!!
  honMentionBench <- c(1.0, # Correct number and sizes of groups
                       0.4, # No more than 2 men in the group
                       0.4, # Within group diversity
                       0.4, # First generation matching
                       0.4, # EOP matching
                       0.4, # No high and low score individuals together
                       0.4) # At most 1 leader)
  
  for (i in seq_len(MAX.ITER)) {
    # Generate offspring by recombination and mutation
    offspring <- recombinate(control = control, 
                             inds = population,
                             fitness = fitness, 
                             lambda = LAMBDA,
                             p.recomb = P.RECOMBINE)
    offspring <- mutate(control,
                        offspring,
                        p.mut = P.MUTATION)
    
    # Calculate costs of new arrangment population
    fitness.o <- evaluateFitness(control, offspring)
    
    # Apply selection
    sel <- replaceMuPlusLambda(control = control,
                               population = population,
                               offspring = offspring,
                               fitness = fitness,
                               fitness.offspring = fitness.o)
    population <- sel$population
    fitness <- sel$fitness
    
    propDone = i / MAX.ITER
    
    # Update log, Pareto archive, Hall of Fame, and honorable mention
    updateLogger(log, population = population, fitness = fitness, n.evals = MU)
    updateParetoArchive(parchive, population, fitness)
    hof <- UpdateHoF(hof, population, fitness, rep(1, OBJS))
    
    # Add honorable mention based on the manually set honorable mention
    # benchmark values
    # honMention <- UpdateHoF(hof, population, fitness, honMentionBench)
    
    # Get the honorable mention benchmark based on mean population values once
    # we're halfway through the iterations
    # !!!!!
    if (i == ceiling(MAX.ITER/2)) {
      honMentionBench <- rowMeans(fitness) / weighting
      honMentionBench[1] <- 1.0 # Keep the first criterion (class size) as
      # a requirement
    }

    # Update honorable mention after we're halfway through iterating
    if (propDone > 0.5) {
      honMention <- UpdateHoF(hof, population, fitness, honMentionBench)
    }
    
    # Print progress into console
    if(i %% 50 == 0) {
      print(paste("Generation number", i, "of", MAX.ITER, "complete"))
      printFitnessStats(fitness)
    }
  }
  
  print("Iterations complete. Preparing output...")
  
  # Organize and format best, good, and Pareto-optimal solutions
  # Initialize data frame for export
  results <- data.frame(ID=1:nStudents)
  results$ID <- classData$ID
  # Only use the Pareto front solutions that have the correct group sizes
  sizePareto <- getFront(parchive)[1,] == weighting[1]
  # Add best, good, and Pareto solutions as columns
  parInds <- getIndividuals(parchive)[sizePareto]
  results <- cbind(results, c(hof, honMention, parInds))
  
  # Maximum length we'll need for column labels in the output
  lens <- c(length(hof), length(honMention), length(parInds))
  maxLen <- max(lens)
  # Build column name templates
  repeats <- ceiling(maxLen / 26)
  labels <- paste0(LETTERS, rep(1:repeats, each = 26))
  # Rename columns
  names(results) <- c('ID',
                      paste0("Best_Grouping_", labels)[0:lens[1]],
                      paste0("Good_Grouping_", labels)[0:lens[2]],
                      paste0("Trade-off_Grouping_", labels)[0:lens[3]])
  
  # Now we have a results data frame--
  # results dataframe format: IDs with numeric group assignments over n columns
  # of group arrangements
  # ID   Best_A1   Best_B1   Good_A1   Good_B1 ... Arrangement_n
  # A, A       1         1         2        4
  # B, Q       1         5         5        3
  # C, F       5         3         4        2
  # D, V       2         1         4        1
  # F, C       2         4         1        5
  # ...
  
  # Output to file
  # Output file format: Text file with group arrangements, fitness values, and
  # IDs listed for each group
  # Best_A1
  # Fitness_criteron_1: value ... Fitness_criterion_n: value
  # 1: B,Q; L,A; D,S; F,M; A,A
  # 2: D,V; F,C; W,L; O,M
  # ...
  # Arrangement_n
  # Fitness_criteron_1: value ... Fitness_criterion_n: value
  # ...
  
  # Generate output name and path
  outputPath <- paste(outputDir,
                      paste0(substr(classFiles[j], 1, nchar(classFiles[j]) - 4),
                             "-results.txt"),
                      sep = '/')
  
  # Build up the lines of output in the text file
  linesOut <- foreach(k = 2:ncol(results), .combine = c) %do% {
    # Get the current column's name
    columnResult <- colnames(results)[k]
    
    # Get the fitness values for the current column
    colFitness <- paste(weightTags, MetaFitness(results[,k]), collapse = '; ')
    
    # Get the IDs for every group
    resultLines <- foreach(l = 1:max(results[k]), .combine = c) %do% {
      # Get the IDs for the current group
      currLine <- results[1][results[k] == l]
      # Format the vector IDs into a single string
      currLine <- paste(currLine, sep = '', collapse = "; ")
      # Paste the group number in front of the IDs
      currLine <- paste(l, currLine, sep = ": ")
      currLine
    }
    columnResult <- c(columnResult, colFitness, resultLines)
    # Add newline to the end of the column's results
    columnResult[length(columnResult)] <- paste0(columnResult[length(columnResult)], '\n')
    columnResult
  }
  
  # Add a header describing ideal fitness values
  linesOut <- c("\tMaximum fitness values and relative weighting",
                paste0('\t', paste(weightTags, weighting, collapse = '; ')),
                '',
                linesOut)
  
  # Write output
  fileConxn <- file(outputPath)
  writeLines(linesOut, fileConxn)
  close(fileConxn)
  
  print(paste("Completed", classFiles[j]))
}
