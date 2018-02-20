## DiscussionGrouper
#===============================================================================
# This script takes in a file of students in a discussion section, and outputs a file
# containing groups optimized for peer learning. Optimized groups are determined by
# the Non-dominated Sorting Genetic Algorithm (NSGA-II) (see Deb, et al. 2002).

# 2017-2018 Robert Paul and Elise Nishikawa
# Corresponding author: robert.f.paul [[at]] gmail [[dot]] com
# Licensed under GPL v3, see https://www.gnu.org/licenses/gpl.html

# Input data formatting (see examples in test_data):
# ID	  Score_Cat    Sex   Ethnicity  Personality
# A, A         -1      1          0             1
# B, Q          0      1          4             0
# C, F          0      1          0             0
# D, V          1      1          4             0
# F, C          1      0          3             0
# ...
# CSV file
# Last name/NetID/hash value or other unique identifier as first column
# Sex coded as: male=0, female or other=1

# Ethnic background coded as an integer,
# 0=White, 1=Asian, 2=Hispanic, 3=African American
# Raw math/test scores are categorized as: middle = 0, high = 1, low = -1
# Personality coded as binary, Leader = 1, other = 0

# Optimization criteria (roughly by order of decreasing importance):
# Group composition:
# All groups consist of 3 or 4 members
# Per group:
# Sum of sexes is 0 or at least group size - 2 (no more than 2 men in group)
# Score categories of -1 and 1 do not appear in the same group
# No minority ethnicity alone in a group, and ideally two minorities present.
# (If possible, match ethnicity. If not possible 2 individuals of any
# underrepresented ethnicity in group)
# Sum of personality types is 0 or 1 (maximum 1 leader)

# For the generated sets of groups included in the Hall of Fame:
# Maximize uniqueness between grouping solutions; this is currently done in a fairly "weak" 
# way that only removes duplicate solutions

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
# Deprecated
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
# All groups have 3 or 4 members
EvalGroupSize <- function(critVals) {
  # Check if all the groups have the appropriate number of members and we
  # have the right number of groups
  sizes <- lapply(critVals, nrow)
  sizesOK <- prod(sizes == maxGroupSize | sizes == maxGroupSize - 1)
  if(sizesOK) {
    # Number of groups and sizes ok, fitness +1
    return(1)
  } else { # Fitness penalty when groups improperly sized or wrong number of groups
    return(0)
  }
}

# Evaluate "fitness" for Sex compositions
# No more than 2 men
EvalSex <- function(critVals)
{
  # Sum the Sex composition of each group
  sumVec <- as.numeric(lapply(critVals, sum))
  # Get the size of each group
  sizeVec <- as.numeric(lapply(critVals, nrow))
  # +partialFit to fitness per group with a sum of 0 or sum at least 2
  # But first we need to redefine the values in the vector
  sumVec[sumVec == 0 | sumVec >= 2] <- partialFit
  sumVec[sumVec > partialFit] <- 0
  fitnessVal <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (length(unique(sumVec)) == 1 & fitnessVal != 0) fitnessVal <- 1
  # return fitness value
  return(fitnessVal)
}

# Evaluate "fitness" for score distributions
# Score categories of -1 and 1 do not appear in the same group
EvalScore <- function(critVals)
{
  # Do -1 and 1 appear together in a group?
  scoreVec <- as.numeric(
    lapply(critVals, function(x) any(x == 1) & any (x == -1)))
  # If all the groups are OK, +1 fitness
  if(sum(scoreVec) == 0) {
    return(1)
  } else { # +Partial fitness for each group meeting criteria
    fitnessVal <- sum(as.numeric(!scoreVec)) * partialFit
    return(fitnessVal)
  }
}

# Evaluate "fitness" for diversity
# No minority ethnicity alone in a group (if possible, match ethnicity.
# If not possible 2 individuals of any underrepresented ethnicity in group)
EvalDiversity <- function(critVals) {
  # List of vectors with non-minorities dropped
  ethOnly <- lapply(critVals, function(x) x[x > 0])
  # Number of minorities in each group
  numEth <- as.numeric(lapply(ethOnly, length))
  # Number of unique values in the ethOnly list
  numUniques <- as.numeric(lapply(ethOnly, function(x) length(unique(x))))
  # Ideally every group will have 2 minorities of the same ethnicity; these are
  # the indices of these fit values
  fitIndices <- which(numEth == 2 & numUniques == 1)
  # Every group is fit, +1 fitness value
  if (length(fitIndices) == nGroups) {
    return(1)
  } else {
    # Partial fitness for each group meeting criteria
    fitnessVal <- length(fitIndices) * partialFit
    # Secondary fit values; 2 minorities of different ethnicities
    # This criterion is worth 75% of the partial fitness for matching ethnicity
    fitIndices <- which(numEth == 2 & numUniques == 2)
    fitnessVal <- fitnessVal + (length(fitIndices) * partialFit  * 0.75)
    return(fitnessVal)
  }
}

# Evaluate "fitness" for only including at most one "leader" personality
EvalLeader <- function(critVals)
{
  # Sum the leader category and cast the list as a vector
  critVals <- as.numeric(lapply(critVals, sum))
  # +partialFit to fitness per group with a sum of 1 or 0
  # But first we need to redefine the values in the vector
  critVals[critVals <= 1] <- partialFit
  critVals[critVals > partialFit] <- 0
  fitnessVal <- sum(critVals)
  # If all groups meet criteria, fitness set to +1
  if (fitnessVal == partialFit * length(critVals)) fitnessVal = 1
  # return fitness value
  return(fitnessVal)
}

# All the fitness functions wrapped up to return a vector of fitness values
# **RFP: I know I probably shouldn't use all these globals in here but I didn't want to pass
# a ton of parameters into this and potentially muck things up badly
MetaFitness <- function(indiv)
{
  currGroup <- AttachCol(indiv)
  # Get a vector of all the fitness values
  result <- c(EvalGroupSize(SplitExtracted(currGroup, "ID")),
              EvalSex(SplitExtracted(currGroup, "Sex")),
              EvalScore(SplitExtracted(currGroup, "Score_Cat")),
              EvalDiversity(SplitExtracted(currGroup, "Ethnicity")),
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

# What is the maximum size groups should be?
maxGroupSize <- 4

# Genetic algorithm variable values
MU = 900L # Number of individuals #900
LAMBDA = 400L # Number of offspring #400
MAX.ITER = 1200L # Max number of generations # 1000
OBJS = 5L # Number of objectives

# Relative weighting of each objective
weighting <-  c(20, # Correct number and sizes of groups
                10, # No more than 2 men in the group
                8, # No high and low score individuals together
                5, # Within group diversity
                1) # At most 1 leader
ref.point <- weighting # Ideal fitness values

# Benchmark values between 0-1 used as the cutoff of Honorable Mention solutions
# i.e. fitness values greater than or equal to weighting * honMentionBench 
# is added to Honorable Mention solutions
honMentionBench <- c(1.0, # Correct number and sizes of groups
                     0.2, # No more than 2 men in the group
                     0.2, # No high and low score individuals together
                     0.2, # Within group diversity
                     0.1) # At most 1 leader)

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

# Loop through all available input folders in inputDir
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
  initArrang <- rep(1:nGroups, 5)[1:nStudents]
  
  # Categorize the scores into a new column named Score_Cat
  # classData$Score_Cat <- CatScores(classData$Score)
  
  # Set up and run genetic algorithm
  # What should be the additive value of partial (per group) fitness?
  partialFit <- .5 / nGroups
  
  # Toolbox initialization
  # MetaFitness gives our vector of fitnesses, 5 objectives, maximize objectives
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
  
  for (i in seq_len(MAX.ITER)) {
    # Generate offspring by recombination and mutation
    offspring <- recombinate(control = control, 
                             inds = population,
                             fitness = fitness, 
                             lambda = LAMBDA,
                             p.recomb = 0.8)
    offspring <- mutate(control,
                        offspring,
                        p.mut = 0.3)
    
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
    
    # Update log, Pareto archive, Hall of Fame, and honorable mention
    updateLogger(log, population = population, fitness = fitness, n.evals = MU)
    updateParetoArchive(parchive, population, fitness)
    hof <- UpdateHoF(hof, population, fitness, rep(1, OBJS))
    honMention <- UpdateHoF(hof, population, fitness, honMentionBench)
    
    # Print progress into console
    if(i %% 10 == 0) {
      print(paste("Generation number", i, "of", MAX.ITER, "complete"))
    }
  }
  
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
                      paste0("Pareto_Grouping_", labels)[0:lens[3]])
  
  # Output to file
  # Format: CSV file, IDs with numeric group assignments over n columns of group arrangements
  # ID   Best_A1   Best_B1   Good_A1   Good_B1 ... Arrangement_n
  # A, A       1         1         2        4
  # B, Q       1         5         5        3
  # C, F       5         3         4        2
  # D, V       2         1         4        1
  # F, C       2         4         1        5
  # ...
  # Generate output name and path
  outputPath <- paste(outputDir,
                      paste0(substr(classFiles[j], 0, nchar(classFiles[j]) - 4),
                             "-results.csv"),
                      sep = '/')
  # # Ask for output file name
  # outputPath <- tclvalue(tkgetSaveFile(initialfile = "results.csv",
  #                                      filetypes = "{ {CSV Files} {.csv} }"))
  # Write output
  write.csv(results,
            file=outputPath,
            row.names = FALSE)
  print(paste("Completed", classFiles[j]))
}
