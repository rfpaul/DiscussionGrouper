## DiscussionGrouper
# This script takes in a file of students in a discussion section, and outputs a file
# containing groups optimized for peer learning. Optimized groups are determined by
# the Non-dominated Sorting Genetic Algorithm (NSGA-II) (see Deb, et al. 2002).

# 2017-2018 Robert Paul and Elise Nishikawa
# Corresponding author: robert.f.paul [[at]] gmail [[dot]] com
# Licensed under GPL v3, see https://www.gnu.org/licenses/gpl.html

# Input data formatting (see examples in test_data):
# ID	  Score  Gender Ethnicity Leader
# A, A     35      1          0     1
# B, Q     30      1          4     0
# C, F     29      1          0     0
# D, V     26      1          4     0
# F, C     29      0          3     0
# ...
# CSV file
# Last name/NetID/hash value or other unique identifier as first column
# Gender coded as: male=0, female or other=1

# Ethnic background coded as an integer,
# 0=White, 1=Asian, 2=International, 3=Hispanic, 4=African American, 5=Other
# Raw math/test scores are converted to: middle 80% = 0, bottom or top 10% = 1
# Personality coded as binary, Leader = 1, other = 0

# Optimization criteria (roughly by order of decreasing importance):
# Group composition:
# Formation of 5 groups with 3 or 4 members
# In smaller classes, 4 groups with 3 or 4 members
# Per group:
# Sum of genders is 0 or equal to group size
# Sum of math score categories is 0 or 1
# If a minority is present in a group, at least 2 of the same minority are in the group
# Also, there is a partial fitness benefit for maximally diverse groups
# Sum of personality types is 0 or 1

# For the generated sets of groups included in the Hall of Fame:
# Maximize uniqueness between grouping solutions; this is currently done in a fairly "weak" 
# way that only removes duplicate solutions

## Package imports
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

## Function definitions

# Encode scores based on percentile rank
CatScores <-function(scores)
{
  # Initialize everyone to category 0 (middle 80%)
  score_cats = rep(0, length(scores))
  # Get the top and bottom 10% scores
  cutoffs <- quantile(scores, c(0.1, 0.9))
  # Everyone below the 10th and above the 90th percentiles is coded to 1
  # Everyone else (middle 80%) is coded to 0
  # 10th percentile or below cutoff
  score_cats[which(scores <= cutoffs[[1]])] <- 1
  # 90th percentile or above cutoff
  score_cats[which(scores >= cutoffs[[2]])] <- 1
  
  # Return vector of categorized scores
  return(score_cats)
}

# Attach a GroupID column to the class data frame
AttachCol <- function(indiv){
  df <- classData
  df$GroupID <- indiv
  return(df)
}

# Given the data frame and column, return a list of values within the column
# and split by group
SplitExtracted <- function(df, c)
{
  result <- df[c]
  result <- split(result, df$GroupID)
  return(result)
}

# Evaluate fitness for size and number of groups
EvalGroupSize <- function(critVals) {
  # Check if all the groups have 3 or 4 members and we have the right
  # number of groups
  sizes <- lapply(critVals, nrow)
  sizesOK <- prod(sizes == 3 | sizes == 4)
  if(sizesOK) {
    # Number of groups and sizes ok, fitness +1
    return(1)
  } else { # Fitness penalty when groups improperly sized or wrong number of groups
    return(0)
  }
}

# Evaluate "fitness" for gender compositions
EvalGender <- function(critVals)
{
  # Sum the gender composition of each group
  sumVec <- as.numeric(lapply(critVals, sum))
  # Get the size of each group
  sizeVec <- as.numeric(lapply(critVals, nrow))
  # +0.1 to fitness per group with a sum of 0 or sum equal to group size
  # But first we need to redefine the values in the vector
  sumVec[sumVec == 0 | sumVec == sizeVec] <- .1
  sumVec[sumVec > .1] <- 0
  fitnessVal <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (length(unique(sumVec)) == 1 & fitnessVal != 0) fitnessVal <- 1
  # return fitness value
  return(fitnessVal)
}

# Evaluate "fitness" for math percentile distributions AND evaluate "fitness" 
# for only including at most one "leader" personality
EvalML <- function(critVals)
{
  # Sum the score or leader category and cast the list as a vector
  critVals <- as.numeric(lapply(critVals, sum))
  # +0.1 to fitness per group with a sum of 1 or 0
  # But first we need to redefine the values in the vector
  critVals[critVals <= 1] <- .1
  critVals[critVals > .1] <- 0
  fitnessVal <- sum(critVals)
  # If all groups meet criteria, fitness set to +1
  if (fitnessVal == .1 * length(critVals)) fitnessVal = 1
  # return fitness value
  return(fitnessVal)
}

# Evaluate "fitness" for diversity
EvalDiversity <- function(critVals) {
  # If groups are homogeneous, return +1 fitness
  # +0.1 to fitness per group where a minority isn't the only one of their
  # ethnicity in the group
  sumVec <- lapply(critVals, function(x) duplicated(x[x > 0]))
  sumVec <- as.numeric(lapply(sumVec, sum)) * 0.1
  # If all groups meet preceding criteria, return +1 fitness
  if (length(unique(sumVec)) == 1 & sumVec[1] != 0)
  {
    return(1)
  } else {
    fitnessVal <- sum(sumVec)
    # Get the size of each group
    sizeVec <- as.numeric(lapply(critVals, nrow))
    # Get number of ethnicities in each group
    sumVec <- as.numeric(lapply(critVals, function(x) nrow(unique(x))))
    # +0.1 to fitness per group where everyone is a different ethnicity
    fitnessVal <- fitnessVal + (sum(sumVec[sumVec == sizeVec]) * 0.1)
    # Improperly sized groups can push fitness above 1.0; return truncated value
    return(floor(fitnessVal))
  }
}

# All the fitness functions wrapped up to return a vector of fitness values
# **RFP: I know I probably shouldn't use all these globals in here but I didn't want to pass
# a ton of parameters into this and potentially muck things up badly
MetaFitness <- function(indiv)
{
  currGroup <- AttachCol(indiv)
  result <- c(EvalGroupSize(SplitExtracted(currGroup, "ID")),
              EvalGender(SplitExtracted(currGroup, "Gender")),
              EvalML(SplitExtracted(currGroup, "Score_Cat")),
              EvalDiversity(SplitExtracted(currGroup, "Ethnicity")),
              EvalML(SplitExtracted(currGroup, "Leader"))
              )
  # Return the weighted results
  return(result * weighting)
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

# Save the unique values that exceed a critical value in a Hall of Fame
# The critical value is the sum of weighting values * scaling factor
UpdateHoF <- function(hof, pop, fit, scaling) {
  # Get the indices where the sum of fitnesses is the sum of weighting, i.e.
  # maximum optimization value
  optIndices <- which(colSums(fit) >= sum(weighting) * scaling)
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

## Main script run

# Read input
# Ask for file
inputPath <- tclvalue(tkgetOpenFile(filetypes = "{ {CSV Files} {.csv} }"))
# Open the file
classData  <- read.csv(inputPath, header=TRUE)

# Process input
nStudents <- nrow(classData)
# If we have at least 15 students...
if (nStudents >= 15)
{
  # Place them into 5 groups
  nGroups <- 5
} else # But if we have 14 or fewer students...
{
  # Place them into 4 groups
  nGroups <- 4
} # **NOTE: It might be a good idea to handle classes of 11 or fewer students too
# Initial group arrangement; this is shuffled to produce the initial population
initArrang <- rep(1:nGroups, 5)[1:nStudents]

# Categorize the scores into a new column named Score_Cat
classData$Score_Cat <- CatScores(classData$Score)

# Set up and run genetic algorithm
MU = 200L # Number of individuals
LAMBDA = 100L # Number of offspring
MAX.ITER = 500L # Max number of generations
# Relative weighting of each parameter
weighting <-  c(20, # Correct number and sizes of groups
                10, # Homogeneous gender groups
                8, # At most 1 person outside the middle 80% of scores
                5, # Within group diversity
                1) # At most 1 leader
ref.point <- weighting # Ideal fitness values

# Toolbox initialization
# MetaFitness gives our vector of fitnesses, 5 objectives, maximize objectives
control <- initECRControl(MetaFitness, n.objectives = 5, minimize = FALSE)
# Initialize the mutation, survival, and reproduction operations
control <- registerECROperator(control, "mutate", mutScramble)
control <- registerECROperator(control, "recombine", recUnifCrossover)
control <- registerECROperator(control, "selectForMating", selSimple)
control <- registerECROperator(control, "selectForSurvival", selNondom)

# Initialize population of random arrangements
population <- initPopulation(mu = MU, gen.fun = gen, expr = sample(initArrang))
# And get the initial fitness
fitness <- evaluateFitness(control, population)

# Initialize logger
log <- initLogger(control,
                  log.stats = list(fitness = list("mean", "HV" = list(
                    fun = computeHV,
                    pars = list(ref.point = ref.point)))),
                  init.size = MAX.ITER)
updateLogger(log, population = population, fitness = fitness, n.evals = MU)

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
  
  # Update log, Pareto archive, and Hall of Fame
  updateLogger(log, population = population, fitness = fitness, n.evals = MU)
  updateParetoArchive(parchive, population, fitness)
  hof <- UpdateHoF(hof, population, fitness, 1)
  honMention <- UpdateHoF(hof, population, fitness, 0.85)
}

# Organize and format best, good, and Pareto-optimal solutions
# Initialize data frame for export
results <- data.frame(ID=1:nStudents)
results$ID <- classData$ID
# Add best, good, and Pareto solutions as columns
parInds <- getIndividuals(parchive)
results <- cbind(results, c(hof, honMention, parInds))

# Maximum length we'll need for labels in the output
lens <- c(length(hof), length(honMention), length(parInds))
maxLen <- max(lens)
# Build column name templates
repeats <- ceiling(maxLen / 26)
labels <- paste0(LETTERS, rep(1:repeats, each = 26))
# Rename columns
names(results) <- c('ID',
                    paste0("Best_", labels)[0:lens[1]],
                    paste0("Good_", labels)[0:lens[2]],
                    paste0("Pareto_", labels)[0:lens[3]])

# Output to file
# Format: CSV file, IDs with numeric group assignments over n columns of group arrangements
# ID   Best_A1   Best_B1   Good_A1   Good_B1 ... Arrangement_n
# A, A       1         1         2        4
# B, Q       1         5         5        3
# C, F       5         3         4        2
# D, V       2         1         4        1
# F, C       2         4         1        5
# ...
outputPath <- tclvalue(tkgetSaveFile(initialfile = "results.csv",
                                     filetypes = "{ {CSV Files} {.csv} }"))
write.csv(results,
          file=outputPath,
          row.names = FALSE)
