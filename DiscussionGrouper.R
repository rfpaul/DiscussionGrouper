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

# Group composition:
# Formation of 5 groups with 3 or 4 members
# In smaller classes, 4 groups with 3 or 4 members

# Optimization criteria (roughly by order of decreasing importance):
# Per group:
# Sum of genders is 0, 3, or 4
# Sum of math score categories is 0 or 1
# If a minority is present in a group, at least 2 of the same minority are in the group
# Sum of personality types is 0 or 1

# For the generated sets of groups included in the Hall of Fame:
# Maximize uniqueness between grouping solutions

## Package imports
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
  fitness <- sum(sumVec)
  # If all groups meet criteria, fitness set to +1
  if (length(unique(sumVec)) == 1 & fitness != 0) fitness <- 1
  # return fitness value
  return(fitness)
  
  # # Trying something else...
  # # Does every group have only one gender represented among its members?
  # genderSplit <- prod((lapply(critVals, function(x) nrow(unique(x)))) == 1)
  # if (genderSplit) {
  #   # Homogeneous groups have +1 fitness
  #   return(1)
  # } else {
  #   # Fitness penalty for mixed
  #   return(0)
  # }
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
  fitness <- sum(critVals)
  # If all groups meet criteria, fitness set to +1
  if (fitness == .1 * length(critVals)) fitness = 1
  # return fitness value
  return(fitness)
}

# Evaluate "fitness" for diversity
# Not yet implemented

# length(unique(Ethnicity)) == 1
# +0.1 to fitness per group meeting criteria
# If all groups meet criteria, fitness set to +1

# All the fitness functions wrapped up to return a vector of fitness values
# **RFP: I know I shouldn't use all these globals in here but I didn't want to pass
# a ton of parameters into this and potentially muck things up badly
MetaFitness <- function(indiv)
{
  currGroup <- classData
  currGroup$GroupID <- indiv
  result <- c(EvalGroupSize(SplitExtracted(currGroup, "ID")),
              EvalGender(SplitExtracted(currGroup, "Gender")),
              EvalML(SplitExtracted(currGroup, "Score_Cat")),
              EvalML(SplitExtracted(currGroup, "Leader"))
              )
  # Return the weighted results
  return(result * weighting)
}

# Get all the unique values that max out the parameters
# Not yet implemented
UpdateHoF <- function(hof, fit, pop) {
  
  return(hof)
}

## Main script run

# Read input
# Ask for file
myFile <- file.choose()
# Open the file
classData  <- read.csv(myFile, header=TRUE)

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
}
# Initial group arrangement; this is shuffled to produce the initial population
initArrang <- rep(1:nGroups, 5)[1:nStudents]

# Categorize the scores into a new column named Score_Cat
classData$Score_Cat <- CatScores(classData$Score)

initGroup <- classData
initGroup$GroupID <- initArrang

# Set up and run genetic algorithm
MU = 50L # Number of individuals
LAMBDA = 20L # Number of offspring
MAX.ITER = 100L # Max number of generations
# Relative weighting of each parameter
weighting <-  c(1, # Correct number and sizes of groups
                1, # Homogeneous gender groups
                1, # At most 1 person outside the middle 80% of scores
                1) # At most 1 leader
ref.point <- weighting # Ideal fitness values

# Toolbox initialization
# MetaFitness gives our vector of fitnesses, 4 objectives, maximize objectives
control <- initECRControl(MetaFitness, n.objectives = 4L, minimize = FALSE)
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

# Initialize Hall of Fame as an empty list
hof <- list()

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
  
  # Update log, Pareto archive, and Hall of Fame with new values
  updateLogger(log, population = population, fitness = fitness, n.evals = MU)
  updateParetoArchive(parchive, population, fitness)
  hof <- UpdateHoF(hof, population, fitness)
}

# Previous attempt using a "black box" wrapper
# gaRun <- ecr(fitness.fun = MetaFitness,
#              n.objectives = 3,
#              n.dim = 3,
#              minimize = FALSE,
#              mu = MU,
#              lambda = LAMBDA,
#              lower = lower,
#              upper = upper,
#              representation = "permutation",
#              perm = initArrang,
#              initial.solutions = population,
#              parent.selector = selSimple,
#              survival.selector = selNondom,
#              terminators = list(stopOnIters(MAX.ITER)))

# Extract and format Pareto-optimal solutions

# Output to file
# Format: CSV file, IDs with numeric group assignments over n columns of group arrangements
# ID   Arrangement_A   Arrangement_B   Arrangement_C   Arrangement_D ... Arrangement_n
# A, A             1               1               2               4
# B, Q             1               5               5               3
# C, F             5               3               4               2
# D, V             2               1               4               1
# F, C             2               4               1               5
# ...
# write.csv(results,
#           file=file.choose(filters = c("Comma Delimited Files (.csv)","*.csv")), 
#           row.names = FALSE)

## First attempt -- for reference
# StudentList$Race=as.character(StudentList$Race)
# if(sum(StudentList$Race == "African-American")==1){(StudentList$Race[StudentList$Race%in%"African-American"]<-"Solo")}
# if(sum(StudentList$Race == "Asian")==1){(StudentList$Race[StudentList$Race%in%"Asian"]<-"Solo")}
# if(sum(StudentList$Race == "Hispanic")==1){(StudentList$Race[StudentList$Race%in%"Hispanic"]<-"Solo")}
# 
# 
# #Number of groups to divide into
# A=5
# #The size of your groups
# B=4
# #Setting a starter value to what you want the sum to be
# D=0
# #While loop means that it will keep running until you meet the criteria
# #Each group that fulfills the rules will get a value of 6
# #If you want to allow one group to have violations of math or personality then you can change E from 0 to 1
# E=0
# while(D<((A*6)-E){
# #assign group number randomly to the student list
# StudentList$Group=(sample((rep(1:A, each=B)), replace=F))
# #a matrix holding the values of how well each group fills the rules
# C=matrix(nrow=A, ncol=1)
# #Loop through each group
#     for(i in 1:A){
#       #Check that there are not more than two males in a group
# gendercheck=ifelse((nrow(StudentList[StudentList$Gender==1&StudentList$Group==i,]))>2,0,2)
#   #Check that if there is an ethnic minority in the group, there is at least one other person that shares their ethnicity if possible
# #If there is only one student of a given ethnic group, we've changed their ethnicity to "Solo" already to not mess up this portion
# racecheck=  ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "African-American")==1), 0,
#       (ifelse((sum(StudentList$Race[which(StudentList$Group==i)]  == "Asian")==1),0,
#        (ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "Hispanic")==1),0,2)))))
# #Make sure that you either have one high scorer with the rest medium OR you have up to two low scorers with the rest medium
# mathcheck=ifelse((sum(as.numeric(as.character(StudentList$Math[which(StudentList$Group==i)]))))>2,0,1)
# #Make sure you don't have more than one "Leader" type
# personalitycheck=ifelse((nrow(StudentList[StudentList$Personality==1&StudentList$Group==i,]))>1,0,1)
# #Sum the values of whether or not the group in question met the rules 
#      C[i,1]=(gendercheck+racecheck+mathcheck+personalitycheck)}
# #Sum all the groups if they met the rules
# #If this is less than A*6, then the while loop will run over again
# #Once the while loop is fulfilled, then the dataframe "Student List" will have everyone assigned to a group that fills the rules
# D=sum(C)}
# 
