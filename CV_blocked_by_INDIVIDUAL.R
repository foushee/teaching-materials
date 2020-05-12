## -------------------------------------------------------------------------------------
## Cross-validation blocked by individual participant
## Ruthe Foushee 05/11/2020 for students in Psych 205 
##
## Example code for a [in this case 4]-fold cross-validation on 
## repeated measures data, where we split our data by randomly assigning 
## individual SUBJECTS (and all their rows) to different folds. 
##
## For the sake of illustration, we're pretending we have a dataset 
## called 'data.df', with repeated observations of 102 participants, 
## whose participant IDs are in the column 'subject.id'. 
##
## We're evaluating a mixed effects logit model (because that 
## has come up a lot with you all as well!) predicting trial-by-trial
## accuracy ('correct') from 'predictor1' and 'predictor2', with 
## random intercepts for each participant.
## -------------------------------------------------------------------------------------

## This is the formula for the model we care about, using the notation
## of the lme4 package: 

our.formula <- correct ~ variable1 + variable2 + (1|subject.id)

## Now we need to split our participants randomly into folds.

# First, we grab our participant ids:
participants.df <- data.frame(subject.id = unique(data$subject.id))

# while nrow(data.df) would give us the total number of observations, 
# and therefore have each participant represented multiple times, 
# nrow(participants.df) should give us the total number of individuals.

NSUBS <- nrow(participants.df) # number of individual participants
  
set.seed(1) # set our seed, obvs

# Because participants are often in your data frame in an order that might
# be meaningful and that we want to disrupt, we scramble them:

scrambled.df <- data.frame(subject.id = 
                              sample(participants.df$subject.id, NSUBS))

# Make a new column where we're going to put fold assignments:
scrambled.df$fold.ass <- 0

# Because we have 102 participants, two of the folds are going to have 
# 25 people in them, and two of them are going to have 26. 
# There are more elegant ways to do this, but this way is illustrative 
# and library-free.

# Tell the first 25 guys in the scrambled list that they're goin 
# in the first fold:
scrambled.df$fold.ass[1:25] <- 1

# Tell the next 25 guys in the scrambled list that they go to 
# fold number 2:
scrambled.df$fold.ass[26:50] <- 2

# Next 26 dudes go in 3:
scrambled.df$fold.ass[50:76] <- 3

# And last guys go in 4:
scrambled.df$fold.ass[76:102] <- 4

# We can double-check there are the right number of guys in each fold:
with(scrambled.df, tapply(subject.id, fold.ass, length))

# BACK TO DATA.DF, WHERE OUR HEART ACTUALLY LIES:
# This is just adding our new fold assignment column to our original
# data frame, so all the rows for each participant know where to go.
data.df <- merge(data.df, scrambled.df, by = "subject.id", all.x = T)


# NOW, KAPOW! YOU KNOW WHAT TO DO.
# Fit the model to all folds but one, to all folds but one...
our.mod.SUBS1 <- glmer(our.formula, data.df[data.df$fold.ass != 1,], family = binomial)
our.mod.SUBS2 <- glmer(our.formula, data.df[data.df$fold.ass != 2,], family = binomial)
our.mod.SUBS3 <- glmer(our.formula, data.df[data.df$fold.ass != 3,], family = binomial)
our.mod.SUBS4 <- glmer(our.formula, data.df[data.df$fold.ass != 4,], family = binomial)

# CALCULATE YOUR STATISTIC ETC. ETC.