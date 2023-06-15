setwd("C:/Users/larue/Downloads/BAM/Thesis")

# importing packages
library(cbcTools)
library(idefix)
library(logitr)
library(choiceDes)

# setting seed
set.seed(626)

# generating the profiles with profile IDs
profiles <- cbc_profiles(
  price = c("€35", "€100", "€165"),
  quality = c("7","32","57"), 
  donations = c("none","2%","4.75%"), 
  emissions = c("carbon neutral", "37 kilotons/year","72.6 kilotons/year")
)

r = 300 # respondents
alts = 3 # alternatives per set
q = 10 # amount of sets

# generating the design
design3 <- cbc_design(
  profiles = profiles, 
  n_resp = r,
  n_alts = alts, 
  n_q = q,
  priors = list(
    price = c(-0.1, -0.2),
    quality = c(0.1, 0.2),
    donations = c(0.1, 0.2),
    emissions = c(-0.1, -0.2)
  ),
  probs = TRUE
)
# Bayesian D-efficient design found with DB-error of 1.01147
# fine if checked with OG documentation: their DB-error also above 1

# checking how many of the profiles are used in the design
unique <- unique(
  design3[
    c("price", 
      "quality", 
      "donations", 
      "emissions", 
      "profileID")
    ]
  )

# selecting a subset of 10 profiles that will be the choice set
# https://cran.r-project.org/web/packages/choiceDes/choiceDes.pdf
set1 <- optBlockC(
  withinData = unique[c("price", "quality", "donations", "emissions")], 
  blocksizes = rep(alts, q)
)

# cbcBlocks <- set1[["Blocks"]]

#define file name
# sink('blocks.txt')
# print(cbcBlocks)
# sink()

# ----------------
# alternatively, the design can also be constructed using the AlgDesign package
# https://cran.r-project.org/web/packages/AlgDesign/AlgDesign.pdf
alg.design <-optFederov(
  ~., 
  profiles[c("price", "quality", "donations", "emissions")],
  nTrials= alts * q,
  nRepeats = r,
  eval=TRUE)

# checking its validity
alg.design$Dea

# the design can be divided to obtain several choice sets
# https://www.rdocumentation.org/packages/AlgDesign/versions/1.2.1/topics/optBlock

set2 <- optBlock(
  ~.,
  alg.design$design,
  blocksizes = rep(alts,q))
# -------------------

# simulating the experiment
data <- cbc_choices(
  design = design3,
  obsID = "obsID",
  priors = list(
    price = c(-0.1, -0.2),
    quality = c(0.1, 0.2),
    donations = c(0.1, 0.2),
    emissions = c(0.1, 0.2)
  )
)

power <- cbc_power(
  data = data,
  pars = c("price", "quality", "donations", "emissions"),
  outcome = "choice",
  obsID = "obsID",
  nbreaks = 10,
  n_q = q
)

# check that if 300 respondents pick 1 option out of 3 over 10 sets, 
# 3000 choices should be made
length(data$choice[data$choice == 1])

# visual check of the ideal amount of respondents
plot(power)

# https://www.zbg.uni-hannover.de/fileadmin/zbg/Publikationen/publications/20160523_Determination_of_part-worth-utilities_of_food-labels_Acta_ZBG_SM_V0.1.pdf
# https://koreascience.kr/article/JAKO201205759627205.pdf
