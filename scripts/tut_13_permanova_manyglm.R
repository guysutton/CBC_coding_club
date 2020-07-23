###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #13: PERManova and model-based alternatives 
# - Rhodes Universtiy
# - Script written: 21/07/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Understand what PERManova is and does
# - 2. Run a simple PERManova
# - 3. Interpret PERMAanova output
# - 4. Understand how 'manyglm' is different from PERManova
# - 5. Run a simple manyglm analysis
# - 6. Interpret a manyglm output 
############################################################

######################################################################
# Load packages ------------------------------------------------------
######################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr, 
               MASS,
               vegan,
               mvabund) # 'vegan' is the workhorse of multivariate analyses

# Change ggplot theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

######################################################################
# Load raw data ------------------------------------------------------
######################################################################

# Import species community dataset
raw_data <- readr::read_csv2("./data_raw/sporobolus_insects_clean.csv")

# Let's make character columns into factors 
raw_data <- raw_data %>%
  dplyr::mutate(across(where(is.character), as.factor)) %>%
  dplyr::rename(disturb = disturb_allow_overwinter)
head(raw_data)

# Keep just the species abundance data 
com_mat <- raw_data %>% 
  dplyr::select(starts_with("sp_")) %>%
  # Add a dummy species for Bray-Curtis dissimilarity 
  dplyr::mutate(sp_dum1 = 1)
com_mat

############################################################################
# Does insect community composition differ between seasons and dis --------
############################################################################

# Remember that last week we performed nMDS to visualise how 
# insect community composition between surveys I performed on my grasses 
# during summer vs winter (season) and between disturbed vs undisturbed sites
# (disturbance regimes).

# This week we are going to perform a hypothesis test to statistically evaluate
# if insect community composition differed between seasons and disturbance regimes. 
# The test we are going to perform is called a Permutation anova, or PERMANOVA. 
# - Null hypothesis = the centroids and dispersion of data points are equivalent
#                     between groups. 
# - Rejecting the null hypothesis means that either the centroids or variance 
#   differs amongst the two groups. 
# - PERManova is extremely popular, particularly in ecology, because it is 
#   thought to be relatively free of constraints (e.g. normality, excess zeroes).
#   - We will see later that this is not necessarily the case. 

# Perform PERManova using the 'adonis' function 
# - Dependent variable = species abundances matrix
# - Predictors = Disturbance * Season (check whether interaction is required)

# Fit global PERManova (i.e. with interaction term)
adonis.int <- adonis(com_mat ~ season * disturb, 
                     # Constrain permutations to within site (can change p vals)
                     strata = raw_data$site_code, 
                     data = raw_data,
                     permutations = 999)
adonis.int

# Fit PERManova without the interaction term
adonis.add <- adonis(com_mat ~ season + disturb, 
                     # Constrain permutations to within site (can change p vals)
                     strata = raw_data$site_code, 
                     data = raw_data,
                     permutations = 999)
adonis.add

# Clearly, we did not require the interaction term. 
# So, our results table can be interpreted much like a typical ANOVA-type table.
# - Both season and disturbance regime had an influence on insect community 
#   composition. 
# - THe R2 values are analogous to an R2 from a linear model - they roughly
#   indicate how much variation was accounted for by each term
#   - Disturbance accounted for 13.2%, and season only 2.5%, of observed 
#     variation in insect community composition variation. 
#   - The residuals show much much variation was not accounted for (83.9%).
#     - Wowzers!!! 

# CURVEBALL: PERManova may not be the happy-go-lucky, easy-does-it analysis
#            that it has often been made out to be. 
#            - Just because something is called (semi)-parametric, does 
#             not mean that it is without constraits.
# *** DISTRIBUTION FREE != ASSUMPTION FREE 
# (IF THIS IS THE ONLY THING YOU LEARN FROM THIS LESSON, THEN I AM HAPPY)!!!

# What is the problem? 
# - Well, most ecological data follows quite a strong mean-variance relationship. 
# - Normally, the variance in abundance increases with increasing mean abundance. 
# - Distance-based analyses (e.g. nMDS, PERManova, anything that requires the 
#   calculation of a dissimilarity or distance matrix) often cannot account
#   for this strong mean-variance relationship in ecological data.

# How does this effect your analyses?
# - Failing to adequately capture mean-variance can result in
#   wildly misleading results. 
# (1) Location effects can be misinterpreted as dispersion effects. 
#     - You can't accurately diagnose whether the differences observed 
#       occur because of more variability, or a shift in the group centroids. 
# (2) Taxa with high variance contribute more to differences between groups 
#     than those taxa which actually differ between groups. 
# (3) NB for PERManova - Power to detect differences between groups is very low,
#     unless the taxa which differ between groups also have highly variation.
#     - We may miss a group-effect, when there is actually a group-effect. 

# What can we do? 
# - Instead of using distance-based analyses, we can use model-based analyses.
# - Model-based analyses rely on the framework of GLM's to specifically
#   model a particular mean-variance relationship. 
# - Overcomes many of the issues associated with distance-based analysis, 
#   but, you must use the right model!!! 


######
### - (1) Model-based hypothesis testing - manyglm 
######

# Here, we are going to use the 'manyglm' function to perform
# model-based hypothesis testing of whether insect community composition
# differed between seasons and disturbance regimes. 

# Firstly, we need to convert the species abundance matrix to an 'mvabund' object
insect.spp <- mvabund(com_mat)

# Have a look at the data
par(mar=c(2,10,2,2)) # adjusts the margins
boxplot(insect.spp,
        horizontal = TRUE,
        las=2, 
        main="Abundance")

# Lots of variation...

# Secondly, check the mean-variance relationship
meanvar.plot(insect.spp,
             all.labels = FALSE,
             xlab = "Mean species abundance",
             ylab = "Variance")

# We clearly see that variance in abundance (y-axis), increases 
# with increasing mean abundance (x-axis). 
# Let's keep this in mind for now, but proceed. 

# Now, let's run a global multivariate GLM 
# We will first try poisson distribution
# Poisson is a standard model for count (e.g. abundance data),
# which has the property that the variance is expected to increase 
# in a linear manner with the mean. 
mod1 <- manyglm(insect.spp ~ raw_data$disturb * raw_data$season,
                family = "poisson")

# Check model assumptions
plot(mod1)

# Here, we see a slight fan-shape in the model residuals. 
# - I mean that the vertical range at the lowest x-value is 
#   much smaller than the spread at the highest x-value. 
# - We need to model the variance differently.

# Before we do that, let's plot mean-variance relationship directly
# Add a factor variable containing the disturb x season level combos
com_data_lts <- raw_data %>%
  unite("tr.block", disturb, season, remove = FALSE)

# Make plot
meanvar.plot(insect.spp ~ com_data_lts$tr.block,
             col = as.factor(com_data_lts$disturb),
             xlab = "Mean (log scale)",
             ylab = "Variance (log scale)",
             main = "")

# Add linear increase line
# if the data were Poisson distributed, then the raw data points
# would be expected to fall approximately on this line 
# (indicate of a linear increase in var with mean)
abline(a=0, 
       b=1, 
       untf = TRUE, 
       lty=3)

# Add legend
legend("bottomright", 
       legend = c("Disturbed", "Undisturbed"), 
       col = c("red", "black"), 
       pch = c(1,1), 
       bty = "n", 
       pt.cex = 1.1, 
       cex = 1.1, 
       text.col = "black", 
       horiz = F)

# Clearly, the variance increases > mean, 
# Therefore, a poisson model likely not appropriate. 

# Try negative-binomial model
# Negative-binomial is another distribution that can be used for count data
# It differs from the poisson in that the variance is expected to increase 
# > mean (quadratically). 
# The previous graph showed that var > mean, so we should expect 
# NB model to perform better than poisson
mod2 <- manyglm(insect.spp ~ raw_data$disturb * raw_data$season, 
                family = "negative_binomial")

# Check model assumptions
plot(mod2)

# Much better. There is no longer a fan-shape in model residuals.

### Now we are going to test the multivariate hypothesis of whether 
### insect species assemblages varied according to disturbance regimes
### and/or season (to account for potential differences between seasons)/ 
### This analysis uses likelihood-ratio tests (LRT) and 
### resampled P-values to test significance.

anova.manyglm(mod2)

### We can see from this table that there is a significant effect of disturbance
### meaning that the species composition 
### of herbivores clearly differs between disturbance regimes.
### Season and an interaction term between season and disturbance were not NB. 

# How does the model-based result differ from the distance-based analysis (PERManova)?
# - 
# - 

###########################################################################
# Which species drive these differences? ----------------------------------
###########################################################################

### To examine this further, and see which insect species are more likely 
### to be found associated with the different disturbance regimes:
### We run univariate tests for each species separately.
### This is done by using the p.uni="adjusted" argument in the anova function. 
### The "adjusted" part of the argument refers to the resampling method
### used to compute the p values, taking into account the correlation between 
### the response variables. 
### This correlation is often found in ecological systems where different species 
### will interact with each other, competing with or facilitating 
### each others' resource use.
anova(mod2, 
      nBoot = 999, 
      p.uni = "adjusted")

### Each insects gets its own ANOVA-like table. 
### P-val < 0.05 for sp_tet1 (disturbance only), sp_tet2 (disturbance only),
###                  sp_bru1 (disturbance only)
### This tells us that these three species were responsible for the difference 
### in community composition between disturbance regimes. 
### - Make a plot of abundances between seasons and disturbance regimes to 
###   determine the sign and magnitude of these effects. 
### - I am not going to show how to make the plot. You should be able to do this 
###   by yourself by now. 
### - Notice how none of the insects differed by season? 
###  - That shouldn't be a surprise as season was not signficant in the 
###    global multivariate model-based test.
###  - Another indicator that PERManova was probably wrong in saying season was
###    significant. 

### DISCLAIMER:
### - I have only really scratched the surface today, so if you would like to 
###   undertake a similar analysis, please chat to me. I would be happy to help. 