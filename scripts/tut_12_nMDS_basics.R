###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #12: nMDS
# - Rhodes Universtiy
# - Script written: 14/07/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Understand what nMDS does
# - 2. Run a simple nMDS
# - 3. Interpret nMDS output
############################################################

######################################################################
# Load packages ------------------------------------------------------
######################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr, 
               MASS,
               vegan) # 'vegan' is the workhorse of multivariate analyses

######################################################################
# Load raw data ------------------------------------------------------
######################################################################

# Import species community dataset
raw_data <- readr::read_csv2("./data_raw/sporobolus_insects_clean.csv")

######################################################################
# Exploratory data analysis ------------------------------------------
######################################################################

# Did the data import correctly?
head(raw_data)

# Let's make character columns into factors 
data_clean <- raw_data %>%
  dplyr::mutate(across(where(is.character), as.factor))
head(data_clean)

# Here, we have a species abundance matrix of 6 insects associated
# with an African grass that I worked on during my Ph.D. 
# - I surveyed 23 sites (once every 2 months) for a single year, 
#   recording the abundance of each insect when dissecting 10 plants
#   during each survey.
# - I then note the site, sampling date, season and disturbance regime
#   at that site.
# - I was really interested to see whether insect community composition
#   differed between seasons and disturbance regimes.
#   - Ecological theory suggests that insects (particularly specialists)
#     should be absent/reduced in abundance under high disturbance. 
# - So, I hypothesized that insect community composition will 
#   differ between seasons and disturbance regimes... 

######################################################################
# nMDS to visualise data ---------------------------------------------
######################################################################

# The first step towards answering this question is to visualise 
# whether community composition differs between surveys performed 
# during seasons and/or disturbance regimes. 

# What is nMDS?
# - nMDS is a statistical tool for multivariate data which allows us 
#   determine how the constituent species in a community may change 
#   between surveys (e.g. from different sites, seasons, disturbance, ect...)
# - Here, we are not just looking at whether an individual species changes
#   in abundance between surveys, but the entire collection of species.
# - Input data requires a similarity matrix (not just species abundances)
# - Typically, ecologists use Bray-Curtis similarity index.
#   - The index takes values ranging from 0 (two surveys share the same      #     species) to 1 (two surveys do not share any common species). 


# Step 1: Prepare data -----------------------------------------------

# Now keep only the species abundances columns 
com_data_sp <- data_clean %>% 
  dplyr::select(-c(site_code, sampling_date, season, disturb_allow_overwinter))
com_data_sp

# We could also have done this more efficiently 
com_data_sp <- data_clean %>% 
  dplyr::select(starts_with("sp_"))
com_data_sp

# Multivariate analyses are sensitive to absolute abundances in a sample,
# which can skew results. To overcome this issue, we can take absolute
# abundance data and convert it to a relative abundance. 
# - Relative abundance is the percent composition of an organism relative 
# to the maximum number of individuals in the community. 

# Because of issues where rows sum to 0, we must first use 
# zero-adjusted Bray-Curtis dissimilarity matrix - add dummy species 
# where abundance = 1
com_data_sp$sp_dum1 <- 1
com_data_sp

# Now calculate relative abundances
com_data_std  <-         
  vegan::decostand(com_data_sp, 
                   method = "total")
com_data_std


# Step 2: Calculate dissimilarity metric -----------------------------

# Now calculate Bray Curtis metric 
com_data_distmat <- 
  vegan::vegdist(com_data_std, method = "bray")

# Make this into a matrix
com_matrix <- as.matrix(com_data_distmat)
com_matrix


# Step 3: Run nMDS ---------------------------------------------------

# Perform a preliminary nMDS
comm.mds <- metaMDS(comm = com_matrix, # matrix of species abundance data
                    distance = "bray", # dissimilarity metric
                    trace = FALSE, # Suppress R information 
                    autotransform = FALSE,
                    k = 2, 
                    trymax = 100) 

# R will not allow any analyses where row sums for the 
# species x matrix are < 1, if this is the case, it throws:
# Error in cmdscale(dist, k = k) : NA values not allowed in 'd'
# In addition: Warning messages:
# 1: In distfun(comm, method = distance, ...) : you have empty rows: 
# their dissimilarities may be meaningless in method 
# 2: In distfun(comm, method = distance, ...) : missing values in results.
# This is why we used the zero-adjusted Bray-Curtis index. NB!


# Step 4: Evaluate fit of nMDS ---------------------------------------

# Firstly, we can plot a Sheppard/stressplot. 
# This looks at how closely the distances between sampling units 
# are preserved in the ordination versus the dissimilarity value itself.
# Here, we are looking for little scatter around the line. 
# - Large scatter is bad. 
# - We really want Goodness of fit (R2) > 0.9 = excellent
stressplot(comm.mds)

# Here, the sheppard plot looks okay.
# (Goodness of fit (R2) > 0.9 = excellent) 

# Secondly, we can calculate a stress value 
# - Stress provides a measure of the degree to which the distance between
#   samples in reduced dimensional space (usually 2-dimensions) corresponds
#   with the actual multivariate distance between the samples. 
#   - Lower stress values indicate greater conformity and therefore are
#     desirable. 
#   - High stress values indicate that there was no 2-dimensional arrangement
#     of your points that reflect their similarities. 
#   - Clarke 1993 suggests the following guidelines for acceptable stress
#     values: 
#     - <0.05 = excellent
#     - <0.10 = good
#     - <0.20 = usable 
#     - >0.20 = not acceptable
#     - Clarke, K. R. (1993). Non-parametric multivariate analysis of changes
#       in community structure. Austral J Ecol 18: 117-143.
comm.mds$stress

# Here, our stress value = 0.12, so our nMDS is usable (okay). 

# Step 5: Plot nMDS --------------------------------------------------

# Plot preliminary nMDS using base functionality 
plot(comm.mds)

### This is not very informative. Let's add some sample details to the plot. 

# # Extract the XY co-ordinates from the nMDS plot
xy.mds <- data.frame(comm.mds$points)

# Now add the categorical factors from the original data.frame
xy.mds$disturb <- data_clean$disturb_allow_overwinter
xy.mds$season <- data_clean$season
xy.mds$site <- data_clean$site_code

# Changes the covariates into factors
xy.mds <- xy.mds %>%
  mutate(season = as.factor(season),
         disturb = as.factor(disturb))

# Plot the nMDS with colour-coded points for disturbance regimes, 
# and shapes for season
ggplot() + 
  # Use geom_jitter to add each survey as a point
  geom_jitter(data = xy.mds, aes(x = MDS1, 
                                 y = MDS2,
                                 shape = season,
                                 colour = as.factor(disturb)),
              width = 0.05, height = 0.05, size = 2) +
  # Manually specify colours 
  scale_colour_manual(values=c("black", "gray60")) + 
  # Labels for axes and legend 
  labs(x = "nMDS axis 1",
       y = "nMDS axis 2",
       colour = "Disturbance",
       shape = "Season") +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
        legend.position = "right")

# Save this nMDS to file 
ggsave("./figures/fig_5_nMDS_plot.png", 
       width = 6, 
       height = 4)

