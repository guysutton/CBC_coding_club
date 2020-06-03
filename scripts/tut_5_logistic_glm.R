###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #5: Logistic/binomial regression (general linear model)
# - Rhodes Universtiy
# - Script written: 27/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Understand when to use a logistic regression 
# - 2. Run a logistic regression model
# - 3. Evaulate the fit of your model (perform model diagnostics)
# - 4. Extract summary statistics from your model
# - 5. Make predictions from your model 
############################################################

# Load required libraries
library(tidyverse)
library(DHARMa)

# Set plot theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

###
# - What is a logistic regression?
###
# - Logistic regression (i.e. logit model, binomial regression)
#   is used to model a dichotomous outcome variable. 
#   - For example, presence/absense, 1/0, yes/no, dead/alive,
# - In contrast to lm and other glm's, logistic regression models 
#   the log odds of the outcome as a linear combination of the predictor variable(s).
#   - What that means is that we model the log(odds) of a presence vs absence, 
#     with respect to a predictor variable.
#   - These units make no sense to me - you will see this just now - 
#     but we can convert these units to predicted probabilities. Phew. 

###
# - Let's look at an example 
###

# Import data
data <- readxl::read_excel("./data_raw/binomial_data.xlsx")
data <- readr::read_csv2("https://raw.githubusercontent.com/guysutton/CBC_coding_club/master/data_raw/binomial_data.csv")

# Check data imported properly
str(data)
head(data)

# Our data is a hypothetical dataset, where we visited a range of field
# sites across an elevation gradient, and recorded whether we found our focal 
# organism (i.e. a Lantana plant, a weevil, a rabbit).
# - altitude = altitude/elevation above sea-level in metres
# - occ = presence (1) and absence (0) of our focal species. 

# Our research question is: does altitude influence the distribution 
# of our focal species?
# - So, we are going to model our species presence/absence (`occ` variable) 
#   as a linear function of altitude. 

# Why is a linear model not appropriate? 
ggplot(data = data, aes(x = altitude,
                        y = occ)) +
  geom_point() + 
  # What if we fitted a linear model? 
  geom_smooth(method = "lm")

# Let's say we did a linear model, how would it fare?
alt.lm <- lm(occ ~ altitude, data = data)
summary(alt.lm)

# Cool, but how well does our model fit the data?
plot(alt.lm, which = 1)
plot(alt.lm, which = 2)
plot(alt.lm, which = 3) # massive concern

# Plot 1 + 3 are a massive concern - 
# Red line should be flat around the y = 0 line.
# - 1. Look at shape of the red line - should be horizontal
# - 2. Look at y-intercept of red line
#      - y values < 0 = overprediction of outcome variable
#                 > 0 = underprediction of outcome variable 
# This model is going to significantly underpredict presence in the 
# middle of the data (i.e. at altitudes of 700-900m). 

###
# Run a logistic regression model
###

# Because your data are binary (dichotomous), you shouldn't be 
# using a 'lm' for your data to start with.
# - Model choice should be about the data properties, not just whether
#   your data fail diagnostics for a certain model family. 

# A simple binomial glm
alt.glm <- glm(occ ~ altitude, 
               data = data,
               family = "binomial")

# Print results
summary(alt.glm)

###
# - Evaluate model fit (model diagnostics)
###

# Calculate standardised residuals
alt.res = simulateResiduals(alt.glm)
testUniformity(alt.res)

# The qqplot shows that our residuals approximate the expectation under 
# the binomial distribution okay.  
# The KS test is a formal statistical test of our residual distribution vs 
# the binomial distribution, a non-significant value (i.e. ns is good). 
# - We want to see all those black triangles in the QQ plot falling
#   approximately on the red 1:1 line, and a KS-test P-value > 0.05. 

# 2. Linearity of expected value of Y and predictors (x)
# - Another assumtpion fo GLM's is linear relationship between
#   expected value of y and predictors (x). 
# - We expect no trend in the residuals - any trend is a concern.
# - To do this, we must inspect residuals vs fitted plot
plot(alt.res)
plotResiduals(alt.res)

# We want to see the bold black lines around the 0.25, 0.50 and 0.75 lines.
# Here, we see some deviations from that trend:
# - As x-vals increase, y vals increase a bit.
# - Bold lines always above our expected values of 0.25, 0.50, 0.75
#   - Taken together, our model is displaying some heterogeneity in variance,
#     and will systematically underpredict probability of recording focal species,
#     BUT: The adjusted quantine test = n.s. here, so not bad enough to be too worried.
#   - Just keep slight issues in mind when interpreting model output. 

###
# - Interpreting model output 
###

# - (1) Is my treatment/factor significant?
#       Look at the row for your predictor variable (here: altitude)
#       Look at the P-value: P <0.05 means this variable is significant 
#       - Some type of association between altitude and occurence of our focal species. 

# - (3) How does my treatment effect response?
#       Look at the estimate value in your predictor variable row
#       > 0 estimate = response increases with greater values of predictor
#       < 0 estimate = response decreases with greater values of predictor
#       NB - This value is on log-odds scale - need to back-transform
#          - Coefficients indicate the change in log-odds of our response
#            with a 1 unit increase in the predictor variable. 
exp(coef(alt.glm)[2])

# So, as altitude increases by 1m, the log-odds of our species 
# being present increases by a factor of 0.99. 
# WHAT ON EARTH DOES THAT MEAN? 

# Let's rather make our outcomes odds-ratios
# - Let's add 95% CI's while we are at it. 
exp(cbind(OR = coef(alt.glm), confint(alt.glm)))

# - Now we can say that for every unit increase in altitude,
#   the odds of us recording our focal species (i.e. recording a 1/presence)
#   decreases by a factor of 1.01 (0.99 means a decrease) or 1%. 
#   - Another example would be easier to interpret:
#     Say our OR for altitude = 1.20, this would mean that for every 1 unit
#     increase in altitude, the odds of recording our species increases by a factor 
#     of 1.2x or 20%. 
#   - Again, OR for altitude = 0.9, this would mean that for every 1 unit
#     increase in altitude, the odds of recording our species decreases by a factor 
#     of 1.1x or 10%.

# This is still pretty confusing though, at least to me. 

# Let's rather look at predicted probabilities. 
# Make a vector of altitudes over which we want to make predictions. 
# Predict from minimum altitude to maximum + 500m
ndata <- with(data, 
              data_frame(altitude = seq(min(altitude), 
                                        max(altitude)+500,
                                        length = 100)))
ndata

## add the fitted values by predicting from the model for the new data
ndata <- cbind(ndata, predict(alt.glm, 
                              newdata = ndata, 
                              type = "link",
                              se = TRUE))
ndata

# Convert (backtransform) these data using 'plogis' and 'se_fit' 
ndata <- ndata %>%
  mutate(pred_prob = plogis(fit),
         lower_ci  = plogis(fit - (1.96 * se.fit)),
         upper_ci  = plogis(fit + (1.96 * se.fit)))
head(ndata)

# Make the plot
ggplot(data = ndata) +
  geom_line(aes(x = altitude,
                y = pred_prob)) +
  geom_ribbon(aes(x = altitude,
                  ymin = lower_ci,
                  ymax = upper_ci),
              alpha = 0.2) +
  labs(x = "Altitude (m)",
       y = "Probability of recording species") 

# That makes a lot more sense to me. 
# - Between 0 and 600m altitude, we are likely to record the species.
# - Above 600m, probability of finding species rapidly declines.
# - Above 1200m, probability approaches 0,
# - But, the CI's (gray bands), are quite wide, indicating a lot of uncertainty. 
# - With a small sample size like this study, not surprising.