# Contrast
# planned contrast
# 1.
#Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership<-emmeans(Interaction_AT, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
Contrast1_AT <- contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)
sum_Contrast1_AT<-summary(Contrast1_AT)

# 2.
#???Contrast 2:by valence
emcontcat  <- emmeans(Interaction_AT, ~ valence*membership,data=my_data_age_T)
Contrast2_AT<-contrast(emcontcat, "pairwise",by="valence")
sum_Contrast2_AT<-summary(Contrast2_AT)

# 3.
```{r planned contrast, include  = FALSE}
#Tip: our dataset is unbalanced, since the analysis of variance is performed in R by fitting a linear model 
#created from indicator variables for the levels of the factor.  
#so this validity of this approach does not depend on balance in the data. 

# look at the levels of our factor
levels(picture)
# Defining contrast matrix
##tell R which groups to compare
c1 <- c(0, 1, -1) # sexy vs. young
c2 <- c(-1, 1, 0) # sexy vs. elderly
c3 <- c(-1, 0, 1) # New: young vs. elderly
## combining groups into a matrix
mat <- cbind(c1,c2,c3)

# tell R that the matrix gives the contrasts you want
contrasts(picture) <- mat

#Compute the analysis of variance
model1 <- lm(acceptance ~ picture, Simulated_Data)
summary(model1)

#Summary of the multiple comparisons
sum_plcon <- summary.aov(model1, split=list(picture=list("sexy vs. young"=1, "sexy vs. elderly"=2, "young vs. elderly"=3)), expand.split = TRUE)
sum_plcon

# Report results
p_plcon <- sum_plcon[[1]][, 5] 
fstat_plcon <- sum_plcon[[1]][, 4] 
df_plcon <- sum_plcon[[1]][, 1]

``` 

# 4.
# Post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(Interaction_AT,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)
