# Secondary Data Analysis Project1: Studying the Relationship between sleep deprivation and emotional regualtion and relationship between psychological states and cognitive performance

# Read the file from the local computer 
data <- read.csv("/Users/zhaolinyang/Desktop/Indpendent Project/Project 1st/Dataset.csv")
set.seed(2026) # Setting random seed for reproducibility of bootstrapping resampling results 

## Section for packages installation
install.packages("qqplotr") # Visualization of relationship between predictor varaibles and outcome variables
install.packages("psych")
install.packages("lmtest")
install.packages("car")
install.packages("boot") # Bootstrapping when there is violation of normality of errors in assumptions checking
install.packages("scatterplot3d") # Visualization of multiple linear regression in a 3D space with OLS hyperplane(two dimensions)

# Necessary packages for principal component analysis (PCA) for variables compression exploratorily 
install.packages("MASS")
install.packages("factoextra")
install.packages("ggplot2") 

install.packages("emmeans") # Calling for levene test particularly designed for testing of homogeneity of variance in ANOVA or ANCOVA

# Installation of necessary packages for construction of correlation heatma for exploratory visualization
install.packages("corrplot")
install.packages("dplyr")
# Due to internet restriction, mirror site is used for the installation. Here, mirror site of Tsinghua University is used for the installation
install.packages("reshape2", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/") 
install.packages("Hmisc", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

## The Section for Corresponding Packages Loading
library(qqplotr)
library(psych)
library(lmtest)
library(car)
library(boot) 
library(scatterplot3d) 
library(MASS)
library(factoextra)
library(ggplot2) 
library(emmeans) # Calling for levene test particularly designed for testing of homogeneity of variance in ANOVA or ANCOVA
library(corrplot)
library(dplyr)
library(reshape2)
library(Hmisc)

#=====================================================================================================================
# MODULE1: Dataset Overview and Descriptive Statistics (EDA)
#=====================================================================================================================

# STEP1: Brief overall summary for the whole data
dim(data) # Quick check for the number of sample size (N = 60) and number of variables (p = 14)
names(data) # Labeling variable names. Especially useful for quick check for if there is any misspelling in code
summary(data) # Generate a brief descriptive summary of all variables

#Checking missing values in the whole data
colSums(is.na(data)) #Confirmation as described by the authors, there is no missing value in the dataset
sum(duplicated(data)) #Checking for any duplicated rows in the data 

# STEP2 Focusing on variables of interest with description and give one initial summary
# Seven variables of interest included in the two aims. Mainly focusing on the sample statistics of the 7 variables
variables <- c("Sleep_Hours", 
               "Emotion_Regulation_Score",
               "Stress_Level",
               "Stroop_Task_Reaction_Time",
               "N_Back_Accuracy",
               "PVT_Reaction_Time")

mean_values <- sapply(data[variables], mean) # Computation of each variable's mean
variance_values <- sapply(data[variables], var) # Computation of each variable's variance in sample
SD_values <- sapply(data[variables], sd) #Computation of each variable's standard deviation in sample
descriptive_statistics_table <-cbind(Mean = mean_values,
                                     variance = variance_values,
                                     SD = SD_values)
round(descriptive_statistics_table,2) #Combine the values to be a table for summary and comparison

#=====================================================================================================================
# MODULE2: Correlation Matrix Heatmap for Visualization of overall structure of correlations
#=====================================================================================================================

# Initial Visually exploration of the correlation structure among variables through correlation matrix heatmap
# Construction of correlation matrix heatmap
numeric_data <- data%>%dplyr::select(Sleep_Hours,
                                     Sleep_Quality_Score,
                                     Daytime_Sleepiness,
                                     Stroop_Task_Reaction_Time,
                                     N_Back_Accuracy,
                                     Emotion_Regulation_Score,
                                     PVT_Reaction_Time,
                                     Age,
                                     BMI,
                                     Caffeine_Intake,
                                     Physical_Activity_Level,
                                     Stress_Level) # Remove all random variables that are not continous numerical but categorical and only left necessary random varaibles 

cor_matrix <- cor(numeric_data) 
print(cor_matrix) # Initial establishment of correlation matrix heatmap mathematically without visualization

# Calculation of values of standardized correlation coefficients and corresponding P-values
cor_results <- rcorr(as.matrix(numeric_data))
print(cor_results)
cor_matrix <- cor_results$r
sig_matrix <- cor_results$P

stars <- ifelse(cor_matrix == 0, "", 
                ifelse(sig_matrix < 0.001, "***",
                       ifelse(sig_matrix <  0.01, "**",
                              ifelse(sig_matrix <0.05, "*",
                                     "")
                              )
                       )
                ) # Stars as indicators of significance. In this heatmap, no star represents there is statistical insignificance

diag(stars) <- "" # Matrix diagnol elements are special (self-correlation)

cor_long <- melt(cor_matrix)
signif_long <- melt(stars)

cor_long$stars <- signif_long$value

ggplot(data = cor_long, 
       aes(x = Var1, y = Var2, fill = value)) +   
  geom_tile() +
  geom_text(aes(label = paste0(round(value,2),stars)), # Combining values of values of standardized coefficients and significance stars together into blocks
            color = "black", 
            size = 4,
            data = cor_long) +
  scale_fill_gradient2(                
    low = "red", 
    high = "blue", 
    mid = "white",
    midpoint = 0,
    limits = c(-1, 1), 
    space = "Lab", 
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10, face = "bold"), # Elaboration of word expression of names of random varaibles
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text (size = 20, hjust = 0.5, face = "bold")) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "",
       caption = "*, p < .05; ** p < .01, *** p < 0.001"
  )
# Correlation Heatmap is used for data exploration as it indicates zero-order standardized correlation coefficients among random. variables 
# Apparently, the map shows any pairwise zero-order associations among continous variables are not statistically significant 
# Nevertheless, it does not mean the modelling is unnecessary and meaningless since modelling is important for providing conditional associations in multiple linear regression
# Regression-based approach address different questions by estimating conditional associations, quantifying unstandardized effects, examinging model assumptions and evaluating potential interaction effects.

#=====================================================================================================================
# MODULE2: Models-Specific Preparation
#=====================================================================================================================

# STEP1: Generation of mean-centered variables of interest(sleep-hours, stress levels and emotional regulation score for AIM2)
data$c_sleep_hours <- data$Sleep_Hours - mean(data$Sleep_Hours) # Mean-centering of sleep duration in a newly created column
data$c_emotion_regulation <- data$Emotion_Regulation_Score - mean(data$Emotion_Regulation_Score) # Mean-centering of emotion regulation in a newly created column
data$c_Stress_Level <- data$Stress_Level - mean(data$Stress_Level) # Mean-centering of stress level in a newly created colum

#STEP2 Converting continous stress level to categorical variable with three groups based on criteria of level of stress (Perceived Stress Scale PSS-10)
# PSS-10 evaluates stress levels on a 0 to 40 scale. It classifies low stress group (0-10), moderate stress (14-26) and high stress group (27-40)
data$Moderate_stress <- ifelse(14 <= data$Stress_Level
                             &data$Stress_Level <= 26, 
                             1, 0)
data$High_stress <- ifelse(27 <= data$Stress_Level&
                                data$Stress_Level <= 40, 
                              1, 0)
data$Stress_Group <- ifelse(data$Moderate_stress == 1, #Classification of different stress groups
                               "Moderate Stress",
                               ifelse(data$High_stress == 1,
                                      "High Stress",
                                      "Low Stress"
                                      )
                            ) 
data$Stress_Group <- factor(data$Stress_Group,
                            levels = c("Low Stress", "Moderate Stress", "High Stress")
                            ) #Low stress group (stress level: 0-13) is baseline group of reference for later ANCOVA


# For valid conventional sampling-distribution-based inference, certain assumptions ought to be resonably satisfied for each fitted regression model.
# In other words, regression assumptions are examined for each fitted model rather than for the dataset as a whole.

#This module defines reusable diagnostic functions so that the same checks can be consistently and conveniently applied to each model in the linear analysis
#As discussed in project overview, particular attention will be given to (1) approximate normality of errors; (2) Linearity of relationship between predictors and outcome (3)homoscedasticity (homogeneity of variance of outcome variable)
# (4) influential observations (e.g., outliers in sample) and (5) multi-collinearity among predictors incorporated in multiple linear regression. 
# Notice, autocorrelation which may influence uncertainty of estimation is not explicitly checked here since the authors state that the observations are measured independently

#---------------------------------------------------------------
# CHECK 1: Diagnostics of Approximate Normality of Errors
# Because the OLS coefficient estimator is a linear function of the model errors, approximate normality of errors supports an approximately normal sampling distribution of coefficient estimates and therefore conventional t- and F-based inference.
# Therefore, normality of errors mainly supports the validity of conventional hypothesis testing based on t and F distributions,especially in small samples because in large sample, CLT can compensate
# QQ plot and the Shapiro-Wilk test (S-W test) will be used for checking the assumption
Residuals_Normality_Checking <- function(model){
  Residuals_Model <- residuals(model) # Inset any linear regression model, the functions automatically compute its residuals for later calculations if applicable
  par(mfrow=c(1,2))
       hist(Residuals_Model,
       breaks = 10,
       freq = FALSE,
       col = "black",
       border = "white",
       main = "Histogram of Regression Residuals",
       xlab = "Residuals") # Constructing histogram describing the residual distributions for visualization of distribution of redisuals in the existing sample
  
       qqnorm(Residuals_Model,
         main = "Q-Q Plot of Regression Residuals",
         ) # Drawing QQ plot for visually comparison of consistency between observed values of residuals and expected values of residuals given normality of errors
  
       qqline(Residuals_Model,
         col = "red"
         )
       
       print(shapiro.test(Residuals_Model)) #S-W test for statistically testing normality of residuals where H0 = no violation of normality of errors
} # As expected, once model is constructed and inserted into the fucntion, hitogram of residuals, Q-Q plot of residuals and result of S-W test should be delivered. 
#Based on the result, deciding whether the assumption of normality of errors is violated or not visually and statistically. Remedy (e.g., log-transformation is then required if there is violation)

#---------------------------------------------------------------
#CHECK2 Linearity of Relationship between Predictors and Outcome 
# Because the linear regression model presumes that conditional expectations of outcome given predictors can be expressed by a linear function, 
# Thereforem the inference and estimation are risky to be invalid and meaningless if the assumtpion is violated
#Scatterplot (fitted values VS. residuals) visual inspection can be used for checking the assumption. Notice, the scatterplot can also partially provide certain evidence that whether the assumption of Homoscedasticity is violated visually

Linearity_Relationship_Checking <- function(model){
  par(mfrow=c(1,1))
  plot(fitted(model),
       residuals(model),
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals VS Fitted Values")
} #If a random fluctuation of residuals without visually obvious curvature or pattern are observed, it provides no obvious evidence that the linear relationship between predictors and outcome variables is violated
#Additionally, if the dispersion of residuals shows no visually obvious cahnge across fitted values (e.g., cone shape of residuals), it may visually provides no obvious evidence that the assumption of homoscedasticity is violated

#---------------------------------------------------------------
# CHECK3 Homoscedasticity Homogeneity of Variance of Outcome Variables)
# Because the OLS coefficient estimator is a linear function of the model errors, satisfaction of homoscedasticity provides accurate reflection of sample statistics uncertainty in conventional sampling distribution
# Therefore, the estimation and inference will be possibly untrustworthy
# Scatterplot (fitted values VS residual values) provides a visual inspection way for diagnosis of the assumption. Breusch-Pagan test (B-P test) provides a statistical way for diagnosis of the asssumption

Homoscedasticity_Checking <- function(model){
  print(bptest(model)) #B-P test states H) as satisfaction of homoscedasticity; small P-value therefore suggests heteroscedasticity of errors
} # NOTICE: Considering the LLN, the statistical examination is suggested to be conducted as a supportive diagnosis in small sample

#---------------------------------------------------------------
# CHECK4 Potential influential Observations (outliers)
# Outliers showing substantial deviation influences variance of residuals and calculation of coefficients, potentially leading to more frequent typeII error
# Cook's distance measures the influence of each observation on the regression equation. 

Outlier_Checking <- function(model){
  par(mfrow=c(1,1))
  cd <- cooks.distance(model)
  plot(cd,
       type = "h",
       main = "Cook's Distance",
       xlab = "Observations",
       ylab = "Cook's Distance")
} # The plot shwos each observation's Cook's distances. When observing values with apparently substantial different, potential serious outliers may be implied

#---------------------------------------------------------------
# CHECK 5 Multicollinearity
# A high Multicollinearity among variables suggesting a high intrinsically high relationship among predictors makes it difficult to isolate each predictor's individual effect
# Therefore, the violation leads to more noise in coeffcientcalculation with increased estimation uncertainty despite possibly well predictive accuracy
# NOTICE: multicollinearity is only checked when there is two or more predictors incorporated into the model. 

Multicollinearity_Checking <- function(model){ # Use hte function when model includes at least two predictors
  VIF_Scores <- vif(model) #VIF. When VIF > 10, a problem of multicollinearity is possibly implied
  Tolerance <- 1/vif(model) #Tolerance; Reciprocal of VIF. When tolerance < 0.1, a problem of multicollinearity is possibly implied
  VIF_and_Tolerance <- cbind(VIF_Scores = VIF_Scores,
                             Tolerance = Tolerance)
  round(VIF_and_Tolerance, 5)
}

#All of the above defined functions can be used in any models directly before formal data analysis for diagnosis of assumption with change of specific model as input


#=====================================================================================================================
# MODULE3: Data Analysis Aim 1: Behavioural and Psychological Contributors and Emotion Regulation Ability
#=====================================================================================================================
# SUBSET1: Relationship Between Sleep Hours and Emotion Regulation Scores
Aim1_model1 <- lm(
  Emotion_Regulation_Score ~ c_sleep_hours,
  data=.GlobalEnv$data,
  model = TRUE
) # Construction of simple linear regression model
summary(Aim1_model1)

# Scatterplot Visualization with OLS-based Regression line and confidence interval band
plot(data$c_sleep_hours,
     data$Emotion_Regulation_Score,
     xlab = "(mean-centering) Sleep Hours",
     ylab = "Emotion Regulation Scores",
     main = "Relationship between Sleep Duration and Emotion Regulation")
x_new <- seq(min(data$c_sleep_hours),
             max(data$c_sleep_hours),
             length.out=100)

CI_band <- predict(
  Aim1_model1,
  newdata=data.frame(c_sleep_hours=x_new),
  interval="confidence",
  level=0.95 # 95% confidence interval band
)

polygon(
  c(x_new, rev(x_new)),
  c(CI_band[,2], rev(CI_band[,3])),
  col= rgb(0.7, 0.7, 0.7, 0.3),
  border=NA
)
curve(
  coef(Aim1_model1)[1] + coef(Aim1_model1)[2]*x,
  add=TRUE,
  col="red",
  lwd=2
)

# Assumption Checking for model1
Residuals_Normality_Checking(Aim1_model1) # Checking for normality of errors
Linearity_Relationship_Checking(Aim1_model1) # Checking for whether relationship is linear
Homoscedasticity_Checking(Aim1_model1) # B-P test for checking homogeneity of variance
Outlier_Checking(Aim1_model1) # Checking for the existence of serious outlier

#Due to potential deviation from residual normality (S-W test shows a statistical significant result), bootstrap confidence intervals were additionally examined.
Bootstrapping_result <- Boot(Aim1_model1, R = 5000) # 5000 samples is recommended for stability
confint(Bootstrapping_result, level = 0.95) 
# Bootstrapping confidence internal still include 0 in the 95% CI; therefore, the effect is statistically insignificant

#SUBSET2: Relationship Between Stress Levels and Sleep Hours and Emotional Regulation
Aim1_model2 <- lm(
  Emotion_Regulation_Score ~ c_sleep_hours + c_Stress_Level,
  data = data,
  model = TRUE
) # Construction of the multiple linear regression model
summary(Aim1_model2)

Residuals_Normality_Checking(Aim1_model2) 
Linearity_Relationship_Checking(Aim1_model2) 
Homoscedasticity_Checking(Aim1_model2) 
Outlier_Checking(Aim1_model2) #
Multicollinearity_Checking(Aim1_model2) # Checking for multicollinearity since there are two predictors now

confint(Aim1_model2, level = .95) # 95% confidence interval for parameter estimation
# Either predictors' bootstrapping 95% CI do not exclude 0, implying that neither sleep hours nor stress levels is statistically significant independent predictors
# Therefore, there is insufficient evidence of meaningful independent effects of sleep hours and stress level (here, respectively behavioral and psychological contributors)

# 3D Space scatterplot Visualization
Three_D_visualization <- with(data,
                      scatterplot3d (Emotion_Regulation_Score ~ c_sleep_hours + c_Stress_Level,
                                     xlab = "Sleep Hours (mean-centering)", 
                                     ylab = "Stress Level (mean-centering)", 
                                     zlab = "Emotion Regulation Scores",
                                     main = "Multiple Regression of Bheavioural and Psychological Predictors of Emotion Regulation",
                                     pch = 20, 
                                     color = "blue")
)

Three_D_visualization$plane3d(Aim1_model2)  # OLS-based constructed hyperplane representing "best-fitting" solution in Multiple linear regression 

d_R_Square <- summary(Aim1_model2)$adj.r.squared - summary(Aim1_model1)$adj.r.squared # Difference on adjusted R^square in comparison
Comparison <- anova(Aim1_model1,Aim1_model2) # Comparison on improvement between model indicates whether the improvement is statistically significantly 
cat("Magnitude of R^2:", round(d_R_Square, 3),
    "\nP-Value:", round(Comparison$`Pr(>F)`[2], 3)) # Output results; Magnitude and statistical significance of improvement
# Insignificance result implies that there is no improvement on model fitting with incorporation of stress level as a novel predictor
# It practically indicates that insufficient evidence of incremental explanatory value beyond the predictors incorporated in the model is provided

Moderation_model3 <- lm(Emotion_Regulation_Score ~ c_sleep_hours*c_Stress_Level, data = data) # Incorporating interaction term
summary(Moderation_model3) # Primarily focusing on statistical significance of interaction term

#END for the AIM1

#=====================================================================================================================
# MODULE4: Data Analysis Aim 2: Cognitive Performance and Psychological States
#=====================================================================================================================
# SUBSET 1: Exploratory data analysis for testing whether the three cognitive ability measurements can be compressed to one variable (e.g. general cognitive ability indicator)

# Principal Component Analysis (PCA) is conducted; PCA aims to compress the three variables with preservation of most information (total variance within each variable)
Cognitive_Measurement <- data[, c(
  "Stroop_Task_Reaction_Time",
  "N_Back_Accuracy",
  "PVT_Reaction_Time"
)]
Cognitive_Measurements_PCA <- prcomp(Cognitive_Measurement,
                   scale = TRUE) # Standardized data; the value of any single eigenvalue is theoretically maximally 3 (Three variables)
summary(Cognitive_Measurements_PCA) # Proportion of total variance (normalized expression of each eigenvalue) captured each PC and cumulative proportion of explained variance

#Scree plot for explained variance by PCs
fviz_eig(Cognitive_Measurements_PCA,
         addlabels = TRUE, # Visualization of proportion of explained variance by PCs (PVEs)
         ylim = c(0,100),
         main = "Scree Plot for PVEs"
         )

Cognitive_Measurements_Rotation <- principal(
  Cognitive_Measurement,
  nfactors = 3,
  rotate = "varimax"# Varimax rotation
)
Cognitive_Measurements_Rotation$loadings

# SUBSET 2: Relationship Between Emotion Regulation and Stroop task RT
Aim2_model1 <- lm(Stroop_Task_Reaction_Time ~ c_emotion_regulation,
                  data = .GlobalEnv$data) # NOTICE: Higher Stroop task RT means worse performance
summary(Aim2_model1)

plot(data$c_emotion_regulation,
     data$Stroop_Task_Reaction_Time,
     xlab = "Emotion Regulation Scores",
     ylab = "Stroop Task RT",
     main = "Relationship between Emotion Regulation and Stroop Task RT")
x_new <- seq(min(data$c_emotion_regulation),
             max(data$c_emotion_regulation),
             length.out=100)

#Visualization of OLS-based Regression Line and confidence interval band
CI_band <- predict(
  Aim2_model1,
  newdata=data.frame(c_emotion_regulation=x_new),
  interval="confidence",
  level=0.95 # 95% confidence interval band
)

polygon(
  c(x_new, rev(x_new)),
  c(CI_band[,2], rev(CI_band[,3])),
  col= rgb(0.7, 0.7, 0.7, 0.3),
  border=NA
)
curve(
  coef(Aim2_model1)[1] + coef(Aim2_model1)[2]*x,
  add=TRUE,
  col="red",
  lwd=2
)

#Assumptions Checking 
Residuals_Normality_Checking(Aim2_model1)
Linearity_Relationship_Checking(Aim1_model1)
Homoscedasticity_Checking(Aim2_model1)
Outlier_Checking(Aim2_model1)

Bootstrapping_result <- Boot(Aim2_model1, R = 5000) # 5000 samples is recommended for stability
confint(Bootstrapping_result, level = 0.95) 

# SUBSET 3: Relationship Between Stress Level and PVT Task Reaction Time (RT)
Aim2_model2 <- lm(PVT_Reaction_Time ~ c_Stress_Level, 
                  data = .GlobalEnv$data,
                  )
summary(Aim2_model2)

plot(data$c_Stress_Level,
     data$PVT_Reaction_Time,
     xlab = "(mean-centering) Stress Levels",
     ylab = "PVT Task RT (ms)",
     main = "Relationship between Stress Levels and PVT task Reaction Time")
x_new <- seq(min(data$c_Stress_Level),
             max(data$c_Stress_Level),
             length.out=100)

CI_band <- predict(
  Aim2_model2,
  newdata=data.frame(c_Stress_Level=x_new),
  interval="confidence",
  level=0.95 # 95% confidence interval band
)
polygon( 
  c(x_new, rev(x_new)),
  c(CI_band[,2], rev(CI_band[,3])),
  col= rgb(0.7, 0.7, 0.7, 0.3),
  border=NA
)
curve(
  coef(Aim2_model2)[1] + coef(Aim2_model2)[2]*x,
  add=TRUE,
  col="red",
  lwd=2
)

Residuals_Normality_Checking(Aim2_model2)
Linearity_Relationship_Checking(Aim2_model2)
Homoscedasticity_Checking(Aim2_model2)
Outlier_Checking(Aim2_model2)

Bootstrapping_result <- Boot(Aim2_model2, R = 5000) # 5000 samples is recommended for stability
confint(Bootstrapping_result, level = 0.95) 

# SUBSET 3: Relationship Between Stress Group and N_Back_Accuracy in Controlling for Emotion Regulation Score
Reduced_Aim2_model3 <- aov(N_Back_Accuracy ~ Stress_Group, data = data) 
summary(Reduced_Aim2_model3)
TukeyHSD(Reduced_Aim2_model3) # Additional Exploration and description

#Visualization with boxplot
boxplot(N_Back_Accuracy ~ Stress_Group,
        data = data,
        main = "N Back Accuracy by Stress Levels",
        xlab = "Stress Groups",
        ylab = "N Back Task Accuracy",
        col = "lightgray"
)

# Necessary Assumptions Checking
Residuals_Normality_Checking(Reduced_Aim2_model3)# Testing homogeneity of variance of each gorup with a linear regression version
leveneTest(N_Back_Accuracy ~ Stress_Group,
  data=data
) # More appropriate function particularly designed for testing homogeneity of variance in ANOVA
Outlier_Checking (Reduced_Aim2_model3)

# Testing for Homogeneity of slopes
Aim2_model3_Checking <- lm(N_Back_Accuracy ~ Stress_Group*c_emotion_regulation,
                           data = data)
anova(
  Reduced_Aim2_model3,
  Aim2_model3_Checking
)# Examination of statistical significance of interaction term for diagnosis of assumption (slope homogeneity)
# The assumption for valid conduction of ANCOVA is slope homogeneity becuase it ensures that the difference of adjusted means can be equal everywhere across values of the covariate

full_Aim2_model3 <- aov(N_Back_Accuracy ~ Stress_Group + c_emotion_regulation,
                        data = data) # ANCOVA model incorporating centered emotion reuglation score as a covariate
summary(full_Aim2_model3)

# Visualization of ANCOVA model 
colors <- ifelse(data$Stress_Group== "High Stress", "red",  #Nested structure of ifelse() fucntion gurantees that each treatment (stress groups) will have a corresponding color 
                 ifelse(data$Stress_Group =="Moderate Stress", "blue","brown")
                 ) #If the stress group is high stress, then the color for observations is red; Otherwise, if it is moderate stress group, the assigned color is blue; otherwise, low stress group's observations are brown

with(data, plot(
  c_emotion_regulation, N_Back_Accuracy,
  xlab = "(mean-centering) Emotion Regulation Scores", # Setting of name of x-dimension
  ylab = "N Back Task Accuracy",  
  main = "Homogeneity of Slepes Visualization",
  col = colors,
  pch = 20,
  xlim=c(min(c_emotion_regulation),
         max(c_emotion_regulation)), # Setting range for x-dimension
  ylim=c(min(N_Back_Accuracy),
         max(N_Back_Accuracy))# Setting range for y-dimension
))

#Step 3, projecting Regression Line for graphical presentation
curve (cbind (1, 0, 0, x) %*% coef(full_Aim2_model3), 
       add = TRUE, # The regression line for the baseline group is colored to be brown
       col = "brown", lwd = 3) # Slopes for low-stress group （For the 
curve (cbind (1, 1, 0, x) %*% coef(full_Aim2_model3), 
       add = TRUE, # The regression line for moderate stress group is colored to be blue
       col = "blue", lwd = 3) # Slope for moderate stress group
curve (cbind (1, 0, 1, x) %*% coef(full_Aim2_model3), 
       add = TRUE, # The regression line for high stress group is colored to be red
       col = "red", lwd = 3) # Slope for high-stress group

legend( # Assignment of names correspondingly to the three slopes
  "topleft", legend=c(
    "Low Stress",
    "Moderate Stress",
    "High Stress" # What are the names for the three slopes respectively
  ),
  col=c("brown","blue","red"),
  lwd=3
)
# the assumption of homogenity of slopes is indirectly further verified because approximate parallel slopes are observed

lsmeans(full_Aim2_model3, "Stress_Group") # Output of adjusted group means in controlling emotion regulation scores to its mean for observations

#=====================================================================================================================
# END for the Project1 Secondary Data Analysis
#=====================================================================================================================