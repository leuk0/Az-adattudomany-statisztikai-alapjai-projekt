# install.packages(c("tidyverse", "foreign", "corrplot", "factoextra"))
# install.packages("plotly")

library(tidyverse)
library(foreign)    # Needed because of ARFF file format
library(corrplot)   # Needed to visualize correlation matrix
library(factoextra) # Needed for 2D visualization for PC1 - PC2 and PC1 - PC3
library(MASS)       # Needed to use LDA
library(ggplot2)    # Needed for LDA visualization
# library(plotly)   # Needed for 3D visualization after PCA for PC1, PC2 and PC3

# The 'messidor_features.arff' file is downloadable from the following dataset: 
# https://archive.ics.uci.edu/dataset/329/diabetic+retinopathy+debrecen
data <- read.arff("messidor_features.arff")


# quality : 
#   The binary result of quality assessment. 0 = bad quality 1 = sufficient 
#   quality.
# 
# pre_screening : 
#   The binary result of pre-screening, where 1 indicates severe retinal 
#   abnormality and 0 its lack.
# 
# ma1 - ma6 : 
#   The results of MA detection. Each feature value stand for the number of MAs 
#   found at the confidence levels alpha = 0.5, . . . , 1, respectively.
# 
# exudate1 - exudate8 :
#   exudate1 - exudate8 contain the same information as 2-7) for exudates. 
#   However, as exudates are represented by a set of points rather than the 
#   number of pixels constructing the lesions, these features are normalized by 
#   dividing the number of lesions with the diameter of the ROI to compensate 
#   different image sizes.
# 
# macula_opticdisc_distance	:
#   The euclidean distance of the center of the macula and the center of the 
#   optic disc to provide important information regarding the patient's condition. 
#   This feature is also normalized with the diameter of the ROI.
# 
# opticdisc_diameter : 
#   The diameter of the optic disc.
# 
# am_fm_classification : 
#   The binary result of the AM/FM-based classification.
#   
# Class : 
#   Class label. 1 = contains signs of DR 
#   Accumulative label for the Messidor classes 1, 2, 3), 0 = no signs of DR.
colnames(data) <- c(
  "quality",
  "pre_screening",
  "ma1",
  "ma2",
  "ma3",
  "ma4",
  "ma5",
  "ma6",
  "exudate1",
  "exudate2",
  "exudate3",
  "exudate4",
  "exudate5",
  "exudate6",
  "exudate7",
  "exudate8",
  "macula_opticdisc_distance",
  "opticdisc_diameter",
  "am_fm_classification",
  "Class"
)


# Convert target variable to factor for classification methods
data$Class <- factor(data$Class,
                     levels = c(0, 1),
                     labels = c("No_DR", "DR"))


# Check column types (Class must be 'Factor'), also the first 6 rows
# str(data)
# head(data)


# Class distribution in exact numbers
table(data$Class)

# Class distribution in percentage
prop.table(table(data$Class))

# Basic descriptive statistics for numerical variables
summary(data[, -which(names(data) == "Class")])


# 'num_data' is the same as 'data', but without the target (Class)
num_data <- data[, -which(names(data) == "Class")]

# Correlation analysis to examine relationships between variables
cor_matrix <- cor(num_data)

# Strong correlations indicate multicollinearity
corrplot(cor_matrix,
         method = "color",
         tl.cex = 0.7)


# Principal Component Analysis with standardization
pca <- prcomp(num_data, scale. = TRUE)
summary(pca)

# Elbow point should be 3 or 4 (for this dataset)
# Elbow suggests using the first 3 principal components
plot(pca, type = "l", main = "Scree plot")


# 2D visualization of the first two principal components
fviz_pca_ind(pca,
             axes = c(1, 2), # PC1 and PC2
             geom.ind = "point",
             col.ind = data$Class,
             addEllipses = TRUE,
             legend.title = "Class",
             title = "Individuals - PC1 and PC2")

# Visualization including the third principal component 
# (meaning the first and third)
fviz_pca_ind(pca,
             axes = c(1, 3), # PC1 and PC3
             geom.ind = "point",
             col.ind = data$Class,
             addEllipses = TRUE,
             legend.title = "Class",
             title = "Individuals - PC1 and PC3")

# 3D visualization for PC1, PC2 and PC3
# pca_scores <- as.data.frame(pca$x[, 1:3])
# pca_scores$Class <- data$Class
# plot_ly(pca_scores,
#         x = ~PC1,
#         y = ~PC2,
#         z = ~PC3,
#         color = ~Class,
#         type = "scatter3d",
#         mode = "markers")


# Linear Discriminant Analysis for class separation (without PCA)
lda_model <- lda(Class ~ ., data = data)
lda_model

# Classification
lda_pred <- predict(lda_model)
# str(lda_pred)


# Classification performance of LDA
# LDA confusion matrix (without PCA)
# 
#                  |  Actual No_DR   |   Actual DR     |
# Predicted No_DR  | True Negative   | False Negative  |
# Predicted DR     | False Positive  | True Positive   |
table(Predicted = lda_pred$class,
      Actual = data$Class)


# New dataframe to visualize distribution of the discriminant function for LDA
lda_df <- data.frame(
  LD1 = lda_pred$x[,1],
  Class = data$Class)

# Distribution of the discriminant function for LDA (without PCA)
ggplot(lda_df, aes(x = LD1, fill = Class)) +
  geom_density(alpha = 0.5) +
  labs(title = "LDA - Distribution of the discriminant function")


# pca <- prcomp(num_data, scale. = TRUE) | Did this already
# PCA (PC1-PC3) as a dataframe
pca_scores <- as.data.frame(pca$x[, 1:3])

# PCA with traget Class
pca_scores$Class <- data$Class


# LDA applied on PCA-reduced feature space (PC1-PC3)
lda_pca_model <- lda(Class ~ ., data = pca_scores)
lda_pca_model

# Classification
lda_pca_pred <- predict(lda_pca_model)
# str(lda_pca_pred)

# Classification performance of LDA (with PCA)
# LDA confusion matrix (with PCA)
# 
#                  |  Actual No_DR   |   Actual DR     |
# Predicted No_DR  | True Negative   | False Negative  |
# Predicted DR     | False Positive  | True Positive   |
table(Predicted = lda_pca_pred$class,
      Actual = pca_scores$Class)


# New dataframe to visualize distribution of the discriminant function for LDA
lda_pca_df <- data.frame(
  LD1 = lda_pca_pred$x[,1],
  Class = pca_scores$Class
)

# Distribution of the discriminant function for LDA (with PCA)
ggplot(lda_pca_df, aes(x = LD1, fill = Class)) +
  geom_density(alpha = 0.5) +
  labs(title = "LDA with PCA - Distribution of the discriminant function")


# QDA applied after PCA due to high dimensionality
qda_pca_model <- qda(Class ~ ., data = pca_scores)
qda_pca_model

qda_pca_pred <- predict(qda_pca_model)
# str(qda_pca_pred)

# Classification performance of QDA (with PCA)
# QDA confusion matrix (with PCA)
# 
#                  |  Actual No_DR   |   Actual DR     |
# Predicted No_DR  | True Negative   | False Negative  |
# Predicted DR     | False Positive  | True Positive   |
table(Predicted = qda_pca_pred$class,
      Actual = pca_scores$Class)

# New dataframe to visualize Posterior probability
qda_pca_df <- data.frame(
  Prob_DR = qda_pca_pred$posterior[, "DR"],
  Class = pca_scores$Class
)

# Posterior probability distribution for DR class
ggplot(qda_pca_df, aes(x = Prob_DR, fill = Class)) +
  geom_density(alpha = 0.5) +
  labs(title = "QDA with PCA - Posterior probability of DR")
