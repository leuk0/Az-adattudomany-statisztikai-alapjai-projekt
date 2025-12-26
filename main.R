library(tidyverse)
library(foreign) # Needed because of ARFF file format

# The 'messidor_features.arff' file is downloadable from the following dataset: 
# https://archive.ics.uci.edu/dataset/329/diabetic+retinopathy+debrecen
data <- read.arff("messidor_features.arff")


# quality : 
#   The binary result of quality assessment. 0 = bad quality 1 = sufficient quality.
# 
# pre_screening : 
#   The binary result of pre-screening, where 1 indicates severe retinal abnormality and 0 its lack.
# 
# ma1 - ma6 : 
#   The results of MA detection. Each feature value stand for the number of MAs 
#   found at the confidence levels alpha = 0.5, . . . , 1, respectively.
# 
# exudate1 - exudate8 :
#   exudate1 - exudate8 contain the same information as 2-7) for exudates. 
#   However, as exudates are represented by a set of points rather than the 
#   number of pixels constructing the lesions, these features are normalized by 
#   dividing the number of lesions with the diameter of the ROI to compensate different image sizes.
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

data$Class <- factor(data$Class,
                     levels = c(0, 1),
                     labels = c("No_DR", "DR"))

