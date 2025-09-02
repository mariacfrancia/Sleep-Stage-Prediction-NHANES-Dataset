# Sleep-Stage Prediction Using NHANES Dataset

## Overview
This project focuses on predicting sleep disorders in adult women using the NHANES dataset (National Health and Nutrition Examination Survey). The analysis aims to identify influential predictors of sleep trouble and to optimize classification models while minimizing false negatives, which is crucial for initial healthcare screening.

**Course Project:** University project completed in a team of 5 members.

### Additional Files
- **Code:** `SleepClassification.R` — contains the full analysis and modeling code.  
- **Editing Rules:** `reglas_Edicion_nhanes.txt` — contains rules applied to clean and preprocess the dataset.

## Dataset
- **Source:** NHANES dataset available in R.
- **Population:** Adult women (ages 21–79) surveyed in 2011–2012.
- **Response Variable:** `SleepTrouble` — whether an individual has sleep problems.
- **Predictors:** Includes continuous, ordinal, and nominal variables related to demographics, health, lifestyle, and lab measurements.
- **Final Dataset:** 735 observations and 27 columns after preprocessing.

## Data Preprocessing
- **Filtering:** Removed duplicate IDs, non-relevant age groups, and redundant variables.
- **Outlier Detection:** Used skewness and boxplot/IQR-based methods, removing influential outliers.
- **Missing Data Imputation:** Applied MICE with CART method for coherent imputation of both continuous and categorical variables.
- **Correlation and Dependency Analysis:**  
  - Spearman correlation for continuous/ordinal variables.  
  - Chi-squared and Fisher tests for categorical variables.  
  - Dependent variables were managed to reduce redundancy.
- **Dummy Variables:** Created for categorical predictors when required by models.
- **Train/Test Split:** 75% train, 25% test.
- **Class Balancing:** Applied SMOTE to handle imbalance in the training set.
- **Scaling:** Rescaled variables to [-1, 1] for distance-based models (e.g., KNN, SVM).

## Modeling Approaches
Various supervised classification models were trained and evaluated using 5-fold cross-validation with sensitivity as the target metric:

1. **Logistic Regression**  
   - Stepwise variable selection and backward elimination.  
   - Sensitivity maximization through threshold adjustment.

2. **Regularization Techniques (Lasso, Ridge, Elastic Net)**  
   - Explored coefficient shrinkage and selection for 20 predictors.  
   - Trade-offs between sensitivity and overall accuracy were observed.

3. **Linear Discriminant Analysis (LDA)**  
   - Checked normality and homoscedasticity assumptions.  
   - Achieved ~62.5% accuracy with balanced sensitivity and specificity.

4. **K-Nearest Neighbors (KNN)**  
   - Optimal `k` chosen based on sensitivity.  
   - Best results with k=5: sensitivity 83.05%, AUC ~63%.

5. **Naive Bayes**  
   - Assumes predictor independence; achieved ~61% accuracy.

6. **Support Vector Machines (SVM)**  
   - Tested linear, polynomial, and radial kernels.  
   - Radial kernel improved sensitivity to ~71%.

7. **Decision Trees**  
   - Pruned tree achieved higher sensitivity (69.49%) than full tree.

8. **Ensemble Methods:**  
   - **Bagging, Random Forest, Adaboost, SGB, XGBoost**  
   - Random Forest and XGBoost showed slightly better overall performance.  
   - Adaboost achieved high sensitivity (~78%), highlighting its potential for detecting sleep problems.

## Key Predictors
Across models, the most influential variables were consistently:
- `SleepHrsNight` — number of hours slept.
- `Smoke100` — whether the individual has smoked 100+ cigarettes.
- `Height` — noted in some models but less interpretable.

## Conclusions
- No single model outperformed all others across all metrics.  
- **LDA** provided a balanced solution (~62% accuracy & AUC).  
- **Sensitivity-focused models** (KNN, SVM Radial, Adaboost) effectively detected sleep problems (~70–78% sensitivity).  
- Adjusting thresholds can reduce false negatives, critical in initial clinical screenings.  
- Sleep duration and smoking status are the most relevant predictors, aligning with clinical expectations.  
