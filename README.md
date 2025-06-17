# Classifying Depression Among University Students with Logistic Regression

## Overview

This repository is a fork of a group project I took part in for the **PSTAT 100: Data Science Concepts and Analysis** course at UC Santa Barbara. The main goal is to use **logistic regression** to identify and evaluate key predictors of depression among university students using a dataset obtained from OpenML.

We investigated how factors like **dietary habits**, **sleep duration**, and **general life stressors** (academic, work-related, financial) influence the likelihood of depression. The analysis is driven by hypothesis testing and supported by statistical modeling.

## Repository Structure

```
├── data/
│   ├── student_depression_dataset.csv
│   └── student_depression_dataset_Codebook
├── final_deliverable/
│   ├── final_deliverable.qmd
│   └── final_deliverable.pdf
├── README.md
```

## Team Members

* Valerie De La Fuente (@valeriedelafuente)
* Matthew Arteaga (@matthewarteaga)
* Phuc Lu (@pdlu)
* William Nelson (@williamnelson)
* Hayden Galletta (@haydengalletta)

## Research Questions

1. Do certain **dietary habits** coincide with increased rates of depression?
2. Is there a **correlation between sleep duration** and the proportion of students reporting depression?
3. Does the presence and magnitude of **life stressors** significantly affect depression likelihood?

## Dataset

* **Filename**: `student_depression_dataset.csv`
* **Codebook**: `student_depression_dataset_Codebook`
* **Observations**: 27,901 students
* **Variables**: 18 (including demographics, mental health history, academic performance, lifestyle factors, and depression status)
* **Source**: [OpenML Dataset #46753](https://www.openml.org/search?type=data&status=active&id=46753&sort=runs)

## Methodology

### Data Cleaning

* Renamed and standardized variable names
* Converted categorical fields to factors
* Removed invalid or missing entries
* Combined `academic_pressure`, `work_pressure`, and `financial_stress` into `total_stress`

### Modeling

Three logistic regression models were developed, one for each hypothesis:

1. **Dietary Habits Model**

   * Statistically significant (p < 2.2e-16)
   * Unhealthy diets increased depression odds by 88%
   * Model accuracy \~61%

2. **Sleep Duration Model**

   * Significant negative association with depression
   * AUC = 0.5494 (weak predictive power)

3. **Total Stress Model**

   * Strongest predictor of depression
   * Odds nearly double for each unit increase in stress
   * Model accuracy = 75.65%

## Results Summary

* All three tested variables (diet, sleep, stress) were statistically significant predictors of student depression.
* The **total stress** model had the highest classification accuracy.
* Data is representative of Indian university students; results may not generalize globally.

## Final Deliverable

The complete project report is located in the `/final_deliverable` folder:

* `final_deliverable.qmd`: Quarto source file
* `final_deliverable.pdf`: Rendered report with full analysis, figures, and conclusions

## Tools & Technologies

* **R** (tidyverse, broom, caret, ggplot2)
* **RStudio**
* **Quarto** (for reproducible reporting)
* **Git** (for version control)
