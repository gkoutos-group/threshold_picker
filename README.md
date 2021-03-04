# Threshold Picker

[![DOI](https://zenodo.org/badge/334302729.svg)](https://zenodo.org/badge/latestdoi/334302729)

Author: Victor Roth Cardoso - V.RothCardoso@bham.ac.uk

E-mail me suggestions, comments and issues.

## Introduction

This is a prototype to evaluate the different threshold points for an AUC curve. It is also available at https://vroth.shinyapps.io/thresholdloss/.

It also shows some different curves usually taken into consideration to make a decision, and as you change the threshold of the model the reference points are shown in the different plots.

## How it works

Risk models give a risk between 0 and 1, or between 0 and 100% of the data sample reaching a condition. This number provides some information, but it is not directly applied to a model. It is necessary to select a cut-off threshold, a value that will separate between the data points that will be marked as being predicted a cases or controls. When selecting this threshold we will define the point that we take actions or not.

### Costs

Whenever a data sample is predicted to belong to a set, there is an associated penalty, or cost. A patient that has a condition without a follow-up may be affected by other co-occurring comorbidities that may severely affect their life expectancy. A similar effect may occur for a patient wrongly predicted as going to develop a condition, these patients may be going the stress of a treatment that is not required, or they take a medicine that increase the risks of developing side-effects. These penalties can be measured as lifetime cost, a numeric penalty for each wrong/right prediction and depending if the data sample or not a condition.

In a similar effect, positive predictions will require further investigations, treatments or any sort of intervention that involve some limited resources or financial costs. These costs can be either to the data point, the health-care provider or insurance. The case of limited resources can be the number of beds available in the hospital, or limited schedule of a clinician, or the budget of a centre.

The costs could be obtained from different ways. It is suggest hypothesizing using different values, inserting some values to get started. For a more complete analysis, it is suggested to consider the different branches (or consequences) that come out of having a condition. For example, a patient with undiagnosed atrial fibrillation might have a stroke, and that is much more serious implications to a patient life-expectancy. Similarly, the costs of having follow-up investigations to identify a false positive, or unrequired complex invasive procedures.

### Input file

You can upload your model output file. This file must be a .csv file with comma-separated values. 
The files requires a column with the `true label` of each data point (the classes must be translated to 0 or 1) and a `prediction score` column with values between 0 and 1.

