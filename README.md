

This is a repository for sharing our eye-tracking project realised during our second year of master degree (2 month duration).

We are focusing on analysing gaze data to get heatmap of gaze from different users in order to make some interpretability on different machine learning algorithm to retrieve sensometric conclusions obtained during sensorial analysis.


# Script
The section script contain all scrpits required for or project.

## Requirements.R 
You can find in this file all the packages required for the project.

## 0_exploratofy_analysis
In this section, you can fin all the exploratory script that are not inclued in the "production part", this script may not be functionnal.

## 1_Data_processing

In the section you can find all scripts that refers to the data process. In that, we refer to all the function that allow us to create usable data in order to create image such as heatmaps.

Mainly, scripts takes gaze recorder data to combine them and reshape them.

## 2_heatmap

In this section, you can find scripts that refers to the heatmap generation. It can be fake heatmaps, or function that help us to create heatmaps.

Generally, scripts use processed data.

## 3_ML_interpretability

In this section you can find machine learning and interpretability analysis.

We used CNN from Keras for image recognition and LIME package for the interpretability.

# Experience

Here you can find all our file relative to our experience part (experimental design, correspondance file, presented slide to consumers, etc). 

# Data

## inputs_ml

In the image section, you can find image (mainly heatmaps) we used as input for our machine learning algorithm.

## Gaze data
Here you can find all data collected from the software Gaze Recorder.

## df_all.RData

df_all.RData is an R dataFrame with all the collected and processed data, ready to use.



Project conduct by Antoine LUCAS @antoinelucasfra, Flavie THEVENARD @FlavThvnrd and Julien PETOT @jpetot

