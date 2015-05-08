# KaggleRevenuePrediction
Predict annual restaurant sales based on objective measurements

This code is for predicting revenue for restaurants. Its based on the kaggle competition https://www.kaggle.com/c/restaurant-revenue-prediction

It uses Genetic Algorithm, Recursive feature selection to find out important variables. Then, models such as random forest, svm with radial and polynomial kernel are built. In the end an ensemble of best models is tried. Most models are built in parallel and for evaluation 10 fold cross validation with 10 repeats is used to stabilize it as the training dataset is very small.
