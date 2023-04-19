# Kaggle_Playground_S3E12

## Approach to stacking

Create a (large) number of (different) recipes and models. Pick out the ones that perform well (enough). 

Note that because there is so little data, the validation strategy has changed from using train/test split and CV, to using only repeated CV with the entire training data. 

Note: the current experiment tracking setup does not really work with a stacked model