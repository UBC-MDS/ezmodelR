# ezmodelR

[![Build Status](https://travis-ci.org/UBC-MDS/ezmodelR.svg?branch=master)](https://travis-ci.org/UBC-MDS/ezmodelR)

# Contributors
* Alexander Kleefeldt
* Sean Conley
* Tyler Roberts

# Summary

This package extends the functionality of `sklearn`/`caret`, specifically with regards to model diagnostics,
adding automation and flexibility.

One goal of this package is to provide a functional API to automate common machine learning workflows such as splitting data,
fitting and evaluating models, as well as visualizing the results to allow for easier interpretation and hyperparameter tuning.

Further, this package will address specific omissions of the `sklearn` and `caret` packages and "patch up" these omissions
by adding functionality. One example for this is the limited scope of the `score` function
in `sklearn` which only returns the unadjusted r-squared value. Here, the package will allow the
user to choose different scoring functions based on the problem at hand.


# List of Functions

1. `train_test_plot()`: Simply and easily visualize the training and validation error of your model, allowing you to spend more time protoyping and less time writing boilerplate code.

2. `score()`: Scoring class to allow a user more control over the scores used to validate their model's performance. Currently includes: accuracy, mse, specificity, sensitivity, $R^2$, and adjusted $R^2$.

3. `regularization_plot()`  Visualize the results of L1 or L2 regularization on your linear models to help determine the optimal strength of your regularization parameter.

# Usage

### regularization_plot()

The functions are straightforward to use. Appropriate sample data can be generated with,

```
X <- mtcars[-1]
Y <- data.frame(mtcars$mpg)
```

The magnitude of coefficients after L2-regularized regression can be viewed with,

```
regularization_plot("ridge", lambda=2, x=X, y=Y)
```

### Score()

For the `Score()` function, we'll use the `iris` dataset, and predict whether or not a given flower is of the `setosa` variety.

First we need to select the right data, and transform the output to represent "is setosa".
```
is_setosa <- function(x){
  if(x == "setosa"){
    return(1)
  }
  else{
    return(0)
  }
}

y <-  apply(iris['Species'], 1, is_setosa)
sum(y)
x <- iris %>% select("Sepal.Width")
```

Then, we call the `score()` function, providing it with the `caret` model we would like to train, and pass `x` and `y` to the output.

```
score('rf', 'accuracy')(x, y)
```

### train_test_plot()

To produce a plot that visualizes the training and test error when iterating over the hyperparameter "cp" of a decision tree model we can follow these steps:

** 1. Load data. Using Sonar data set as example **

```
data(Sonar)
```

** 2. Create plot using 'train_test_plot*()'**
```
train_test_plot(model = "decision_tree", score_type = "accuracy", x = Sonar[,1:60],
                y = Sonar[,61], hyperparameter = "cp", param_range = seq(0,1,.05), random_seed= 123)

```


See the vignette [here](https://github.com/UBC-MDS/ezmodelR/blob/master/vignettes/ezmodelR.Rmd) for a more detailed outline of usage.

# Installation

Installation is straightforward:

```
devtools::install_github("UBC-MDS/ezmodelR")
```

# Description of Landscape

There exists a limited ability to complete all of these tasks within both `sklearn` and `caret`, but they require user-defined functions that utilize manually
extracted data (e.g. coefficients, predictions, etc.), or only offer limited diagnostics (e.g. unadjusted R^2 scores). Users of these packages frequently find
themselves repeating the same workflow, for example, splitting the dataset, training the model, and plotting training/validation error. This package will
streamline this process.

[Here](https://github.com/UBC-MDS/ezmodel/) is a link to the Python version of this package. It implements similar tools using scikit-learn to train models.
