# ezmodel/ezmodelR


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

1. Visualizations to aid model/feature selection.

2. Additional scoring functions for use with `sklearn`/`caret` models (i.e. MSE ...)

3. Transformers to coerce common data types into the requisite data types to be passed to existing model objects.


# Description of Landscape

There exists a limited ability to complete all of these tasks within both `sklearn` and `caret`, but they require user-defined functions that utilize manually
extracted data (e.g. coefficients, predictions, etc.), or only offer limited diagnostics (e.g. unadjusted R^2 scores). Users of these packages frequently find
themselves repeating the same workflow, for example, splitting the dataset, training the model, and plotting training/validation error. This package will
streamline this process.
