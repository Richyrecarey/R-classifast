# classifast

The package `classifast` is a side project that my friend [Manuel](https://github.com/MVaamonde "Manuel's GitHub profile") and I are starting. Basicaly, the main goal is to make a function that, given multivariate labeled data, gives you a quick overview of which classifiers are the best for your problem (based on several error criterious).

This minimal package is intended to be a project to both enhance our R skills and to make (hopefully) a usefull tool for Data Scientists. All help and feedback will be highly appreciated, so feel free to contact me on <richyrecarey@gmail.com> to get involved in the project.

## Instalation

`instal.packages(devtools)` <br />
`devtools::install_github("Richyrecarey/classifast")` <br />



## Usage

Given multivariate data in a matrix-like objet `x` (where rows are observations and cols variables) and its correct labels in a vector-like object `y`, the main function `classifast()` will train the desired statistical classifiers selected in the vector `method`. Several options are:


* `method = "log"`: It will train binary or multinomial logistic regression, as needed. <br />
* `method = "svm"`: It will train Radial SVM. <br />
* `method = "simple"`: It's the same as c("log", "svm") <br />
* More methods are under development <br />

Once trained, `classifast()` will return an object of class `classifast`, which has several methods implemented. For example:

```python
s = "Python syntax highlighting"
print s
```



