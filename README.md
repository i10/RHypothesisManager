# Hypothesis Manager: An RStudio Plugin to Support Exploratory Programming

## Installation Instructions

1. Install the `devtools` package via CRAN. To do this, open an R session, and then type

```install.packages("devtools")```

2. Load the package.

```library(devtools)```

3. To install the Hypothesis Manager plugin, type

```install_github("i10/RHypothesisManager")```
   
Installation is complete!

## How to Use Hypothesis Manager

* Hypothesis Manager is available under the "Add-in" list at the top of RStudio. Clicking on it will open a viewer pane (usually located on the right) in RStudio. 
* The addin captures the source code of the R file that is currently open and active. You can use this while writing source code or to view your past source code files.

# Architectural Overivew
The main entrypoint is `./R/HypothesisManager.R`, providing the definition of the [addin](https://rstudio.github.io/rstudioaddins/) and glueing together the parser that analyzes the code (`./R/parser.R`) and the [custom widget](https://www.htmlwidgets.org/develop_intro.html) for the tree (`./inst/htmlwidgets/HypothesisManager.js`).

As syntactic sugar, the package [`zeallot`](https://github.com/r-lib/zeallot) is used to simplify unpacking function returns containing multiple values.