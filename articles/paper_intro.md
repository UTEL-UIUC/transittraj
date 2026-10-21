# Academic Resources

## Introduction

Today’s public transit vehicles produce a large amount of automatic
vehicle location (AVL) data. This data is very useful for planning and
performance studies, but can be noisy, error-prone, and sparse. We
present `transittraj`, an R package which cleans AVL point data and
reconstructs continuous, differentiable, monotonic, and invertible
vehicle trajectory functions.

These vignettes provide code to support the `transittraj` paper
([O’Brien and Lehe, 2026](https://arxiv.org/abs/2608.31078)), including
all data cleaning, analysis, and visualization code. Unfortunately, we
do not have permission to share the underlying data, though we encourage
the reader to replicate our analyses with their own data. The vignettes
presented here are not intended as stand-alone articles; for a
beginner-friendly introduction to `transittraj` with open sample data,
check out our tutorial vignettes
([`vignette("articles/input-data-la")`](https://obrien-ben.github.io/transittraj/articles/input-data-la.md)).

## Navigating these Vignettes

We’ve split the code used in the paper into three vignettes:

1.  [The AVL Cleaning
    Workflow](https://utel-uiuc.github.io/transittraj/articles/paper_cleaning.html).
    This vignette includes the code for the visualizations presented in
    Section 2 (Package Architecture and Workflow) and the cleaning
    results presented in Section 3 (Case Study).

2.  [Estimating Signal Performance Measures Using Reconstructed
    Trajectories](https://utel-uiuc.github.io/transittraj/articles/paper_tspms.html).
    This vignette includes all code used to estimate and visualize the
    traffic signal performance measures presented in Section 3 (Case
    Study).

3.  [Evaluating the Robustness of Reconstructed
    Trajectories](https://utel-uiuc.github.io/transittraj/articles/paper_robustness.html).
    This vignette includes all code used to quantify and visualize the
    error in reconstructed trajectories across various polling
    frequencies and interpolating methodologies, as discussed in Section
    4 (Robustness Check).

Thorough discussions of the results are presented in our paper. In
addition to the knit vignettes, the raw `Rmarkdown` files are available
at our [supplementary GitHub
repository](https://github.com/UTEL-UIUC/transittraj_paper-supp). These
include all raw code chunks, including those hidden in the knit
vignettes.
