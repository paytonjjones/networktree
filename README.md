
<!-- README.md is generated from README.Rmd. Please edit that file and run rmarkdown::render("README.Rmd") -->

Psychometric networks provide information about the statistical
relationships between observed variables. ***networktree*** is a package
for partitioning psychometric networks to reveal heterogeneity.

Consider a depression network where the nodes represent different
symptoms:

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

***networktree*** can be used to identify if this depression network is
heterogeneous depending on the sample characteristics. For instance, we
can test whether the network differs depending on participants’ marital
status.

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

We can explore multiple characteristics at once, with the more important
splitting characteristics being prioritized in the tree model.
Continuous partitioning variables (e.g., age) can be included, and
***networktree*** will automatically search for the ideal split
point(s).

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

Resources:

  - [networktree Home Page](https://paytonjjones.github.io/networktree/)
  - [Getting Started -
    Tutorial](https://paytonjjones.github.io/networktree/index.html)
  - [CRAN
    documentation](https://cran.r-project.org/web/packages/networktree/index.html)  
  - [Example
    application](https://paytonjjones.github.io/networktree/articles/returns.html)
  - [Published paper](https://osf.io/ha4cw) for a more advanced look.

To cite ***networktree***, use:

Jones, P.J., Mair, P., Simon, T., & Zeileis, A. (in press). Network
trees: A method for recursively partitioning covariance structures.
Psychometrika.
