# trouBBlme4SolveR v0.1

* Initial release

# trouBBlme4SolveR v0.1.1

* `minqa` library added to 'Suggests' field in DESCRIPTION file to solve NOTE on CRAN Check Results, and as explicitly required at section **2.5 Cross-references** of *Writing R Extensions*:
    > Packages referred to by these ‘other forms’ should be declared in the `DESCRIPTION` file, in the '`Depends`', '`Imports`', '`Suggests`' or '`Enhances`' fields. 
* Add a README.md file
* Fix examples in documentation for datasets

# trouBBlme4solver v0.1.2

* `lme4` specified in links to `merMod-class` through documentation files to solve NOTE on CRAN Check Results, and as explicitly required at section **2.5 Cross-references** of *Writing R Extensions*:
    > There are two other forms with an optional ‘anchor’ argument, specified as `\link[pkg]{foo}` and `\link[pkg:bar]{foo}`, to link to topics *foo* and *bar* respectively in the package **pkg**.
* Update README.md file
* Add a new argument to `dwmw` function which will allow to prioritise incrising the `nAGQ` value over updating model start parameters when both possibilities are available.
