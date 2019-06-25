Test environments
-----------------

-   local x86\_64, mingw32, R 3.6.0
-   Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0
-   win-builder (devel and release)

R CMD check results
-------------------

There were no ERRORs or WARNINGs.

There was 1 NOTE:

-   checking CRAN incoming feasibility … NOTE

    Maintainer: ‘Hoon Hwangbo
    <a href="mailto:hhwangb1@utk.edu" class="email">hhwangb1@utk.edu</a>’

    New submission

    Possibly mis-spelled words in DESCRIPTION: Cabezon (22:31) Hwangbo
    (22:6)

The authors’ comments:

-   This is the first submission of this package, and the names in
    DESCRIPTION are spelled correctly.

Downstream dependencies
-----------------------

There are currently no downstream dependencies for this package.

Optional comment (from the second submission)
---------------------------------------------

From the previous submission, we have received the following comments.

-   Most of your examples are wrapped in and hence not tested. Please
    unwrap the examples if that is feasible and if they can be executed
    in \< 5 sec for each Rd file or create additionally small toy
    examples.
-   Please ensure that your functions do not write by default or in your
    examples/vignettes/tests in the user’s home filespace.
-   Please do not change the working directory in your functions without
    ensuring that the user’s old working directory is reset by
    on.exit().

The authors’ comments:

-   We have unwrapped all the examples by simplifying testing
    environments.
-   We have removed all internal writing functionalities.
-   We have modified the package so that it never changes working
    directory.

Optional comment (from the first submission)
--------------------------------------------

From the previous submission, we have received the following comments.

-   Please add all authors and copyright holders in the
    <a href="mailto:Authors@R" class="email">Authors@R</a> field with
    the appropriate roles.
-   You write information messages to the console that cannot be easily
    suppressed… Instead of print()/cat() rather use message()/warning()
    if you really have to write text to the console.
-   We see all your examples are commented out. Please never do that!
    Ideally find toy examples that can be regularly executed and
    checked. Lengthy examples (\> 5 sec), can be wrapped in .
-   If there are references describing the methods in your package,
    please add these in the description field of your DESCRIPTION file.

The authors’ comments:

-   We have added all copyright holders in the
    <a href="mailto:Authors@R" class="email">Authors@R</a> field as
    suggested.
-   The purpose of information messages was to indicate the current
    state of code running. If the package is used to analyze real data
    from industry, it can take quite a long time, so such a message is
    necessary. We have reduced the amount of messages and also used
    message() instead of cat().
-   We have wrapped lengthy examples instead of commenting them out.
-   We have added an arXiv document in the description field of the
    DESCRIPTION file.
