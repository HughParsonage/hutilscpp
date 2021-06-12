2021-06-13 Hugh Parsonage

## Moving from Rcpp to C

I recently updated `hutilscpp`, a package for my compiled code. The 
package was refactored to be entirely in C, rather than written in C++
and using Rcpp for the heavy-lifting. This aide-ménoire summarizes the
motivation behind the change and the fun and pain of the transition.

### Motivation

#### Install time

`hutilscpp` is a package containing compiled code. Its brother, 
`hutils` is a package containing zero compiled code. The original 
motivation of `hutils` was to harness some useful functions in 
other packages (notably `dplyr`) but without incurring the install
time of those packages and their dependencies. At the time, I had 
a particular common task that involved spinning up a virtual machine, 
installing R and executing a small script.  I noticed that the 
script itself took about 30s but than install times could easily 
extend to 15 minutes.  When I saw that I was using 
the turbocharged `dplyr::if_else`
for a subsecond performance boost, I realized I was suffering
more turbo lag than turbo boost, and rewrote it, and restricted `hutils` 
package dependencies with overall install time in mind.

Because I already had a package with a lean installation,
when it come time to microdose with compiled time, I wrote
`hutilscpp` with the opposite tradeoff between runtime and 
install time performance.  Using Rcpp made this really easy,
and I actually found it easier to write than R.
As my use of compiled code became more than just a social 
vice, both the install time and install size of `hutilscpp`
started to grow. I believe the problem is particularly acute
on Windows, the OS I use.

This became acute when I was doing some COVID-19 modelling with 
Rcpp. While writing the compiled code was easy, 
the model
development pace was high, and as the package was updated frequently
the install time became a serious problem, even though the runtime performance
not have been achievable with R alone.  Particularly acute was code coverage.
While wanting 100 per cent code coverage is probably a weakness of mine, 
I still have the marks from times when a single untested line has bitten me.
Using `covr` with compiled code works brilliantly but effectively meant 
the install time doubled for every change, even if it was a single 
line with a wrong side failure, or a typo in a test suite. In some cases,
I actually forgot which line I was meant to be covering.

Reducing the install time of `hutilscpp` was the primary challenge. I noticed that packages
with C tended to have lower install times. I was also not deaf to remarks in
R Core that the C API was a bit more 'official' and enduring.

#### Learning more about R

I'm comfortable with my level of expertise in R, but one particularly
obvious gap is my fluency in C. The fact that R is written in C means
that understanding how particular functions work, or why bugs are present
means understanding the C source. The urge to understand the R source, as
well as some developing some competence the C language, was another 
motivating factor.


### The experience of 2c 
Luckily, the code in `hutilscpp` is pretty damn simple. (One 
particularly egregious example of this is `xor2(x, y)` which was 
a bit faster than base R's `xor(x, y)` for logical `x, y`, 
though my triumph was undercut when I realized later that `x != y` 
was even faster and didn't involve a nest of case statements.)

Further, Rcpp had taught me plenty about compiled code that C 
wasn't a huge challenge. It remains my recommendation for anyone
wanting to improve the runtime performance.

#### Pointers

> Young man, in C you don't understand pointers. You just get used to them.
> _John von Neumann\0











