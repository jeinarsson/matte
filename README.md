# Matte

Symbolic Cartesian tensor algebra in Mathematica using pattern matching.

## Example

<img src="doc/fig/screenshot.png" alt="Example code" width="600"/>


## Getting started

Matte is only a few hundred lines of relatively simple code, and I explain the entire implementation in a series of tutorial notebooks. Following through the tutorial will get you started with doing calculations.

Clone this repository, or [download the master branch as a .zip](https://github.com/jeinarsson/matte/archive/master.zip) to get all files mentioned below.

### Tutorials

I explain the entire basic implementation, step by step, in three tutorial notebooks. 

1. tutorial01-derivatives.nb representation and differentiation. 
2. tutorial02-contract.nb covers contraction and simplification.
3. tutorial03-symmetries.nb covers handling of dummy indices and other tensor symmetries. 

The seemingly arbitrary order is for pedagogical reasons - in order of increasing complexity.

After these three tutorials the essential code is finished. The two following tutorials implement new functionality on top of the basics, and illustrate how to extend and modify the code to suit your needs.

4. In tutorial04-integrals.nb we implement integration over spherical volumes and surfaces.
5. In tutorial05-fourier-transform.nb we implement the 3D Fourier transform of certain tensorial forms. This demonstrates how to add custom rules to the system, and deal with complicated algebraic manipulations.

### Examples

The examples have less explanatory text than the tutorials, but are more extensive examples of how I work with the method.

In example-sphere-in-sof-shear.nb I reproduce parts of the calculation in [(Einarsson & Mehlig, PRF 2017) (open access)](https://doi.org/10.1103/PhysRevFluids.2.063301). I compute the mobility of a sedimenting sphere in a second-order fluid shear flow by two methods: 1) evaluating the reciprocal integral theorem, and 2) calculating the flow velocity and stress fields and integrating them over the particle surface. 

In example-drop-in-quadratic-flow.nb I derive the flow velocity and stress fields inside and outside a spherical viscous drop suspended in a quadratic viscous flow. See e.g. [(Chan & Leal, JFM 1970)](http://dx.doi.org/10.1017/S0022112079000562) for background on the problem, and [Nadim, A. & Stone, H. A. Studies in Applied Mathematics 85, 53–73 (1991)](http://dx.doi.org/10.1002/sapm199185153). The problem is very labor intensive because the boundary condition is given on the stress. Therefore, we must make an ansatz for the flow field, compute the stress associated with that ansatz, and apply the boundary conditions.

## About 

For the past years I have worked on several theoretical calculations to understand particle dynamics in viscous flows. These calculations often involve overwhelming amounts of tedious tensor algebra - contracting products, taking derivatives, applying symmetries. Over time I have developed a method of dealing with this routine algebra using the pattern-matching capabilities of Mathematica. The code helps me evaluate mathematical expressions, and allows me more time to think about the physics.

This method in various incarnations is behind much of the mathematical work in [my publications.](https://arxiv.org/find/physics/1/au:+Einarsson_J/0/1/0/all/0/1) Several of those calculations would have been prohibitively tedious without computer assistance.

The code consists of a convention for tensorial expressions on index notation, and a number of patterns and functions to manipulate these expressions according to the rules of algebra. 

The code is not meant as an encapsulated black box, and it does not solve [the general problem.](https://xkcd.com/974/) I think of it as a template that provides the fundamental functionality and methodology. In this template I can implement the patterns and rules I need for my particular calculation. The code is a shallow layer on top of Mathematica, meant to be as flexible as possible.

If you do any substantial calculations on Cartesian tensor notation I believe you'll find a large return on your invested time. After walking through the tutorial notebooks you will have a good understanding of whether this method could help you, and how to adapt the code to your specific needs. 

## To Package or Not to Package

In this repository [Matte.m](Matte.m) is the master version of the code. But I don't think of this code as a package - I use it as a template for new projects.

In practise I fork Matte.m for each new calculation so that I can hack freely on it without breaking any other calculation. Useful functions and ideas eventually make it back upstream manually. This is a consequence of the shallow hackable implementation, and while perhaps objectionable from a software engineering perspective it works well in a research environment. I don't need to care about backwards compatability, because each project is self-contained, and I can spend that effort on physics instead.

## Thanks

Kristian Gustafsson (Gothenburg University) who once upon a time got me started on the pattern matching path.

Joe Barakat (Stanford University) for inspiring me to finally clean up and release this code.

The Knut & Alice Wallenberg foundation for the generous postdoc grant KAW 2015.0419 that allowed me to work on this.

## License

Matte is Copyright 2017-2018 Jonas Einarsson and released freely under the MIT License. See [LICENSE](LICENSE).


## Contact

email: me at jonaseinarsson.se
