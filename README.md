# Library and console application for calculation of definite integrals.

In this study project I have implemented 3 methods for calculation of definite integrals over segments with fixed precision in Haskell.

Corresponding functions can be found in `src/Integration.hs`.

Besides, console application is provided. To run it use `stack run exe --` with options(see Usage of console application).

## Usage of library functions

In `src/Integration.hs` you can find 3 functions for calculation of definite integrals with some fixed precision:
- "evalIntegralMidPoint f a b eps" - implementation of Middle Point Method( $O(1/n^2)$ aproximation)
- "evalIntegralTrapezoid f a b eps" - implementation of Trapezoid Method( $O(1/n^2)$ aproximation)
- "evalIntegralSimpson f a b eps" - implementation of Simpson Method( $O(1/n^4)$ aproximation)

To use them import module Integration.

## Usage of console application

```
Usage: stack run exe -- (-f|--function STRING) (-a|--from DOUBLE) (-b|--to DOUBLE) (-e|--eps DOUBLE)
  This application evaluates definite integrals with some fixed precision.

Available options:
  -f,--function STRING     Function to be integrated.
  -a,--from DOUBLE         Start of integration segment.
  -b,--to DOUBLE           End of integration segment.
  -e,--eps DOUBLE          Precision of integration.
  -h,--help                Show this help text
 ```
 
 Example: `stack run exe -- -f 4*x*x*x-20*x*x+17*x+3 -a -13 -b 20 -e 0.0001`
