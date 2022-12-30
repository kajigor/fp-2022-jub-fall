# Performance

Down you can find performance on several functions with precisions from 0.1 to 1e-6. I measured effectiveness of integral evaluation by the number of segments needed to achieve some fixed precision. Code for measurements can be found in app/AnalizeIntegration.hs. To run use `stack run analize`.

## Analitics

As expected Middle Point and Trapezoid Methods both give us similar results, close to $O(1/n^2)$. And Simpson Method is much more efficient on all used integrals, showing results close to $O(1/n^4)$.

## Measurements:

2*x+1/sqrt(x+1/16)
0.1
Middle Point Method: 9
Trapezoid Method: 16
Simpson Method: 8
1.0e-2
Middle Point Method: 81
Trapezoid Method: 64
Simpson Method: 32
1.0e-3
Middle Point Method: 243
Trapezoid Method: 256
Simpson Method: 64
1.0e-4
Middle Point Method: 729
Trapezoid Method: 512
Simpson Method: 128
1.0e-5
Middle Point Method: 2187
Trapezoid Method: 2048
Simpson Method: 256
1.0e-6
Middle Point Method: 6561
Trapezoid Method: 8192
Simpson Method: 512

cos(x/2)*x
0.1
Middle Point Method: 9
Trapezoid Method: 8
Simpson Method: 4
1.0e-2
Middle Point Method: 27
Trapezoid Method: 32
Simpson Method: 8
1.0e-3
Middle Point Method: 81
Trapezoid Method: 128
Simpson Method: 16
1.0e-4
Middle Point Method: 729
Trapezoid Method: 256
Simpson Method: 32
1.0e-5
Middle Point Method: 2187
Trapezoid Method: 1024
Simpson Method: 32
1.0e-6
Middle Point Method: 6561
Trapezoid Method: 4096
Simpson Method: 64

x^5-7*x^4+4*x^3-20*x^2+17*x+3
0.1
Middle Point Method: 59049
Trapezoid Method: 32768
Simpson Method: 512
1.0e-2
Middle Point Method: 177147
Trapezoid Method: 131072
Simpson Method: 1024
1.0e-3
Middle Point Method: 531441
Trapezoid Method: 524288
Simpson Method: 1024
1.0e-4
Middle Point Method: 1594323
Trapezoid Method: 1048576
Simpson Method: 2048
1.0e-5
Middle Point Method: 4782969
Trapezoid Method: 4194304
Simpson Method: 4096
1.0e-6
Middle Point Method: 14348907
Trapezoid Method: 16777216
Simpson Method: 8192

e^(3*x/5)+log(x/4)/x
0.1
Middle Point Method: 59049
Trapezoid Method: 32768
Simpson Method: 512
1.0e-2
Middle Point Method: 177147
Trapezoid Method: 131072
Simpson Method: 1024
1.0e-3
Middle Point Method: 531441
Trapezoid Method: 524288
Simpson Method: 2048
1.0e-4
Middle Point Method: 1594323
Trapezoid Method: 1048576
Simpson Method: 2048
1.0e-5
Middle Point Method: 4782969
Trapezoid Method: 4194304
Simpson Method: 4096
1.0e-6
Middle Point Method: 14348907
Trapezoid Method: 16777216
Simpson Method: 8192