curso R
================
Val F. Lanza
24/10/2016

VARIABLES R (y la mayoria de lenguajes de programacion) tienen distintos tipos de "variables". Variables simples: Numero, Letra, Palabra... Variables complejas: Vector, Lista, Matriz, Tabla....

Indepentdientemente del tipo de variables todas ellas se "guardan"" de la misma manera.

``` r
A = 100
B = 'H'
C = 'Hola Mundo'

A
```

    ## [1] 100

``` r
B
```

    ## [1] "H"

``` r
C
```

    ## [1] "Hola Mundo"

Y se puede usar tanto el simbolo '=' como '&lt;-'

``` r
A <- 100
B <- 'H'
C <- 'Hola Mundo'
```

Las variables se usan para realizar operaciones matematicas (y no matematicas)

``` r
A*A
```

    ## [1] 10000

``` r
A-A
```

    ## [1] 0

``` r
A/A
```

    ## [1] 1

``` r
A+A
```

    ## [1] 200

Ademas de las variables existen las funciones. Las funciones son operaciones mas o menos complejas que se realizan sobre las variables.

``` r
sqrt(A)
```

    ## [1] 10

``` r
log10(A)
```

    ## [1] 2

Las funciones siempre se definen de la misma forma. nombre(atributos). Las funciones se pueden anidar, es decir hacer una funcion dentro de otra funcion.

``` r
log10(sqrt(10))
```

    ## [1] 0.5

Hasta aqui R funciona como una calculadora avanzada. Lo interesante son las variables de tipos complejos. El mas simple: un vector. Un vector es un conjunto de variables organizados en una unica dimension.

``` r
A = c(1,2,3,5,7,11,13,17)
A
```

    ## [1]  1  2  3  5  7 11 13 17

``` r
B = 1:100
B
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
    ##  [18]  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
    ##  [35]  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
    ##  [52]  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
    ##  [69]  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
    ##  [86]  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100

Los vectores nos permiten acceder a una variable a traves de su posicion usando los corchetes \[\]

``` r
A[3]
```

    ## [1] 3

O a un subconjunto de sus variables

``` r
A
```

    ## [1]  1  2  3  5  7 11 13 17

``` r
A[3:6]
```

    ## [1]  3  5  7 11

Tambien podemos hacer comparaciones logicas

``` r
A > 5
```

    ## [1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE

Y podemos anidarlas

``` r
A[A>5]
```

    ## [1]  7 11 13 17

Por supuesto podemos hacer vectores que no sean numericos

``` r
B = c("Hola","a","todos")
B
```

    ## [1] "Hola"  "a"     "todos"

``` r
B[2]
```

    ## [1] "a"

La siguiente variable compleja es la matrix. Una matriz es un vector con dimension 2 (n x m) en donde n y m no tiene porque ser iguales.

``` r
A = matrix(1:100, nrow = 10)
A
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]    1   11   21   31   41   51   61   71   81    91
    ##  [2,]    2   12   22   32   42   52   62   72   82    92
    ##  [3,]    3   13   23   33   43   53   63   73   83    93
    ##  [4,]    4   14   24   34   44   54   64   74   84    94
    ##  [5,]    5   15   25   35   45   55   65   75   85    95
    ##  [6,]    6   16   26   36   46   56   66   76   86    96
    ##  [7,]    7   17   27   37   47   57   67   77   87    97
    ##  [8,]    8   18   28   38   48   58   68   78   88    98
    ##  [9,]    9   19   29   39   49   59   69   79   89    99
    ## [10,]   10   20   30   40   50   60   70   80   90   100

``` r
A = matrix(1:100, nrow = 20)
A
```

    ##       [,1] [,2] [,3] [,4] [,5]
    ##  [1,]    1   21   41   61   81
    ##  [2,]    2   22   42   62   82
    ##  [3,]    3   23   43   63   83
    ##  [4,]    4   24   44   64   84
    ##  [5,]    5   25   45   65   85
    ##  [6,]    6   26   46   66   86
    ##  [7,]    7   27   47   67   87
    ##  [8,]    8   28   48   68   88
    ##  [9,]    9   29   49   69   89
    ## [10,]   10   30   50   70   90
    ## [11,]   11   31   51   71   91
    ## [12,]   12   32   52   72   92
    ## [13,]   13   33   53   73   93
    ## [14,]   14   34   54   74   94
    ## [15,]   15   35   55   75   95
    ## [16,]   16   36   56   76   96
    ## [17,]   17   37   57   77   97
    ## [18,]   18   38   58   78   98
    ## [19,]   19   39   59   79   99
    ## [20,]   20   40   60   80  100

Igual que los vectores, puedo acceder a los elementos individualmente mediante el uso de los \[\]

``` r
A[10,4]
```

    ## [1] 70
