
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RFI: R to Modern Fortran Interface

<!-- badges: start -->

<!-- badges: end -->

This R package provides `.ModernFortran()`, an interface similar to
`.Fortran()` but for Fortran 2018.

The 2018 Fortran language standard expanded support for interfacing C
and Fortran. One of the additions is the introduction of *C
descriptors*, a data structure for passing arrays between C and Fortran.
This package takes advantage of that.

In contrast with `.Fortran`, R arrays are not passed as naked pointers,
but as *C descriptors* that contain information about rank, shape,
element size, type, and memory stride of the array that the Fortran
routine can access directly. This means that additional arguments for
passing the size or rank of arrays are no longer needed, which should
lead to cleaner, simpler Fortran code. Additionally, logical and raw
types are now supported directly.

Currently supported type conversions are:

| R type    | Fortran type                |
| :-------- | :-------------------------- |
| `logical` | `logical(c_bool)`           |
| `integer` | `integer(c_int)`            |
| `double`  | `real(c_double)`            |
| `complex` | `complex(c_double_complex)` |
| `raw`     | `integer(c_int8_t)`         |

## Installation

You can install RFI from github:

``` r
if(!requireNamespace("remotes")) install.packages("remote")
remotes::install_github("t-kalinowski/RFI")
```

## Example

Say you have a Fortran 2018 module with a subroutine like so:

``` f90
 module mod_cshift

   use iso_c_binding, only: c_int, c_double, c_double_complex, c_bool
   implicit none

contains

   subroutine my_subroutine(array, shift) bind(c)
      integer(c_int), intent(in out) :: array(:)
      integer(c_int), intent(in) :: shift

      array = cshift(array, shift)
   end subroutine my_subroutine

end module mod_cshift
```

(`cshift` by the way is Fortran intrinsic for **circular shift**)

From R you can compile it like so:

``` r
f_module_file <- "mod_my_fortran_module.f90"
so_file <- sub("f90$", "so", f_module_file)

compile_cmd <- sprintf(
  "gfortran -std=f2018 -shared -lgfortran -lm -lquadmath %s -o %s",
  f_module_file, so_file)

system(compile_cmd)
```

(you could also use the official R pathway to creating shared objects),
with something like:

``` r
compile_cmd <- sprintf(
  "PKG_FFLAGS=-std=f2018 R CMD SHLIB %s -o %s",
  f_module_file, so_file)
system(compile_cmd)
```

Once the shared object is made, use it from R like so:

``` r
# load the fortran module
dll <- dyn.load(so_file)

# get C pointer to the Fortran subroutine
func_ptr <- getNativeSymbolInfo('my_subroutine', dll)$address

# call the subroutine with R arrays
RFI::.ModernFortran(func_ptr, array=1:5, shift=2L)
#> $array
#> [1] 3 4 5 1 2
#> 
#> $shift
#> [1] 2
```

Just like the other interfaces to compiled code (`.Fortran`,`.Call`,
etc), you’ll probably want to wrap this in an R function for convince
and input type coercion.

``` r

cshift <- function(array, shift) {
  RFI::.ModernFortran(func_ptr, 
                      array = as.integer(array), 
                      shift = as.integer(shift))$array
}

cshift(1:10, -3)
#>  [1]  8  9 10  1  2  3  4  5  6  7
```

For the most part, the interface to `.ModernFortran` tries to match that
of `.Fortran`. One area where `.ModernFortran` deviates a little is in
regards to duplicating objects. A mechanism is provided for selectively
duplicating only some of the SEXP objects. For example, we know the
subroutine above only modifies the first argument, `array`, and does not
modify the second argument, `shift`. We can use that and skip
duplicating `shift` and only duplicate `array` like so.

``` r
cshift <- function(array, shift) {
  RFI::.ModernFortran(
    func_ptr,
    array = if (typeof(array) == "integer") RFI::.dup(array) else as.integer(array),
    shift = as.integer(shift),
    DUP = FALSE
  )$array
}
cshift(1:10, 3)
#>  [1]  4  5  6  7  8  9 10  1  2  3
```

Tested with gfortran 9.2 and 9.3, R 3.6.3 and R 4.0.
