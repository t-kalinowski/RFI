


#' Modern Fortran Interface
#'
#' @param .NAME an object of class `"NativeSymbol"`, `"NativeSymbolInfo"`, or a
#'   character string giving the name of the subroutine
#' @param ... Arguments passed on to the Fortran subroutine.
#' @param PACKAGE passed on to [`getNativeSymbolInfo()`]
#' @param DUP logical. If `TRUE` (the default), all objects passed to `...` are
#'   duplicated before being passed to the Fortran code. If `FALSE`, all objects
#'   are potentially modified in place. Use [`.dup`] to selectively duplicate
#'   items.
#'
#' @section Duplicating:
#'
#'   By default, all arguments are duplicated before being passed to the Fortran
#'   code. You may want to avoid duplicating some objects for performance
#'   reasons. If you know it is safe to skip duplicating, for example because
#'   the subroutine does not modify the object (e.g., the argument is declared
#'   in the subroutine manifest with `intent(in)`), or you know for a fact that
#'   no other references to the SEXP will be used, you can avoid duplicating the
#'   object by passing a 0-based integer vector of argument index positions to
#'   `DUP`.
#'
#' @return A list of similar structure to supplied to ..., but reflecting
#'   changes made by the Fortran code
#' @export
#' @useDynLib RFI, dot_ModernFortran
#' @examples
#' \dontrun{
#' DLL <- dyn.load("my_shared_object.so")
#' func_ptr <- getNativeSymbolInfo('my_subroutine', dll)$address
#' .ModernFortran(func_ptr, arg1, arg2)
#'
#' # only duplicate arg1, arg2 is read-only (intent(in))
#' .ModernFortran(func_ptr, arg1, arg2, DUP=0L)
#' }
.ModernFortran <- function(.NAME, ..., PACKAGE, DUP=TRUE) {
  if (!inherits(.NAME, "NativeSymbol")) {
    .NAME <- if (inherits(.NAME, "NativeSymbolInfo"))
      .NAME[["address"]]
    else
      getNativeSymbolInfo(.NAME, PACKAGE, FALSE)[["address"]]
  }
  .Call(dot_ModernFortran, .NAME, list(...), DUP)
}

