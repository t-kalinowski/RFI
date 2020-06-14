


#' Modern Fortran Interface
#'
#' @param .NAME an object of class `"NativeSymbol"`, `"NativeSymbolInfo"`, or a
#'   character string giving the name of the subroutine
#' @param ... Arguments passed on to the Fortran subroutine.
#' @param PACKAGE passed on to [`getNativeSymbolInfo()`]
#'
#' @return A list of similar structure to  supplied to ..., but reflecting
#'   changes made by the Fortran code
#' @export
#' @useDynLib RFI, dot_ModernFortran
.ModernFortran <- function(.NAME, ..., PACKAGE) {
  if (!inherits(.NAME, "NativeSymbol")) {
    .NAME <- if (inherits(.NAME, "NativeSymbolInfo"))
      .NAME[["address"]]
    else
      getNativeSymbolInfo(.NAME, PACKAGE, FALSE)[["address"]]
  }
  .Call(dot_ModernFortran, .NAME, list(...))
}
