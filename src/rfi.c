#define R_NO_REMAP
#include <ISO_Fortran_binding.h>
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>

typedef CFI_CDESC_T(CFI_MAX_RANK) CFI_cdesc_anyrank_t;


CFI_cdesc_anyrank_t as_c_descriptor(SEXP x) {
  CFI_rank_t rank = 0;

  SEXP x_dim = Rf_getAttrib(x, R_DimSymbol); // GET_DIM(x)
  int *dims;
  int x_len = LENGTH(x);
  if (x_dim != R_NilValue) {
    rank = (CFI_rank_t)LENGTH(x_dim);
    dims = INTEGER(x_dim);
  } else {
    rank = x_len > 1;
    dims = &x_len;
  }

  // extent is ignored for rank == 0
  CFI_index_t extent[rank];

  for (CFI_rank_t r = 0; r < rank; r++)
    extent[r] = (CFI_index_t)dims[r];

  CFI_cdesc_anyrank_t desc = {0};
  if (rank == 0) {
    /*
     Technically, initializing the c descriptor to 0 above is not compliant with
     the fortran language standard, since descriptors should only be modified
     and created through the CFI_* macros + functions. However, in gfortran 9.2
     and 9.3, passing of scalars (rank 0) c descriptors appears to be broken;
     this is a hack around that where we use the allocated memory for the struct
     to instead store a pointer to the R scalar value. Later in the call we take
     a pointer to the struct and cast it as a pointer to CFI_cdesc_t, but that
     doesn't seem to impact how the value is read on the fortran side, since the
     fortran subroutine manifest is what's consulted to get the pointer type.
    */
    desc = *((CFI_cdesc_anyrank_t *)DATAPTR(x));
    return desc;
  }

  CFI_type_t type;
  switch (TYPEOF(x)) {
  case LGLSXP: {
    type = CFI_type_Bool;
    /*
     make a cdescripter that is strided view of the underlying R (int)logical array,
     looking only at the first byte of each int. the only cdescriptor that can be a fortran logical
     is for a C Bool array, so this is the best way short of doing a full copy. Many fortran
     routines will likely do a copy-in copy-out anyway since the cdescriptor describes an array that
     is not contiguous.
     TODO: FIXME: don't assume endiannes
    */

    CFI_index_t lower[rank + 1], upper[rank + 1], stride[rank + 1], new_extent[rank + 1];
    new_extent[0] = 4;
    stride[0] = lower[0] = upper[0] = 0;

    for (CFI_rank_t r = 1; r <= rank; r++) {
      lower[r] = 0;
      new_extent[r] = extent[r - 1];
      upper[r] = extent[r - 1] - 1;
      stride[r] = 1;
    }

    CFI_CDESC_T(rank + 1) tmp_cdesc;
    CFI_establish((CFI_cdesc_t *)&tmp_cdesc, DATAPTR(x), CFI_attribute_other, type, 0, rank + 1,
                  new_extent);
    CFI_establish((CFI_cdesc_t *)&desc, 0, CFI_attribute_other, type, 0, rank, extent);
    CFI_section((CFI_cdesc_t *)&desc, (CFI_cdesc_t *)&tmp_cdesc, lower, upper, stride);

    return (desc);
  }
  // case STRSXP: {
  //   type = CFI_type_char;
  //   if (x_len != 1)
  //     Rf_error("Only character vectors of length 1 supported");

  //   // CHAR() is meant to be read-only.
  //   // How to enforce the fortran routine does not modify?
  //   void *data = CHAR(STRING_ELT(x, 0));

  // }
  case INTSXP: {
    type = CFI_type_int;
    break;
  }
  case REALSXP: {
    type = CFI_type_double;
    break;
  }
  case CPLXSXP: {
    type = CFI_type_double_Complex;
    break;
  }
  case RAWSXP: {
    // TODO: assert that int8_t is same size as Rbyte
    // TODO: should we use c_char here instead?
    type = CFI_type_int8_t;
    break;
  }
  default:
    Rf_error("Unsupported type");
  }

  CFI_establish((CFI_cdesc_t *)&desc, DATAPTR(x), CFI_attribute_other, type, 0, rank, extent);
  return desc;
}

void call_fortran_subroutine(const DL_FUNC fsub, const unsigned nargs, CFI_cdesc_anyrank_t args[]);

SEXP dot_ModernFortran(SEXP fsub_sexp, SEXP args, SEXP dup_s) {
  DL_FUNC fsub = R_ExternalPtrAddrFn(fsub_sexp);
  unsigned nargs = LENGTH(args);

  SEXP retval = args;
  unsigned n_protected = 0;
  switch (TYPEOF(dup_s)) {
  case LGLSXP: {
    if (Rf_asLogical(dup_s)) {
      retval = PROTECT(Rf_duplicate(retval));
      n_protected++;
    }
    break;
  }
  case INTSXP: {
    unsigned n_dups = LENGTH(dup_s);
    int *di = INTEGER(dup_s);
    for (unsigned n = n_dups; n; n--) {
      // no need to PROTECT since we're assigning right away to a protected list
      SET_VECTOR_ELT(args, *di, Rf_duplicate(VECTOR_ELT(args, *di)));
      di++;
    }
    break;
  }
  default:
    Rf_error("Argument passed to dup must be a scalar logical or an integer vector");
  }

  CFI_cdesc_anyrank_t *fargs = (CFI_cdesc_anyrank_t *)R_alloc(nargs, sizeof(CFI_cdesc_anyrank_t));
  for (unsigned i = 0; i < nargs; i++)
    fargs[i] = as_c_descriptor(VECTOR_ELT(retval, i));

  call_fortran_subroutine(fsub, nargs, fargs);

  if (n_protected)
    UNPROTECT(n_protected);
  return (retval);
}

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
  CALLDEF(dot_ModernFortran, 2),
  {NULL, NULL, 0}};

void R_init_RFI(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

void call_fortran_subroutine(const DL_FUNC fsub, const unsigned nargs, CFI_cdesc_anyrank_t args[]) {
  switch (nargs) {
  case 0:
    fsub();
    break;
  case 1:
    fsub((CFI_cdesc_t *)&(args[0]));
    break;
  case 2:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]));
    break;
  case 3:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]));
    break;
  case 4:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]));
    break;
  case 5:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]));
    break;
  case 6:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]));
    break;
  case 7:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]));
    break;
  case 8:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]));
    break;
  case 9:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]));
    break;
  case 10:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]));
    break;
  case 11:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]));
    break;
  case 12:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]));
    break;
  case 13:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]));
    break;
  case 14:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]));
    break;
  case 15:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]));
    break;
  case 16:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]));
    break;
  case 17:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]));
    break;
  case 18:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]));
    break;
  case 19:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]));
    break;
  case 20:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]));
    break;
  case 21:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]));
    break;
  case 22:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]));
    break;
  case 23:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]));
    break;
  case 24:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]));
    break;
  case 25:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]));
    break;
  case 26:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]));
    break;
  case 27:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]));
    break;
  case 28:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]));
    break;
  case 29:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]));
    break;
  case 30:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]));
    break;
  case 31:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]));
    break;
  case 32:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]));
    break;
  case 33:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]));
    break;
  case 34:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]));
    break;
  case 35:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]));
    break;
  case 36:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]));
    break;
  case 37:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]));
    break;
  case 38:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]));
    break;
  case 39:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]));
    break;
  case 40:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]));
    break;
  case 41:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]));
    break;
  case 42:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]));
    break;
  case 43:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]));
    break;
  case 44:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]));
    break;
  case 45:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]));
    break;
  case 46:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]));
    break;
  case 47:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]));
    break;
  case 48:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]));
    break;
  case 49:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]));
    break;
  case 50:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]));
    break;
  case 51:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]));
    break;
  case 52:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]));
    break;
  case 53:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]));
    break;
  case 54:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]));
    break;
  case 55:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]));
    break;
  case 56:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]));
    break;
  case 57:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]));
    break;
  case 58:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]));
    break;
  case 59:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]));
    break;
  case 60:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]), (CFI_cdesc_t *)&(args[59]));
    break;
  case 61:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]), (CFI_cdesc_t *)&(args[59]),
         (CFI_cdesc_t *)&(args[60]));
    break;
  case 62:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]), (CFI_cdesc_t *)&(args[59]),
         (CFI_cdesc_t *)&(args[60]), (CFI_cdesc_t *)&(args[61]));
    break;
  case 63:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]), (CFI_cdesc_t *)&(args[59]),
         (CFI_cdesc_t *)&(args[60]), (CFI_cdesc_t *)&(args[61]), (CFI_cdesc_t *)&(args[62]));
    break;
  case 64:
    fsub((CFI_cdesc_t *)&(args[0]), (CFI_cdesc_t *)&(args[1]), (CFI_cdesc_t *)&(args[2]),
         (CFI_cdesc_t *)&(args[3]), (CFI_cdesc_t *)&(args[4]), (CFI_cdesc_t *)&(args[5]),
         (CFI_cdesc_t *)&(args[6]), (CFI_cdesc_t *)&(args[7]), (CFI_cdesc_t *)&(args[8]),
         (CFI_cdesc_t *)&(args[9]), (CFI_cdesc_t *)&(args[10]), (CFI_cdesc_t *)&(args[11]),
         (CFI_cdesc_t *)&(args[12]), (CFI_cdesc_t *)&(args[13]), (CFI_cdesc_t *)&(args[14]),
         (CFI_cdesc_t *)&(args[15]), (CFI_cdesc_t *)&(args[16]), (CFI_cdesc_t *)&(args[17]),
         (CFI_cdesc_t *)&(args[18]), (CFI_cdesc_t *)&(args[19]), (CFI_cdesc_t *)&(args[20]),
         (CFI_cdesc_t *)&(args[21]), (CFI_cdesc_t *)&(args[22]), (CFI_cdesc_t *)&(args[23]),
         (CFI_cdesc_t *)&(args[24]), (CFI_cdesc_t *)&(args[25]), (CFI_cdesc_t *)&(args[26]),
         (CFI_cdesc_t *)&(args[27]), (CFI_cdesc_t *)&(args[28]), (CFI_cdesc_t *)&(args[29]),
         (CFI_cdesc_t *)&(args[30]), (CFI_cdesc_t *)&(args[31]), (CFI_cdesc_t *)&(args[32]),
         (CFI_cdesc_t *)&(args[33]), (CFI_cdesc_t *)&(args[34]), (CFI_cdesc_t *)&(args[35]),
         (CFI_cdesc_t *)&(args[36]), (CFI_cdesc_t *)&(args[37]), (CFI_cdesc_t *)&(args[38]),
         (CFI_cdesc_t *)&(args[39]), (CFI_cdesc_t *)&(args[40]), (CFI_cdesc_t *)&(args[41]),
         (CFI_cdesc_t *)&(args[42]), (CFI_cdesc_t *)&(args[43]), (CFI_cdesc_t *)&(args[44]),
         (CFI_cdesc_t *)&(args[45]), (CFI_cdesc_t *)&(args[46]), (CFI_cdesc_t *)&(args[47]),
         (CFI_cdesc_t *)&(args[48]), (CFI_cdesc_t *)&(args[49]), (CFI_cdesc_t *)&(args[50]),
         (CFI_cdesc_t *)&(args[51]), (CFI_cdesc_t *)&(args[52]), (CFI_cdesc_t *)&(args[53]),
         (CFI_cdesc_t *)&(args[54]), (CFI_cdesc_t *)&(args[55]), (CFI_cdesc_t *)&(args[56]),
         (CFI_cdesc_t *)&(args[57]), (CFI_cdesc_t *)&(args[58]), (CFI_cdesc_t *)&(args[59]),
         (CFI_cdesc_t *)&(args[60]), (CFI_cdesc_t *)&(args[61]), (CFI_cdesc_t *)&(args[62]),
         (CFI_cdesc_t *)&(args[63]));
    break;
  default:
    Rf_error("too many arguments");
  }
}

/*
# build body of call_fortran_subroutine
library(glue);library(magrittr)
seq_len0 <- function(i) seq.int(0, i-1)
build_args <- function(i) {
  paste0(
    sprintf("(CFI_cdesc_t *)&(args[%i])", seq_len0(i)),
    collapse = ",")
}

build_case_branch <- function(i) {
  glue("
case {i}:
  fsub({build_args(i)});
  break;
")
}

lapply(seq_len0(65), build_case_branch) %>%
  unlist() %>% paste0(collapse = '\n') %>%
  clipr::write_clip()
*/
