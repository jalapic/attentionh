### cumjoin function 

body <- '
    SEXP res;
int i, j, l, out_len = 0, len = LENGTH(lst), index=0;
double g, inc = REAL(gap)[0];
for (i = 0; i < len; i++) out_len += LENGTH(VECTOR_ELT(lst, i));
PROTECT(res = allocVector(REALSXP, out_len));
double *elem, *rval = REAL(res);

for (i = 0; i < len; i++) {
l = LENGTH(VECTOR_ELT(lst, i));
elem = REAL(VECTOR_ELT(lst, i)), 
g = i > 0 ? rval[index-1] + inc : 0.0;  // add the gap and prev max
for (j = 0; j < l; j++) rval[index++] = elem[j] + g;
}

UNPROTECT(1);
return res;'


library(inline)
cumjoin <- cfunction(signature(lst = 'list', gap = 'numeric'), body=body)
