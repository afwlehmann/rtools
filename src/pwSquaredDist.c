/*
 * pwSquaredDist.c
 * copyright (c) by Alexander Lehmann <afwlehmann@googlemail.com>
 */

#include <R.h>
#include <Rdefines.h>

/**
 * Squared distance of each pair of row vectors of the given
 * matrix `X` with dimensions `rows` x `cols`. The result is
 * stored in `d` which must be a vector of length N*(N-1)/2
 * where N = `rows`.
 */
void pwSquaredDist(double *X, int *rows, int *cols, double *d) {
    int i, k;
    double *dd, m, aux;
    register int base, j;

    base = 0;
    for (k = 0; k < *cols; ++k) {
        dd = d;
        for (i = 0; i < *rows; ++i) {
            m = X[base + i];
            for (j = i+1; j < *rows; ++j) {
                aux = X[base + j] - m;
                *dd++ += aux * aux;
            }
        }
        base += *rows;
    }
}
