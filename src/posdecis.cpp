#include <Rcpp.h>
#include <cstdlib>
#include <iostream>
#include<math.h>

using namespace Rcpp;

NumericMatrix catmatrix(NumericMatrix a, NumericMatrix b) {
  int acoln = a.ncol();
  int bcoln = b.ncol();
  NumericMatrix out = no_init_matrix(a.nrow(), acoln + bcoln);
  for (int j = 0; j < acoln + bcoln; j++) {
    if (j < acoln) {
      out(_, j) = a(_, j);
    } else {
      out(_, j) = b(_, j - acoln);
    }
  }
  return out;
}

int vecmax(NumericVector x) {
  NumericVector::iterator it = std::max_element(x.begin(), x.end());
  return it - x.begin();
}

//' Test
//'
//' @param crlist
//' @param maplist
//'
//' @export
// [[Rcpp::export]]
List posdec(List crlist, List maplist){

  Function asMatrix("as.matrix");

  // loop to deal with every cube/square
  for (int crp = 0; crp < crlist.size(); crp++){
    // create table for current cube with decision column
    SEXP cube2mid = crlist[crp];
    NumericMatrix cube2 = asMatrix(cube2mid);
    NumericMatrix cubedec = NumericMatrix(cube2.nrow(), 1);
    // loop to deal with every layer for the current cube
    for (int mp = 0; mp < maplist.size(); mp++){
      SEXP curmapmid = maplist[mp];
      NumericMatrix curmap = asMatrix(curmapmid);
      // loop to deal with every single point of the current cube
      NumericVector mindistps(4);
      NumericVector mindistz(4);
      for (int pcube = 0; pcube < cube2.nrow(); pcube++) {
        double x1 = cube2(pcube, 1);
        double y1 = cube2(pcube, 2);
        // loop to determine four points with the shortest distance
        for (int p1 = 0; p1 < curmap.nrow(); p1++) {
          double x2 = curmap(p1, 1);
          double y2 = curmap(p1, 2);
          double x = x1 - x2;
          double y = y1 - y2;
          double dist = pow(x, 2) + pow(y, 2);
          dist = sqrt(dist);

          if (p1 == 0){
            mindistps(0) = dist;
            mindistps(1) = dist;
            mindistps(2) = dist;
            mindistps(3) = dist;
          }

          double max = 0;
          int id = 0;
          for (int p2 = 0; p2 < 4; p2++) {
            if(mindistps(p2) > max) {
              max = mindistps(p2);
              id = p2;
            }
          }

          if (dist < mindistps(id)) {
            mindistps(id) = dist;
            mindistz(id) = curmap(p1, 3);
          }

        }
        double zmap = sum(mindistz)/4;
        if (cube2(pcube, 3) < zmap) {
          cubedec(pcube, 1) = mp;
        } else {
          cubedec(pcube, 1) = mp+1;
        }
      }
    }
    crlist[crp] = catmatrix(cube2, cubedec);
  }

  // output
  return crlist;
}