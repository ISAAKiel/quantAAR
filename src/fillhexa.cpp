#include <Rcpp.h>
#include <cstdlib>
#include <iostream>
#include <math.h>

using namespace Rcpp;

NumericMatrix makematrix(NumericVector vec, int lres3){
  NumericMatrix result(lres3, 3);
  for (int p1 = 0; p1 < 3; p1++) {
    for (int p2 = 0; p2 < lres3; p2++) {
      result(p2,p1) = vec(p1);
    }
  }
  return result;
}

//' Fills hexahedrons with a systematic point raster
//'
//' See \url{https://stackoverflow.com/questions/36115215/filling-a-3d-body-with-a-systematic-point-raster}
//' for a description of the function and how it was developed.
//'
//' @param hex dataframe with three columns and eight rows to define a hexahedron by its corner
//' point coordinates
//' @param res numeric value > 0 and <= 1 for the resolution of the point raster
//'
//' @return data.frame with the spatial coordinates of the resulting points of the filling raster
//'
//' @examples
//' hexatestdf <- data.frame(
//'   x = c(0,1,0,4,5,5,5,5),
//'   y = c(1,1,4,4,1,1,4,4),
//'   z = c(4,8,4,9,4,8,4,6)
//' )
//'
//' cx = fillhexa(hexatestdf, 0.1)
//'
//' #library(rgl)
//' #plot3d(
//' # cx[,1], cx[,2], cx[,3],
//' # type = "p",
//' # xlab = "x", ylab = "y", zlab = "z"
//' #)
//'
//' @export
// [[Rcpp::export]]
DataFrame fillhexa(DataFrame hex, double res){

  Function asMatrix("as.matrix");

  SEXP hex2mid = hex;
  NumericMatrix hexa = asMatrix(hex2mid);

  // check, if res is between 0 and 1

  // check, if res is 1/res != int

  // check if hexa has the right form

  // ...

  // create new coordinate system (u,v,w)
  int lres = (1/res)+1;
  NumericVector resvec(lres);
  for (int p1 = 0; p1 < lres; p1++) {
    resvec(p1) = res*p1;
  }

  int lres2 = pow(lres,2);
  int lres3 = pow(lres,3);

  // u
  NumericVector u(lres3);
  int counter1 = 0;
  int counter2 = 0;
  int p2 = 0;
  for (int p1 = 0; p1 < lres3; p1++) {
    counter1++;
    counter2++;
    u(p1) = resvec(p2);
    if (counter1 == lres){
      counter1 = 0;
      p2++;
      if (counter2 == lres2){
        counter2 = 0;
        p2 = 0;
      }
    }
  }

  // v
  NumericVector v(lres3);
  counter1 = 0;
  p2 = 0;
  for (int p1 = 0; p1 < lres3; p1++) {
    counter1++;
    v(p1) = resvec(p2);
    if (counter1 == lres2){
      counter1 = 0;
      p2++;
    }
  }

  // w
  NumericVector w(lres3);
  counter1 = 0;
  p2 = 0;
  for (int p1 = 0; p1 < lres3; p1++) {
    counter1++;
    w(p1) = resvec(p2);
    p2++;
    if (counter1 == lres){
      counter1 = 0;
      p2 = 0;
    }
  }

  // transformation
  NumericVector A = hexa(0,_);
  NumericVector B = hexa(1,_) - A;
  NumericVector C = hexa(2,_) - A;
  NumericVector D = hexa(4,_) - A;
  NumericVector E = hexa(3,_) - A - B - C;
  NumericVector F = hexa(5,_) - A - B - D;
  NumericVector G = hexa(6,_) - A - C - D;
  NumericVector H = hexa(7,_) - A - B - C - D - E - F - G;

  NumericMatrix Am = makematrix(A, lres3);
  NumericMatrix Bm = makematrix(B, lres3);
  NumericMatrix Cm = makematrix(C, lres3);
  NumericMatrix Dm = makematrix(D, lres3);
  NumericMatrix Em = makematrix(E, lres3);
  NumericMatrix Fm = makematrix(F, lres3);
  NumericMatrix Gm = makematrix(G, lres3);
  NumericMatrix Hm = makematrix(H, lres3);

  for (int p1 = 0; p1 < lres3; p1++) {
    Bm(p1,_) = Bm(p1,_) * u(p1);
    Cm(p1,_) = Cm(p1,_) * v(p1);
    Dm(p1,_) = Dm(p1,_) * w(p1);
    Em(p1,_) = Em(p1,_) * u(p1) * v(p1);
    Fm(p1,_) = Fm(p1,_) * u(p1) * w(p1);
    Gm(p1,_) = Gm(p1,_) * v(p1) * w(p1);
    Hm(p1,_) = Hm(p1,_) * u(p1) * v(p1) * w(p1);
  }

  NumericMatrix final(lres3, 3);

  for (int p1 = 0; p1 < lres3; p1++) {
    final(p1,_) = Am(p1,_) + Bm(p1,_) + Cm(p1,_) + Dm(p1,_) + Em(p1,_) + Fm(p1,_) + Gm(p1,_) + Hm(p1,_);
  }

  NumericVector x = final(_,0);
  NumericVector y = final(_,1);
  NumericVector z = final(_,2);

  // output
  return DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
}