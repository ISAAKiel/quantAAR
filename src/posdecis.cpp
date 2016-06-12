#include <Rcpp.h>
#include <cstdlib>
#include <iostream>
#include <math.h>

using namespace Rcpp;

//' position decision in relation to a set of stacked surfaces
//'
//' \code{posdec} has the purpose to make a decision about the position of individual points in relation
//' to a set of stacked surfaces. The decision is made by comparing the mean z-axis value of the four
//' horizontally closest points of a surface to the z-axis value of the point in question.
//'
//' @param crlist data.frame with the spatial coordinates of the points of interest
//' @param maplist list of data.frames which contain the points that make up the surfaces
//'
//' @return data.frame with the spatial coordinates of the points of interest and the respective position
//' information
//'
//' @examples
//' df1 <- data.frame(
//'   x = rnorm(50),
//'   y = rnorm(50),
//'   z = rnorm(50) - 5
//' )
//'
//' df2 <- data.frame(
//'   x = rnorm(50),
//'   y = rnorm(50),
//'   z = rnorm(50) + 5
//')
//'
//' lpoints <- list(df1, df2)
//'
//' maps <- kriglist(lpoints, lags = 3, model = "spherical")
//'
//' finds <- data.frame(
//'   x = c(0, 1, 0.5, 0.7),
//'   y = c(0.5, 0, 1, 0.7),
//'   z = c(-10, 10, 0, 2)
//' )
//'
//' posdec(finds, maps)
//'
//' @export
// [[Rcpp::export]]
DataFrame posdec(DataFrame crlist, List maplist){

  Function asMatrix("as.matrix");

  // create table with decision column
  SEXP cube2mid = crlist;
  NumericMatrix cube2 = asMatrix(cube2mid);
  NumericMatrix cubedec(cube2.nrow(), 4);
  // loop to deal with every layer
  for (int mp = 0; mp < maplist.size(); mp++){
    SEXP curmapmid = maplist[mp];
    NumericMatrix curmap = asMatrix(curmapmid);
    // loop to deal with every single point
    NumericVector mindistps(4);
    NumericVector mindistz(4);
    for (int pcube = 0; pcube < cube2.nrow(); pcube++) {
      double x1 = cube2(pcube, 0);
      double y1 = cube2(pcube, 1);
      // loop to determine four points with the shortest distance
      for (int p1 = 0; p1 < curmap.nrow(); p1++) {
        double x2 = curmap(p1, 0);
        double y2 = curmap(p1, 1);
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
          mindistz(id) = curmap(p1, 2);
        }

      }
      double ztemp = 0;
      for (int p3 = 0; p3 < mindistz.size(); p3++) {
        ztemp += mindistz(p3);
      }
      double zmap = ztemp/4.0;

      cubedec(pcube, 0) = cube2(pcube, 0);
      cubedec(pcube, 1) = cube2(pcube, 1);
      cubedec(pcube, 2) = cube2(pcube, 2);

      if (mp == 0 & cube2(pcube, 2) >= zmap) {
        cubedec(pcube, 3) = mp+1;
      } else if (cube2(pcube, 2) != 0 & cube2(pcube, 2) >= zmap) {
        cubedec(pcube, 3) += 1;
      }
    }
  }

  NumericVector x = cubedec(_,0);
  NumericVector y = cubedec(_,1);
  NumericVector z = cubedec(_,2);
  NumericVector pos = cubedec(_,3);

  // output
  return DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z, _["pos"] = pos);
}

//' position decision in relation to a set of stacked surfaces (for lists of data.frames)
//'
//' \code{posdeclist} works as \code{posdec} but not just for a single data.frame but for a list of
//' data.frames
//'
//' @param crlist list of data.frames with the spatial coordinates of the points of interest
//' @param maplist list of data.frames which contain the points that make up the surfaces
//'
//' @return list of data.frames with the spatial coordinates of the points of interest and the respective
//' position information
//'
//' @examples
//' df1 <- data.frame(
//'   x = rnorm(50),
//'   y = rnorm(50),
//'   z = rnorm(50) - 5
//' )
//'
//' df2 <- data.frame(
//'   x = rnorm(50),
//'   y = rnorm(50),
//'   z = rnorm(50) + 5
//')
//'
//' lpoints <- list(df1, df2)
//'
//' maps <- kriglist(lpoints, lags = 3, model = "spherical")
//'
//' hexadf1 <- data.frame(
//'   x = c(0, 1, 0, 4, 5, 5, 5, 5),
//'   y = c(1, 1, 4, 4, 1, 1, 4, 4),
//'   z = c(1, 5, 1, 6, 1, 5, 1, 3)
//' )
//'
//' hexadf2 <- data.frame(
//'   x = c(0, 1, 0, 4, 5, 5, 5, 5),
//'   y = c(1, 1, 4, 4, 1, 1, 4, 4),
//'   z = c(-1, -5, -1, -6, -1, -5, -1, -3)
//' )
//'
//' cx1 <- fillhexa(hexadf1, 0.1)
//' cx2 <- fillhexa(hexadf2, 0.1)
//'
//' cubelist <- list(cx1, cx2)
//'
//' posdeclist(cubelist, maps)
//'
//' @export
// [[Rcpp::export]]
List posdeclist(List crlist, List maplist){

  Function asMatrix("as.matrix");

  // loop to deal with every cube/square
  for (int crp = 0; crp < crlist.size(); crp++){
    // create table for current cube with decision column
    SEXP cube2mid = crlist[crp];
    NumericMatrix cube2 = asMatrix(cube2mid);
    NumericMatrix cubedec(cube2.nrow(), 4);
    // loop to deal with every layer for the current cube
    for (int mp = 0; mp < maplist.size(); mp++){
      SEXP curmapmid = maplist[mp];
      NumericMatrix curmap = asMatrix(curmapmid);
      // loop to deal with every single point of the current cube
      NumericVector mindistps(4);
      NumericVector mindistz(4);
      for (int pcube = 0; pcube < cube2.nrow(); pcube++) {
        double x1 = cube2(pcube, 0);
        double y1 = cube2(pcube, 1);
        // loop to determine four points with the shortest distance
        for (int p1 = 0; p1 < curmap.nrow(); p1++) {
          double x2 = curmap(p1, 0);
          double y2 = curmap(p1, 1);
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
            mindistz(id) = curmap(p1, 2);
          }

        }
        double ztemp = 0;
        for (int p3 = 0; p3 < mindistz.size(); p3++) {
          ztemp += mindistz(p3);
        }
        double zmap = ztemp/4.0;

        cubedec(pcube, 0) = cube2(pcube, 0);
        cubedec(pcube, 1) = cube2(pcube, 1);
        cubedec(pcube, 2) = cube2(pcube, 2);

        if (mp == 0 & cube2(pcube, 2) >= zmap) {
          cubedec(pcube, 3) = mp+1;
        } else if (cube2(pcube, 2) != 0 & cube2(pcube, 2) >= zmap) {
          cubedec(pcube, 3) += 1;
        }
      }
    }

    NumericVector x = cubedec(_,0);
    NumericVector y = cubedec(_,1);
    NumericVector z = cubedec(_,2);
    NumericVector pos = cubedec(_,3);

    // output
    crlist[crp] = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z, _["pos"] = pos);
  }

  // output
  return crlist;
}