#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector centraliseCoordinates(NumericVector x) {

   //Calculate centroid
  double avgX = std::accumulate(x.begin(),x.end(),0)/x.size();

  //Modify in place subtract avg from vector.
  for (auto& i : x)
    i -= avgX;

  //Output dataframe via rcpp
  return x;

}



/*** R
centraliseCoordinates(c(rep(5,5),1))
*/
