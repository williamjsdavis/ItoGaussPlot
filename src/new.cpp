#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
double muRcpp(NumericVector x){
  int n = x.size(); // Size of vector
  double sum = 0; // Sum value
  // For loop, note cpp index shift to 0
  for(int i = 0; i < n; i++){
      // Shorthand for sum = sum + x[i]
      sum += x[i];
  }

  return sum/n; // Obtain and return the Mean
}

// [[Rcpp::export]]
double varRcpp(NumericVector x){
  int n = x.size();
  double sum = 0;
  double sumSq = 0;

  for(int i = 0; i < n; i++){

    sum += x[i];
    sumSq += x[i]*x[i];
  }
  return (sumSq - (sum * sum) / n) / (n - 1);
}

// [[Rcpp::export]]
double varRcpp2(NumericVector x, int a, bool bias = true){
  // Calculate the mean using C++ function
  double mean = muRcpp(x);
  double sum = 0;
  int n = x.size();
  for(int i = 0; i < n; i++){
    sum += pow(x[i] - mean, 2.0); // Square
  }
  return sum/(n-bias) + a; // Return variance
}

// [[Rcpp::export]]
double slope(NumericVector timeIn, NumericVector ampIn, int ii, int n){
  // Calculate slope
  double sumX = 0;
  double sumY = 0;
  double sumXY = 0;
  double sumXX = 0;

  for(int i = 0; i < n; i++){
    sumX += timeIn[ii];
    sumY += ampIn[ii];
    sumXY += timeIn[ii] * ampIn[ii];
    sumXX += timeIn[ii] * timeIn[ii];
    ii +=  1;
  }
  return (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX);
}

// [[Rcpp::export]]
double varSlope(NumericVector timeIn, NumericVector ampIn, int n, int N){
  // Calculate slope
  double sumB = 0;
  double sumBSq = 0;
  double B = 0;
  int ii = 0;

  for(int i = 0; i < N; i++){
    B = slope(timeIn, ampIn, ii, n);

    sumB += B;
    sumBSq += B * B;
    ii += n;
  }
  return (sumBSq - (sumB * sumB) / N) / (N - 1);
}


