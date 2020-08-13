#include <Rcpp.h>
using namespace Rcpp;

// Calculating slopes and variances of them

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
  // Calculate variance of slopes
  double sumB = 0;
  double sumBSq = 0;
  double B = 0;
  int iStart = 0;

  for(int i = 0; i < N; i++){
    B = slope(timeIn, ampIn, iStart, n);

    sumB += B;
    sumBSq += B * B;
    iStart += n;
  }
  return (sumBSq - (sumB * sumB) / N) / (N - 1);
}

// [[Rcpp::export]]
double varSlopeNU(NumericVector timeIn, NumericVector ampIn, double windowLength, int N){
  // Calculate variance of slopes, non-uniform
  double sumB = 0;
  double sumBSq = 0;
  double B = 0;
  int iStart = 0;

  double tStart = timeIn[0];
  double* timeLower;
  int iEnd;
  int n;

  for(int i = 0; i < N; i++){
    tStart += windowLength;
    timeLower = std::lower_bound(timeIn.begin(), timeIn.end(), tStart);
    iEnd = timeLower-timeIn.begin() - 1;
    n = iEnd - iStart + 1;

    B = slope(timeIn, ampIn, iStart, n);

    sumB += B;
    sumBSq += B * B;
    iStart = iEnd;
  }
  return (sumBSq - (sumB * sumB) / N) / (N - 1);
}
