// Copyright (C) 2021-2023 Koen Derks

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

void transform(double& x,
               double& y,
               const double& a,
               const double& b,
               const double& c,
               const double& d,
               const double& e,
               const double& f) {
  const double newx = a * x + b * y + c;
  const double newy = d * x + e * y + f;
  x = newx;
  y = newy;
}

void variation(double& x,
               double& y,
               const int& i,
               const double& a,
               const double& b,
               const double& c,
               const double& d,
               const double& e,
               const double& f,
               const Rcpp::DoubleVector& pparams) {
  if (i == 0) { // Linear
    const double newx = x;
    const double newy = y;
    x = newx;
    y = newy;
  } else if (i == 1) { // Sine
    const double newx = sin(x);
    const double newy = sin(y);
    x = newx;
    y = newy;
  } else if (i == 2) { // Sperical
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = x / pow(r, 2);
    const double newy = y / pow(r, 2);
    x = newx;
    y = newy;
  } else if (i == 3) { // Swirl
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = x * sin(pow(r, 2)) - y * cos(pow(r, 2));
    const double newy = x * cos(pow(r, 2)) + y * sin(pow(r, 2));
    x = newx;
    y = newy;
  } else if (i == 4) { // Horsehoe
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = (1 / r) * ((x - y) * (x + y));
    const double newy = (1 / r) * (2 * x * y);
    x = newx;
    y = newy;
  } else if (i == 5) { // Polar
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = theta / M_PI;
    const double newy = r - 1;
    x = newx;
    y = newy;
  } else if (i == 6) { // Handkerchief
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = r * sin(theta + r);
    const double newy = r * cos(theta - r);
    x = newx;
    y = newy;
  } else if (i == 7) { // Heart
    const double theta = atan(x / y);
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = r * sin(theta * r);
    const double newy = r * (-cos(theta * r));
    x = newx;
    y = newy;
  } else if (i == 8) { // Disc
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = theta / M_PI * sin(M_PI * r);
    const double newy = theta / M_PI * cos(M_PI * r);
    x = newx;
    y = newy;
  } else if (i == 9) { // Spiral
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = 1 / r * (cos(theta) + sin(r));
    const double newy = 1 / r * (sin(theta) + cos(r));
    x = newx;
    y = newy;
  } else if (i == 10) { // Hyperbolic
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = sin(theta) / r;
    const double newy = r * cos(theta);
    x = newx;
    y = newy;
  } else if (i == 11) { // Diamond
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = sin(theta) * cos(r);
    const double newy = cos(theta) * sin(r);
    x = newx;
    y = newy;
  } else if (i == 12) { // Ex
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double p0 = sin(theta + r);
    const double p1 = cos(theta - r);
    const double newx = r * (pow(p0, 3) + pow(p1, 3));
    const double newy = r * (pow(p0, 3) - pow(p1, 3));
    x = newx;
    y = newy;
  } else if (i == 13) { // Julia
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double s = R::runif(0, 1);
    const double Omega = (s < .05) ? 0 : M_PI;
    const double newx = sqrt(r) * cos(theta / 2 + Omega);
    const double newy = sqrt(r) * sin(theta / 2 + Omega);
    x = newx;
    y = newy;
  } else if (i == 14) { // Bent
    const double newx = ((x >= 0) && (y >= 0)) ? x : (((x < 0) && (y >= 0)) ? (2 * x) : (((x >= 0) && (y < 0)) ? x : (2 * x)));
    const double newy = ((x >= 0) && (y >= 0)) ? y : (((x < 0) && (y >= 0)) ? y : (((x >= 0) && (y < 0)) ? (y / 2) : (y / 2)));
    x = newx;
    y = newy;
  } else if (i == 15) { // Waves
    const double newx = x + b * sin(y / pow(c, 2));
    const double newy = y + e * sin(x / pow(f, 2));
    x = newx;
    y = newy;
  } else if (i == 16) { // Fisheye
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = (2 / (r + 1)) * y;
    const double newy = (2 / (r + 1)) * x;
    x = newx;
    y = newy;
  } else if (i == 17) { // Popcorn
    const double newx = x + c * sin(tan(3 * y));
    const double newy = y + f * sin(tan(3 * x));
    x = newx;
    y = newy;
  } else if (i == 18) { // Exponential
    const double newx = exp(x - 1) * cos(M_PI * y);
    const double newy = exp(x - 1) * sin(M_PI * y);
    x = newx;
    y = newy;
  } else if (i == 19) { // Power
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double newx = pow(r, sin(theta)) * cos(theta);
    const double newy = pow(r, sin(theta)) * sin(theta);
    x = newx;
    y = newy;
  } else if (i == 20) { // Cosine
    const double newx = cos(M_PI * x) * cosh(y);
    const double newy = -(sin(M_PI * x) * sinh(y));
    x = newx;
    y = newy;
  } else if (i == 21) { // Rings
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double first = fmod(r + pow(c, 2), 2 * pow(c, 2)) - pow(c, 2) + r * (1 - pow(c, 2));
    const double newx = first * cos(theta);
    const double newy = first * sin(theta);
    x = newx;
    y = newy;
  } else if (i == 22) { // Fan
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double t = M_PI * pow(c, 2);
    const double newx = fmod(theta + f, t) > (t / 2) ? r * cos(theta - (t/2)) : r * cos(theta + (t/2));
    const double newy = fmod(theta + f, t) > (t / 2) ? r * sin(theta - (t/2)) : r * sin(theta + (t/2));
    x = newx;
    y = newy;
  } else if (i == 23) { // Blob
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double first = r * (pparams[1] + ((pparams[0] - pparams[1])/ 2) * (sin(pparams[2] * theta) + 1));
    const double newx = first * cos(theta);
    const double newy = first * sin(theta);
    x = newx;
    y = newy;
  } else if (i == 24) { // PDJ
    const double newx = sin(pparams[3] * y) - cos(pparams[4] * x);
    const double newy = sin(pparams[5] * x) - cos(pparams[6] * y);
    x = newx;
    y = newy;
  } else if (i == 25) { // Fan2
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    variation(x, y, 22, a, b, c, d, e, f, pparams);
    const double p1 = M_PI * pow(x, 2);
    const double p2 = y;
    const double t = theta + p2 - p1 * trunc((2 * theta * p2) / p1);
    const double newx = t > (p1 / 2) ? r * sin(theta - (p1 / 2)) : r * sin(theta + (p1 / 2));
    const double newy = t > (p1 / 2) ? r * cos(theta - (p1 / 2)) : r * cos(theta + (p1 / 2));
    x = newx;
    y = newy;
  } else if (i == 26) { // Rings2
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double theta = atan(x / y);
    const double pp = pow(pparams[7], 2);
    const double t = r - 2 * pp * trunc((r + pp) / (2 * pp)) + r * (1 - pp);
    const double newx = t * sin(theta);
    const double newy = t * cos(theta);
    x = newx;
    y = newy;
  } else if (i == 27) { // Eyefish
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = (2 / (r + 1)) * x;
    const double newy = (2 / (r + 1)) * y;
    x = newx;
    y = newy;
  } else if (i == 28) { // Bubble
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = (4 / (pow(r, 2) + 4)) * x;
    const double newy = (4 / (pow(r, 2) + 4)) * y;
    x = newx;
    y = newy;
  } else if (i == 29) { // Cylinder
    const double newx = sin(x);
    const double newy = y;
    x = newx;
    y = newy;
  } else if (i == 30) { // Perspective
    const double first = (pparams[9] / (pparams[9] - y * sin(pparams[8])));
    const double newx = first * x;
    const double newy = first * (y * cos(pparams[8]));
    x = newx;
    y = newy;
  } else if (i == 31) { // Noise
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double newx = Psi1 * (x * cos(2 * M_PI * Psi2));
    const double newy = Psi1 * (y * sin(2 * M_PI * Psi2));
    x = newx;
    y = newy;
  } else if (i == 32) { // JuliaN
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double phi = atan(y / x);
    const double Psi = R::runif(0, 1);
    const double p3 = trunc(fabs(pparams[11]) * Psi);
    const double t = (phi + 2 * M_PI * p3) / pparams[11];
    const double newx = pow(r, pparams[11] / pparams[10]) * cos(t);
    const double newy = pow(r, pparams[11] / pparams[10]) * sin(t);
    x = newx;
    y = newy;
  } else if (i == 33) { // JuliaScope
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double phi = atan(y / x);
    const double Psi = R::runif(0, 1);
    const int Lambda = floor(R::runif(0, 2));
    const double p3 = trunc(fabs(pparams[12] * Psi));
    const double t = (Lambda * phi + 2 * M_PI * p3) / pparams[12];
    const double newx = pow(r, pparams[13] / pparams[12]) * cos(t);
    const double newy = pow(r, pparams[13] / pparams[12]) * sin(t);
    x = newx;
    y = newy;
  } else if (i == 34) { // Blur
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double newx = Psi1 * cos(2 * M_PI * Psi2);
    const double newy = Psi1 * sin(2 * M_PI * Psi2);
    x = newx;
    y = newy;
  } else if (i == 35) { // Gaussian
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double Psi3 = R::runif(0, 1);
    const double Psi4 = R::runif(0, 1);
    const double Psi5 = R::runif(0, 1);
    const double newx = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * cos(2 * M_PI * Psi5);
    const double newy = (Psi1 + Psi2 + Psi3 + Psi4 - 2) * sin(2 * M_PI * Psi5);
    x = newx;
    y = newy;
  } else if (i == 36) { // RadialBlur
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double phi = atan(y / x);
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double Psi3 = R::runif(0, 1);
    const double Psi4 = R::runif(0, 1);
    const double p1 = pparams[14] * (M_PI / 2);
    const double t1 = pparams[15] * (Psi1 + Psi2 + Psi3 + Psi4 - 2);
    const double t2 = phi + t1 * sin(p1);
    const double t3 = t1 * cos(p1) - 1;
    const double newx = (1 / pparams[15]) * (r * cos(t2) + t3 * x);
    const double newy = (1 / pparams[15]) * (r * sin(t2) + t3 * y);
    x = newx;
    y = newy;
  } else if (i == 37) { // Pie
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double Psi3 = R::runif(0, 1);
    const double t1 = trunc(Psi1 * pparams[16] + 0.5);
    const double t2 = pparams[17] + ((2 * M_PI) / pparams[16]) * (t1 + Psi2 * pparams[18]);
    const double newx = Psi3 * cos(t2);
    const double newy = Psi3 * sin(t2);
    x = newx;
    y = newy;
  } else if (i == 38) { // Ngon
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double phi = atan(y / x);
    const double p2 = 2 * M_PI / pparams[20];
    const double t3 = phi - p2 * floor(phi / p2);
    const double t4 = (t3 > (p2 / 2)) ? t3 : t3 - p2;
    const double k = (pparams[21] * (1 / cos(t4) - 1) + pparams[22]) / pow(r, pparams[19]);
    const double newx = k * x;
    const double newy = k * y;
    x = newx;
    y = newy;
  } else if (i == 39) { // Curl
    const double t1 = 1 + pparams[23] * x + pparams[24] * (pow(x, 2) - pow(y, 2));
    const double t2 = pparams[23] * y + 2 * pparams[24] * x * y;
    const double first = 1 / (pow(t1, 2) + pow(t2, 2));
    const double newx = first * (x * t1 + y * t2);
    const double newy = first * (y * t1 - x * t2);
    x = newx;
    y = newy;
  } else if (i == 40) { // Rectangles
    const double newx = (2 * floor(x / pparams[25]) + 1) * pparams[25] - x;
    const double newy = (2 * floor(y / pparams[26]) + 1) * pparams[26] - y;
    x = newx;
    y = newy;
  } else if (i == 41) { // Arch
    const double Psi = R::runif(0, 1);
    const double newx = sin(Psi * M_PI * pparams[27]);
    const double newy = pow(sin(Psi * M_PI * pparams[27]), 2) / cos(Psi * M_PI * pparams[27]);
    x = newx;
    y = newy;
  } else if (i == 42) { // Tangent
    const double newx = sin(x) / cos(y);
    const double newy = tan(y);
    x = newx;
    y = newy;
  } else if (i == 43) { // Square
    const double Psi1 = R::runif(0, 1);
    const double Psi2 = R::runif(0, 1);
    const double newx = Psi1 - 0.5;
    const double newy = Psi2 - 0.5;
    x = newx;
    y = newy;
  } else if (i == 44) { // Rays
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double Psi = R::runif(0, 1);
    const double first = (pparams[28] * tan(Psi * M_PI * pparams[28])) / pow(r, 2);
    const double newx = first * cos(x);
    const double newy = first * sin(x);
    x = newx;
    y = newy;
  } else if (i == 45) { // Blade
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double Psi = R::runif(0, 1);
    const double newx = x * (cos(Psi * r * pparams[29]) + sin(Psi * r * pparams[29]));
    const double newy = x * (cos(Psi * r * pparams[29]) - sin(Psi * r * pparams[29]));
    x = newx;
    y = newy;
  } else if (i == 46) { // Secant
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double newx = x;
    const double newy = 1 / (pparams[30] * cos(pparams[30] * r));
    x = newx;
    y = newy;
  } else if (i == 47) { // Twintrian
    const double r = sqrt(pow(x, 2) + pow(y, 2));
    const double Psi = R::runif(0, 1);
    const double t = log10(pow(sin(Psi * r * pparams[31]), 2)) + cos(Psi * r * pparams[31]);
    const double newx = x * t;
    const double newy = x * (t - M_PI * sin(Psi * r * pparams[31]));
    x = newx;
    y = newy;
  } else if (i == 48) { // Cross
    const double first = sqrt(1 / pow(pow(x, 2) - pow(y, 2), 2));
    const double newx = first * x;
    const double newy = first * y;
    x = newx;
    y = newy;
  }
}

// [[Rcpp::export]]
arma::cube cpp_flame(arma::cube& canvas,
                     const int& iterations,
                     const int& resolution,
                     const int& edge,
                     const bool& blend,
                     const bool& weighted,
                     const bool& post,
                     const bool& final,
                     const bool& extra,
                     const arma::mat& colors,
                     const Rcpp::DoubleVector& functions,
                     const Rcpp::DoubleVector& funcWeights,
                     const arma::mat& funcPars,
                     const Rcpp::DoubleVector& variations,
                     const arma::mat& varWeights,
                     const Rcpp::DoubleVector& varParams,
                     const arma::mat& postPars,
                     const Rcpp::DoubleVector& finalPars,
                     const Rcpp::DoubleVector& extraPars,
                     const int& bsym) {
  const int nvar = variations.length(), nfunc = functions.length();
  double x = R::runif(-1, 1), y = R::runif(-1, 1), c1 = R::runif(0, 1), c2 = R::runif(0, 1), c3 = R::runif(0, 1);
  const bool vary = !((nvar == 1) && (variations[0] == 0));
  for (int iter = 1; iter < iterations; ++iter) {
    if (iter % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    // Pick an affine function to use and apply to the current point
    const int i = weighted ? Rcpp::sample(functions, 1, false, funcWeights)[0] : floor(R::runif(0, nfunc));
    transform(x, y, funcPars.at(i, 0), funcPars.at(i, 1), funcPars.at(i, 2), funcPars.at(i, 3), funcPars.at(i, 4), funcPars.at(i, 5));
    // Apply variations
    if (i < bsym) { // Functions with i < bsym are affine functions, the rest is symmtry functions so we skip
      if (vary) { // Do not vary if the only affine is linear
        if (blend) { // Blend variations
          double xc = 0, yc = 0;
          for (int j = 0; j < nvar; ++j) {
            double xp = x, yp = y;
            variation(xp, yp, variations[j], funcPars.at(i, 0), funcPars.at(i, 1), funcPars.at(i, 2), funcPars.at(i, 3), funcPars.at(i, 4), funcPars.at(i, 5), varParams);
            xc += varWeights.at(i, j) * xp;
            yc += varWeights.at(i, j) * yp;
          }
          x = xc, y = yc;
        } else { // Do not blend variations
          const int j = (variations.size() == 1) ? 0 : (weighted ? Rcpp::sample(Rcpp::IntegerVector::create(0, variations.size() - 1), 1, false, Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(varWeights.row(i))))[0] : floor(R::runif(0, nvar)));
          variation(x, y, variations[j], funcPars.at(i, 0), funcPars.at(i, 1), funcPars.at(i, 2), funcPars.at(i, 3), funcPars.at(i, 4), funcPars.at(i, 5), varParams);
        }
      }
      // Apply a post transformation
      if (post) {
        transform(x, y, postPars.at(i, 0), postPars.at(i, 1), postPars.at(i, 2), postPars.at(i, 3), postPars.at(i, 4), postPars.at(i, 5));
      }
      // Apply a final transformation
      if (final) {
        transform(x, y, finalPars[0], finalPars[1], finalPars[2], finalPars[3], finalPars[4], finalPars[5]);
        // Apply an additional post transformation
        if (extra) {
          transform(x, y, extraPars[0], extraPars[1], extraPars[2], extraPars[3], extraPars[4], extraPars[5]);
        }
      }
      // Update color channels for the current iteration
      c1 = (c1 + colors.at(i, 0)) / 2;
      c2 = (c2 + colors.at(i, 1)) / 2;
      c3 = (c3 + colors.at(i, 2)) / 2;
    }
    // Update the cube data structure
    if (iter > 20) {
      const int indx = (x * resolution / (2 * edge)) + resolution / 2;
      if ((indx >= 0) && (indx < resolution)) {
        const int indy = (y * resolution / (2 * edge)) + resolution / 2;
        if ((indy >= 0) && (indy < resolution)) {
          ++canvas.at(indx, indy, 0);
          canvas.at(indx, indy, 1) += c1;
          canvas.at(indx, indy, 2) += c2;
          canvas.at(indx, indy, 3) += c3;
        }
      }
    }
  }
  return canvas;
}
