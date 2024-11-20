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

inline int keep_inside(const int& M, const int& i) {
  return (i % M + M) % M;
}

// [[Rcpp::export]]
arma::mat iterate_slime(arma::mat canvas,
                        arma::mat agents,
                        const double& decay_factor,
                        const double& forward_left,
                        const double& forward_right,
                        const double& rotation_angle,
                        const double& sensor_offset,
                        const int& step_size,
                        const double& decomposition,
                        const int& iters) {
  const int rows = canvas.n_rows;
  const int cols = canvas.n_cols;
  const int n_agents = agents.n_rows;
  for (int iter = 0; iter < iters; ++iter){
    Rcpp::checkUserInterrupt();
    for (int i = 0; i < n_agents; ++i) {
        // Sensor
        const double x = agents.at(i, 0);
        const double y = agents.at(i, 1);
        const double theta = agents.at(i, 2);
        const double so_cos_theta = sensor_offset * cos(theta);
        const double so_sin_theta = sensor_offset * sin(theta);
        const double so_FL_cos = sensor_offset * cos(theta + forward_left);
        const double so_FL_sin = sensor_offset * sin(theta + forward_left);
        const double so_FR_cos = sensor_offset * cos(theta + forward_right);
        const double so_FR_sin = sensor_offset * sin(theta + forward_right);
        const int Fx = keep_inside(rows, (int)(round(x + so_cos_theta)));
        const int Fy = keep_inside(cols, (int)(round(y + so_sin_theta)));
        const int FLx = keep_inside(rows, (int)(round(x + so_FL_cos)));
        const int FLy = keep_inside(cols, (int)(round(y + so_FL_sin)));
        const int FRx = keep_inside(rows, (int)(round(x + so_FR_cos)));
        const int FRy = keep_inside(cols, (int)(round(y + so_FR_sin)));
        const double F = canvas.at(Fx, Fy);
        const double FL_val = canvas.at(FLx, FLy);
        const double FR_val = canvas.at(FRx, FRy);
        if (F <= FL_val || F <= FR_val) {
          if (F < FL_val && F < FR_val) {
            agents.at(i, 2) += (R::runif(0, 1) < 0.5) ? rotation_angle : -rotation_angle;
          } else if (FL_val < FR_val) {
            agents.at(i, 2) -= rotation_angle;
          } else if (FR_val < FL_val) {
            agents.at(i, 2) += rotation_angle;
          }
        }
        // Motor
        agents.at(i, 1) = keep_inside(rows, (int)round(agents.at(i, 1) + step_size * sin(agents.at(i, 2))));
        agents.at(i, 0) = keep_inside(cols, (int)round(agents.at(i, 0) + step_size * cos(agents.at(i, 2))));
        // Decomposition
        canvas.at(agents.at(i, 0), agents.at(i, 1)) += decomposition;
    }
    // Evaporate
    canvas *= (1.0 - decay_factor);
  }
  return canvas;
}
