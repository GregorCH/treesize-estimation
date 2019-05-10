#! /bin/bash
#
# compute forecast errors on large data set, concatenate them, and visualize them.
#
./save_forecast_errors.R

bash -c "cd tables ; mv *p_u_* uniform/"
bash -c "cd tables ; mv *p_k_* nonuniform/"

./combine_forecast_errors.R
./makeplots.R
