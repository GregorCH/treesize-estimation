#! /bin/bash
#
# compute forecast errors on large data set, concatenate them, and visualize them.
#
./save_forecast_errors.R

bash -c "cd tables ; mkdir uniform ; mv *p_u_* uniform/"

./combine_forecast_errors.R
./makeplots.R
