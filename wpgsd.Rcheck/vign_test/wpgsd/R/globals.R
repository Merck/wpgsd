# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the wpgsd program.
#
# wpgsd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

utils::globalVariables(
  unique(
    c(
      # From `calc_seq_p()`
      c("analysis"),
      # From `closed_test()`
      c("Analysis"),
      # From `generate_bounds()`
      c("Analysis", "Hypotheses"),
      # From `generate_corr()`
      c("H1", "H2", "Analysis", "Event")
    )
  )
)
