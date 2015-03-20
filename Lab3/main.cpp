#include "transportation_solver.h"

#include <iostream>
#include <fstream>
#include <iterator>
#include <set>

#include <boost/range/algorithm/copy.hpp>

int main(int argc, char ** argv)
{
   if (argc < 2)
   {
      std::cerr << "Please provide input file as first argument" << std::endl;
      return EXIT_FAILURE;
   }

   std::ifstream in(argv[1]);
   if (in.bad() || in.fail())
   {
      std::cerr << "Could not open input file" << std::endl;
      return EXIT_FAILURE;
   }
   in.exceptions(std::ios::failbit | std::ios::badbit);

   transportation::solver_t transportation_solver(in);
   transportation_solver.solve();
   transportation::print_result(transportation_solver, std::cout);

   auto angle_points = transportation_solver.angle_points();
   std::set<transportation::solver_t::point_t> angle_points_set(angle_points.begin(), angle_points.end());
   std::cout << "Angle points count = " << angle_points_set.size() << std::endl;
   //for (transportation::solver_t::point_t const & angle_point : angle_points_set)
   //{
      //boost::copy(angle_point, std::ostream_iterator<double>(std::cout, " "));
      //std::cout << std::endl;
   //}

   return EXIT_SUCCESS;
}
