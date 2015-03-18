#include "transportation_solver.h"

#include <iostream>
#include <fstream>

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
   for (size_t i = 0; i != transportation_solver.suppliers_count(); ++i)
   {
      std::cout << "Suppliers #" << i << " gives: " << std::endl;
      for (size_t j = 0; j != transportation_solver.consumers_count(); ++j)
      {
         std::cout << transportation_solver.get_value(i, j) << " ";
      }
      std::cout << std::endl;
   }

   return 0;
}
