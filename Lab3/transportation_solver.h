#pragma once

#include <iostream>
#include <memory>

namespace transportation
{
   struct solver_t
   {
      solver_t(std::istream &);
      ~solver_t();

      size_t consumers_count() const;
      size_t suppliers_count() const;

      void solve();
      double get_value(size_t i, size_t j) const;

      size_t iterations_count() const;

   private:
      struct implementation_t;
      std::unique_ptr<implementation_t> pimpl_;
   };

   void print_result(solver_t const &, std::ostream &);
}
