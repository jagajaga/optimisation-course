#include "transportation_solver.h"

#include <vector>

#include <boost/throw_exception.hpp>

namespace transportation
{
   struct solver_t::implementation_t
   {
      size_t suppliers_count;
      size_t consumers_count;

      std::vector<double> supply, consume;

      std::vector<std::vector<double>> cost;
      std::vector<std::vector<double>> result;

      bool solved;

      implementation_t(std::istream & in)
         : solved(false)
      {
         in >> suppliers_count >> consumers_count;
         supply.resize(suppliers_count);
         consume.resize(consumers_count);
         cost.resize(suppliers_count, std::vector<double>(consumers_count));
         result.resize(suppliers_count, std::vector<double>(consumers_count, 0));
         for (size_t i = 0; i != suppliers_count; ++i)
         {
            in >> supply[i];
         }
         for (size_t i = 0; i != consumers_count; ++i)
         {
            in >> consume[i];
         }
         for (size_t i = 0; i != suppliers_count; ++i)
         {
            for (size_t j = 0; j != consumers_count; ++j)
            {
               in >> cost[i][j];
            }
         }
      }

      void solve()
      {
         find_initial_plan();
         // TODO
         solved = true;
      }

   private:
      void find_initial_plan()
      {
         typedef std::pair<size_t, size_t> coordinates_t;
         auto find_element = [this](coordinates_t coords)
         {
            size_t i = coords.first, j = coords.second;
            double can_supply = supply[i];
            for (size_t k = 0; k != consumers_count; ++k)
            {
               can_supply -= result[i][k];
            }
            double need_consume = consume[j];
            for (size_t k = 0; k != suppliers_count; ++k)
            {
               need_consume -= result[k][j];
            }
            if (need_consume < can_supply)
            {
               result[i][j] = need_consume;
               return coordinates_t(i, j + 1);
            }
            else
            {
               result[i][j] = can_supply;
               return coordinates_t(i + 1, j);
            }
         };

         coordinates_t element_coords(0, 0);
         while (element_coords.first < suppliers_count && element_coords.second < consumers_count)
         {
            element_coords = find_element(element_coords);
         }
      }

   };

   solver_t::solver_t(std::istream & in)
      : pimpl_(new implementation_t(in))
   {
   }

   solver_t::~solver_t()
   {
   }

   size_t solver_t::consumers_count() const
   {
      return pimpl_->consumers_count;
   }

   size_t solver_t::suppliers_count() const
   {
      return pimpl_->suppliers_count;
   }

   void solver_t::solve()
   {
      pimpl_->solve();
   }

   double solver_t::get_value(size_t i, size_t j) const
   {
      if (!pimpl_->solved)
      {
         BOOST_THROW_EXCEPTION(std::runtime_error("Not solved"));
      }

      return pimpl_->result[i][j];
   }

}
