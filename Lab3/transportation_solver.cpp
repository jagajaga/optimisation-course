#include "transportation_solver.h"

#include <vector>
#include <stdexcept>
#include <cassert>
#include <iterator>
#include <functional>
#include <random>

#include <boost/range/algorithm/copy.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/throw_exception.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/operation.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <boost/optional.hpp>

namespace transportation
{
   namespace
   {
      typedef boost::numeric::ublas::matrix<double> matrix_t;
      typedef boost::numeric::ublas::identity_matrix<double> identity_matrix_t;

      /* Matrix inversion routine.
      Uses lu_factorize and lu_substitute in uBLAS to invert a matrix */
      bool invert_matrix(matrix_t const& input, matrix_t& inverse)
      {
         typedef boost::numeric::ublas::permutation_matrix<std::size_t> pmatrix;

         // create a working copy of the input
         matrix_t A(input);

         // create a permutation matrix for the LU-factorization
         pmatrix pm(A.size1());

         // perform LU-factorization
         int res = lu_factorize(A, pm);

         if (res != 0)
         {
            return false;
         }

         // create identity matrix of "inverse"
         inverse.assign(identity_matrix_t(A.size1()));

         // backsubstitute to get the inverse
         boost::numeric::ublas::lu_substitute(A, pm, inverse);

         return true;
      }

      std::vector<double> solve_linear_system(matrix_t const& matrix, matrix_t const& rhs)
      {
         size_t m = matrix.size1(),
               n = matrix.size2();

         if (rhs.size1() != m || rhs.size2() != 1)
         {
            BOOST_THROW_EXCEPTION(std::runtime_error("Invalid rhs size"));
         }

         matrix_t inverse(n, m);
         if (!invert_matrix(matrix, inverse))
            return {};

         matrix_t product(n, 1);
         boost::numeric::ublas::axpy_prod(inverse, rhs, product);
         std::vector<double> res;

         for (size_t i = 0; i != n; ++i)
         {
            res.push_back(product(i, 0));
         }

         return res;
      }

      std::vector<std::vector<size_t>> generated_ordered_combinations(size_t n, size_t k)
      {
         std::vector<std::vector<size_t>> result;
         std::function<void(std::vector<size_t>)> generate = [&](std::vector<size_t> v)
         {
            if (v.size() == k)
            {
               result.push_back(std::move(v));
               return;
            }
            for (size_t i = 0; i != n; ++i)
            {
               if (std::find(v.begin(), v.end(), i) != v.end())
                  continue;

               std::vector<size_t> new_v = v;
               new_v.push_back(i);
               generate(std::move(new_v));
            }
         };

         generate({});
         return result;
      }

      std::vector<solver_t::point_t> find_angle_points(matrix_t const & matrix, matrix_t const & rhs)
      {
         size_t m = matrix.size1(),
                n = matrix.size2();

         if (rhs.size1() != m || rhs.size2() != 1)
         {
            BOOST_THROW_EXCEPTION(std::runtime_error("Invalid rhs size"));
         }

         size_t rank = m - 1; // It seems to be true

         std::vector<solver_t::point_t> result;
         for (std::vector<size_t> subset : generated_ordered_combinations(n, rank))
         {
            assert(subset.size() == rank);
            matrix_t equations(rank, rank);
            for (size_t i = 0; i != rank; ++i)
            {
               for (size_t j = 0; j != rank; ++j)
               {
                  equations(i, j) = matrix(i, subset[j]);
               }
            }
            matrix_t new_rhs(rank, 1);
            for (size_t i = 0; i != rank; ++i)
               new_rhs(i, 0) = rhs(i, 0);

            std::vector<double> solution = solve_linear_system(equations, new_rhs);
            if (solution.empty())
               continue;

//            std::clog << "Equations: " << equations << std::endl
//                      << "new_rhs: " << new_rhs << std::endl
//                      ;
//            boost::copy(solution, std::ostream_iterator<double>(std::clog, " "));
//            std::clog << std::endl;
            std::vector<double> angle_point(n, 0);
            for (size_t i = 0; i != subset.size(); ++i)
            {
               angle_point[subset[i]] = solution[i];
            }
            result.push_back(std::move(angle_point));
         }

         return result;
      }

      size_t generate_random(size_t n)
      {
         std::uniform_int_distribution<size_t> distr(0, n - 1);
         std::mt19937 generator;
         return distr(generator);
      }

   }

   struct solver_t::implementation_t
   {
      size_t suppliers_count;
      size_t consumers_count;

      std::vector<std::vector<double>> result;

      bool solved;

      size_t iterations_count;

      implementation_t(std::istream & in)
         : solved(false)
         , iterations_count(0)
      {
         in >> suppliers_count >> consumers_count;
         supply_.resize(suppliers_count);
         consume_.resize(consumers_count);
         cost_.resize(suppliers_count, std::vector<double>(consumers_count));
         result.resize(suppliers_count, std::vector<double>(consumers_count, 0));

         for (size_t i = 0; i != suppliers_count; ++i)
         {
            in >> supply_[i];
         }

         for (size_t i = 0; i != consumers_count; ++i)
         {
            in >> consume_[i];
         }

         for (size_t i = 0; i != suppliers_count; ++i)
         {
            for (size_t j = 0; j != consumers_count; ++j)
            {
               in >> cost_[i][j];
            }
         }
      }

      void solve()
      {
         find_initial_plan();
         ++iterations_count;

         while (auto bad_coordinates = find_negative_delta())
         {
            ++iterations_count;
            improve_plan(*bad_coordinates);
         }

         solved = true;
      }

      std::vector<point_t> angle_points()
      {
         size_t m = suppliers_count,
                n = consumers_count;
         matrix_t equations(m + n, m * n);
         matrix_t rhs(m + n, 1);

         for (size_t i = 0; i != m; ++i)
         {
            for (size_t k = 0; k != m * n; ++k)
            {
               equations(i, k) = (k / n == i) ? 1 : 0;
            }

            rhs(i, 0) = supply_[i];
         }
         for (size_t j = 0; j != n; ++j)
         {
            for (size_t k = 0; k != m * n; ++k)
            {
               equations(j + m, k) = (k % n == j) ? 1 : 0;
            }

            rhs(j + m, 0) = consume_[j];
         }

         return find_angle_points(equations, rhs);
      }

   private:
      typedef std::pair<size_t, size_t> coordinates_t;

      void find_initial_plan()
      {
         std::vector<coordinates_t> visited;
         auto find_element = [this](coordinates_t coords)
         {
            size_t i = coords.first,
                   j = coords.second;
            double can_supply = supply_[i];

            for (size_t k = 0; k != consumers_count; ++k)
            {
               can_supply -= result[i][k];
            }

            double need_consume = consume_[j];

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
            visited.push_back(element_coords);
            element_coords = find_element(element_coords);
         }

         std::vector<coordinates_t> new_visited, others;
         for (auto const & v : visited)
         {
             if (result[v.first][v.second] > 0)
                 new_visited.push_back(v);
             else
                 others.push_back(v);
         }

         while (new_visited.size() < consumers_count + suppliers_count - 1)
         {
             size_t idx = generate_random(others.size());
             std::swap(others[idx], others.back());
             new_visited.push_back(others.back());
             others.pop_back();
         }

         find_potentials(new_visited);
      }

      void find_potentials(std::vector<coordinates_t> const & coordinates)
      {
         size_t m = suppliers_count,
                n = consumers_count;
         assert(coordinates.size() == m + n - 1);
         matrix_t equations(m + n, m + n);
         matrix_t rhs(m + n, 1);

         for (size_t j = 0; j != m + n; ++j)
         {
            equations(0, j) = j == 0 ? 1 : 0;
         }

         rhs(0, 0) = 0;

         for (size_t i = 1; i != m + n; ++i)
         {
            auto coords = coordinates[i - 1];

            for (size_t j = 0; j != m + n; ++j)
            {
               equations(i, j) = (j == coords.first || j == m + coords.second) ? 1 : 0;
            }

            rhs(i, 0) = cost_[coords.first][coords.second];
         }

         std::vector<double> system_solution = solve_linear_system(equations, rhs);
         u_.resize(m);
         v_.resize(n);

         for (size_t i = 0; i != m; ++i)
         {
            u_[i] = system_solution[i];
         }

         for (size_t j = 0; j != n; ++j)
         {
            v_[j] = system_solution[m + j];
         }

//       std::clog << "Potentials:" << std::endl << "U: ";
//       boost::copy(u_, std::ostream_iterator<double>(std::clog, " "));
//       std::clog << std::endl << "V: ";
//       boost::copy(v_, std::ostream_iterator<double>(std::clog, " "));
//       std::clog << std::endl;
      }

      boost::optional<coordinates_t> find_negative_delta()
      {
         auto delta_calculator = [this](size_t i, size_t j)
         {
            return cost_[i][j] - u_[i] - v_[j];
         };
         boost::optional<coordinates_t> result;
         double min;

         for (size_t i = 0; i != suppliers_count; ++i)
         {
            for (size_t j = 0; j != consumers_count; ++j)
            {
               double delta = delta_calculator(i, j);

               if (delta < 0)
               {
                  if (!result || delta < min)
                  {
                     result = boost::in_place(i, j);
                     min = delta;
                  }
               }
            }
         }

         return result;
      }

      void improve_plan(coordinates_t const & to_increase)
      {
         enum mark_t
         {
            START,
            PLUS,
            MINUS,
            UNKNOWN,
         };

         std::vector<std::vector<mark_t>> pre_calc(suppliers_count, std::vector<mark_t>(consumers_count, UNKNOWN));

         pre_calc[to_increase.first][to_increase.second] = START;
         std::vector<coordinates_t> to_change;
         enum direction_t
         {
            VERT = -1,
            ANY = 0,
            HOR = 1,
         };
         std::function<bool(std::vector<std::vector<mark_t>> const &, coordinates_t const &, direction_t)> build_cycle =
            [&](std::vector<std::vector<mark_t>> const & A, coordinates_t const & cur, direction_t dir)
         {
            size_t m = suppliers_count,
                   n = consumers_count;
            if (dir == VERT || dir == ANY)
            {
               for (size_t i = 0; i != m; ++i)
               {
//                  if (result[i][cur.second] == 0) continue;
                  if (A[i][cur.second] == UNKNOWN)
                  {
                     auto B = A;
                     B[i][cur.second] = A[cur.first][cur.second] == MINUS ? PLUS : MINUS;
                     coordinates_t new_point(i, cur.second);
                     bool flag = build_cycle(B, new_point, HOR);

                     if (flag)
                     {
                        to_change.push_back(cur);
                        return true;
                     }
                  }
                  else if (dir != ANY && A[i][cur.second] == START && A[cur.first][cur.second] == MINUS)
                  {
                     to_change.push_back(cur);
                     return true;
                  }
               }
            }

            if (dir == HOR || dir == ANY)
            {
               for (size_t j = 0; j != n; ++j)
               {
//                  if (result[cur.first][j] == 0) continue;
                  if (A[cur.first][j] == UNKNOWN)
                  {
                     auto B = A;
                     B[cur.first][j] = A[cur.first][cur.second] == MINUS ? PLUS : MINUS;
                     coordinates_t new_point(cur.first, j);
                     bool flag = build_cycle(B, new_point, VERT);

                     if (flag)
                     {
                        to_change.push_back(cur);
                        return true;
                     }
                  }
                  else if (dir != ANY && A[cur.first][j] == START && A[cur.first][cur.second] == MINUS)
                  {
                     to_change.push_back(cur);
                     return true;
                  }
               }
            }

            return false;
         };

         build_cycle(pre_calc, to_increase, ANY);
         boost::reverse(to_change);

         assert(to_change.size() % 2 == 0);
//         for (auto p : to_change)
//             std::cout << p.first << "," << p.second << std::endl;

         double theta = std::numeric_limits<double>::max();

         for (size_t i = 1; i < to_change.size(); i += 2)
         {
            size_t x = to_change[i].first;
            size_t y = to_change[i].second;
            if (result[x][y] < theta)
               theta = result[x][y];
         }

         for (size_t i = 0; i != to_change.size(); ++i)
         {
            size_t x = to_change[i].first;
            size_t y = to_change[i].second;
            result[x][y] += i % 2 ? -theta : theta;
         }

         std::vector<coordinates_t> coords_for_potentials;
         std::vector<coordinates_t> others;
         for (size_t i = 0; i != suppliers_count; ++i)
         {
            for (size_t j = 0; j != consumers_count; ++j)
            {
               if (coords_for_potentials.size() == suppliers_count + consumers_count - 1)
                  break;
               if (result[i][j] > 0 && coords_for_potentials.size() < suppliers_count + consumers_count - 1)
               {
                  coords_for_potentials.push_back(std::make_pair(i, j));
               }
               else
               {
                   others.push_back(std::make_pair(i, j));
               }
            }
         }

         while (coords_for_potentials.size() < consumers_count + suppliers_count - 1)
         {
             size_t idx = generate_random(others.size());
             std::swap(others[idx], others.back());
             coords_for_potentials.push_back(others.back());
             others.pop_back();
         }

         find_potentials(coords_for_potentials);
      }

   private:
      std::vector<double> supply_, consume_;
      std::vector<std::vector<double>> cost_;
      std::vector<double> u_, v_;  // potentials
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

   size_t solver_t::iterations_count() const
   {
      if (!pimpl_->solved)
      {
         BOOST_THROW_EXCEPTION(std::runtime_error("Not solved"));
      }

      return pimpl_->iterations_count;
   }

   std::vector<solver_t::point_t> solver_t::angle_points()
   {
      return pimpl_->angle_points();
   }

   void print_result(solver_t const & solver, std::ostream & out)
   {
      for (size_t i = 0; i != solver.suppliers_count(); ++i)
      {
         out << "Supplier #" << i << " gives: " << std::endl;

         for (size_t j = 0; j != solver.consumers_count(); ++j)
         {
            out << solver.get_value(i, j) << " ";
         }

         out << std::endl;
      }

      out << "Iterations count: " << solver.iterations_count() << std::endl;
   }

}
