#include "transportation_solver.h"

#include <vector>
#include <stdexcept>
#include <cassert>
#include <iterator>
#include <deque>

#include <boost/range/algorithm/copy.hpp>
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
        typedef enum {VERT = -1, HOR = 1, ANY = 0} direction;

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

            matrix_t inverse(m, n);
            invert_matrix(matrix, inverse);
            matrix_t product(n, 1);
            boost::numeric::ublas::axpy_prod(inverse, rhs, product);
            std::vector<double> res;

//         std::clog << "Matrix: " << matrix << std::endl
//                   << "Rhs: " << rhs << std::endl
//                   << "Inverse: " << inverse << std::endl
//                   << "Product: " << product << std::endl
//                   ;

            for (size_t i = 0; i != n; ++i)
            {
                res.push_back(product(i, 0));
            }

            return res;
        }

        std::vector<solver_t::point_t> find_angle_points(matrix_t const& matrix, matrix_t const& rhs)
        {
            size_t m = matrix.size1(),
                   n = matrix.size2();

            if (rhs.size1() != m || rhs.size2() != 1)
            {
                BOOST_THROW_EXCEPTION(std::runtime_error("Invalid rhs size"));
            }

            // HACK
            size_t rank = m - 1; // I hope it's always true in our case, but I'm not sure :(

            return {};
        }
    }

    struct solver_t::implementation_t
    {
        size_t suppliers_count;
        size_t consumers_count;

        std::vector<std::vector<double>> result;

        bool solved;

        size_t iterations_count;

        implementation_t(std::istream& in)
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

            for (size_t j = 0; j != m; ++j)
            {
                for (size_t k = 0; k != m * n; ++k)
                {
                    equations(j + m, k) = (k % n == j) ? 1 : 0;
                }

                rhs(j + m, 0) = consume_[j];
            }

//         std::clog << "Equations: " << equations << std::endl
//                   << "Rhs: " << rhs << std::endl
//                   ;

            return find_angle_points(equations, rhs);
        }

    private:
        typedef std::pair<size_t, size_t> coordinates_t;

        void find_initial_plan()
        {
            std::vector<coordinates_t> visited;
            auto find_element = [this](coordinates_t coords)
            {
                size_t i = coords.first, j = coords.second;
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

            find_potentials(visited);
        }

        void find_potentials(std::vector<coordinates_t> const& coordinates)
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

//         std::clog << "Potentials:" << std::endl << "U: ";
//         boost::copy(u_, std::ostream_iterator<double>(std::clog, " "));
//         std::clog << std::endl << "V: ";
//         boost::copy(v_, std::ostream_iterator<double>(std::clog, " "));
//         std::clog << std::endl;
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

        void improve_plan(coordinates_t const& to_increase)
        {
            std::vector<std::vector<char> > pre_calc(
                result.size(), std::vector<char>(result[0].size())
            );

            for (int i = 0; i < result.size(); i++)
            {
                for (int j = 0; j < result[i].size(); j++)
                {
                    pre_calc[i][j] = '?';
                }
            }

            pre_calc[to_increase.second][to_increase.first] = 'S';
            std::deque<coordinates_t> to_change;
            std::vector<coordinates_t> not_null;
            _buildCycle(pre_calc, to_increase, result.size(), result[0].size(), ANY, to_change);

            double theta = 9999999999;

            for (int i = 0; i < to_change.size(); i++)
            {
                if (i % 2)
                {
                    theta = result[to_change[i].first][to_change[i].second] < theta ? result[to_change[i].first][to_change[i].second] : theta;
                }
            }

            for (int i = 0; i < to_change.size(); i++)
            {
                if (i % 2)
                {
                    result[to_change[i].first][to_change[i].second] = result[to_change[i].first][to_change[i].second] - theta;
                }
                else
                {
                    result[to_change[i].first][to_change[i].second] = theta + result[to_change[i].first][to_change[i].second];
                }
            }

            for (int i = 0; i < result.size(); i++)
            {
                for (int j = 0; j < result[i].size(); j++)
                {
                    if (result[i][j] > 0)
                    {
                        coordinates_t to_add(i,j);
                        not_null.push_back(to_add);
                    }
                }
            }

            find_potentials(not_null);
        }

        bool _buildCycle(std::vector<std::vector<char> > A, coordinates_t curPoint, int m, int n, direction dir, std::deque<coordinates_t>& q)
        {
            if (dir == VERT || dir == ANY)
            {
                for (int i = 0; i < m; i++)
                {
                    if (A[i][curPoint.first] == '?') // try
                    {
                        std::vector<std::vector<char> > B = A;
                        A[curPoint.second][curPoint.first] == '-' ? B[i][curPoint.first] = '+' :  B[i][curPoint.first] = '-';
                        coordinates_t newPoint(curPoint.first, i);
                        bool flag = _buildCycle(B, newPoint, m, n, HOR, q);

                        if (flag)
                        {
                            q.push_front(curPoint);
                            return true;
                        }
                    }
                    else if (dir != ANY && A[i][curPoint.first] == 'S') // found cycle!
                    {
                        q.push_front(curPoint);
                        return true;
                    }
                }
            }

            if (dir == HOR || dir == ANY)
            {
                for (int i = 0; i < n; i++)
                {
                    if (A[curPoint.second][i] == '?') // try
                    {
                        std::vector<std::vector<char> > B = A;
                        A[curPoint.second][curPoint.second] == '-' ? B[curPoint.second][i] = '+' :  B[curPoint.second][i] = '-';
                        coordinates_t newPoint(i, curPoint.second);
                        bool flag = _buildCycle(B, newPoint, m, n, VERT, q);

                        if (flag)
                        {
                            q.push_front(curPoint);
                            return true;
                        }
                    }
                    else if (dir != ANY && A[curPoint.second][i] == 'S') // found cycle!
                    {
                        q.push_front(curPoint);
                        return true;
                    }
                }
            }

            return false;
        }

    private:
        std::vector<double> supply_, consume_;
        std::vector<std::vector<double>> cost_;
        std::vector<double> u_, v_;  // potentials
    };

    solver_t::solver_t(std::istream& in)
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

    void print_result(solver_t const& solver, std::ostream& out)
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
