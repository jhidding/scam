#pragma once
#include <array>
#include <algorithm>
#include <numeric>
#include <iostream>

namespace Scam
{
	template <typename T = double>
	class Vector_
	{
		std::array<T, 3> X;

		public:
			// constructors
			Vector_() {}

			Vector_(T x_, T y_, T z_)
			{
				X[0] = x_; X[1] = y_; X[2] = z_;
			}

			template <typename Iter>
			Vector_(Iter a, Iter b)
			{
				std::copy(a, b, X.begin());
			}

			template <typename Range>
			Vector_(Range const &R)
			{
				std::copy(R.cbegin(), R.cend(), X.begin());
			}

			// accessors
			T x() const { return X[0]; }
			T y() const { return X[1]; }
			T z() const { return X[2]; }

			// range methods
			// using std::array<T, 3>::iterator;
			using const_iterator = typename std::array<T, 3>::const_iterator;

			const_iterator begin() { return X.cbegin(); }
			const_iterator end() { return X.cend(); }

			const_iterator cbegin() const { return X.cbegin(); }
			const_iterator cend() const { return X.cend(); }

			// products
			static T dot(Vector_ const &a, Vector_ const &b)
			{
				return a.x() * b.x() + a.y() * b.y() + a.z() * b.z();
				//return std::inner_product(a.cbegin(), a.cend(), b.cbegin(), T(0));
			}

			static Vector_ cross(Vector_ const &a, Vector_ const &b)
			{
				return Vector_(
					a.y()*b.z() - a.z()*b.y(),
					a.z()*b.x() - a.x()*b.z(),
					a.x()*b.y() - a.y()*b.x());
			}

			// methods
			T sqr() const
			{
				return dot(*this, *this);
				//return std::inner_product(cbegin(), cend(), cbegin(), T(0));
			}

			T norm() const
			{
				return sqrt(sqr());
			}

			Vector_ scale(T s) const
			{
				return Vector_(x()*s, y()*s, z()*s);
			}

			Vector_ operator-() const
			{
				return scale(-1);
			}

			Vector_ operator+(Vector_ const &o) const
			{
				return Vector_(x() + o.x(), y() + o.y(), z() + o.z());
			}

			T operator*(Vector_ const &o) const
			{
				return dot(*this, o);
			}

			Vector_ operator*(T s) const
			{
				return scale(s);
			}

			Vector_ normalize() const
			{
				T n = norm();
				return Vector_(x()/n, y()/n, z()/n);
			}
	};

	using Vector = Vector_<>;

	template <typename T>
	std::ostream &operator<<(std::ostream &out, Vector_<T> const &a)
	{
		return out << a.x() << " " << a.y() << " " << a.z();
	}
}

