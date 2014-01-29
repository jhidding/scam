#pragma once
#include <array>
#include <algorithm>
#include <numeric>
#include <iostream>
#include "vector.hh"

namespace Scam
{
	template <typename T = double>
	class Point_
	{
		std::array<T, 3> X;

		public:
			// constructors
			Point_(T x_, T y_, T z_)
			{
				X[0] = x_; X[1] = y_; X[2] = z_;
			}

			Point_() {}

			template <typename Iter>
			Point_(Iter a, Iter b)
			{
				std::copy(a, b, X.begin());
			}

			/*
			template <typename Range>
			Point_(Range const &R)
			{
				std::copy(R.cbegin(), R.cend(), X.begin());
			}*/


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

			// methods
			Vector_<T> operator-(Point_ const &o) const
			{
				return Vector_<T>(x()-o.x(), y()-o.y(), z()-o.z());
			}

			Point_ operator+(Vector const &v) const
			{
				return Point_(x()+v.x(), y()+v.y(), z()+v.z());
			}
	};

	template <typename T>
	std::ostream &operator<<(std::ostream &out, Point_<T> const &p)
	{
		return out << p.x() << " " << p.y() << " " << p.z();
	}

	using Point = Point_<>;
}

