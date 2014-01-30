/* mvector.h
 *
 * mini vector
 */

#pragma once

#include <algorithm>
#include <functional>
#include <numeric>
#include <initializer_list>
#include <array>

#include <iostream>
#include <iterator>
#include "common.hh"

namespace Scam
{
	template <typename T, unsigned R>
	class mVector: public std::array<T, R>
	{
		public:
			using std::array<T,R>::begin;
			using std::array<T,R>::end;
			using std::array<T,R>::array;

			mVector() {}

			mVector(T x)
			{
				std::fill(begin(), end(), x);
			}

			mVector(std::initializer_list<T> l)
			{
				std::copy(l.begin(), l.end(), begin());
			}

			template <typename U>
			mVector<U, R> as() const
			{
				mVector<U, R> v;
				std::copy(begin(), end(), v.begin());
				return v;
			}

			template <typename Fun>
			mVector operate(Fun f) const
			{ 
				mVector a;
				std::transform(begin(), end(), a.begin(), f);
				return a;
			}

			template <typename Fun>
			mVector operate(Fun f, mVector const &o) const
			{ 
				mVector a;
				std::transform(begin(), end(), o.begin(), a.begin(), f);
				return a;
			}

			mVector operator+=(mVector const &o)
			{ std::transform(begin(), end(), o.begin(), begin(), std::plus<T>()); return *this; }
			mVector operator-=(mVector const &o)
			{ std::transform(begin(), end(), o.begin(), begin(), std::minus<T>()); return *this; }
			mVector operator*=(mVector const &o)
			{ std::transform(begin(), end(), o.begin(), begin(), std::multiplies<T>()); return *this; }

			template <typename U>
			mVector operator/=(U o)
			{ 
				std::transform(begin(), end(), begin(),
						std::bind2nd(std::divides<T>(), o)); 
				return *this; 
			}
			template <typename U>
			mVector operator*=(U o)
			{ 
				std::transform(begin(), end(), begin(),
						std::bind2nd(std::multiplies<T>(), o)); 
				return *this; 
			}
			template <typename U>
			mVector operator%=(U o)
			{ 
				std::transform(begin(), end(), begin(), 
						[o] (T a) { return modulus(a, o); }); 
				return *this; 
			}

			mVector operator+(mVector const &o) const
			{ return operate(std::plus<T>(), o); }
			mVector operator-(mVector const &o) const
			{ return operate(std::minus<T>(), o); }
			mVector operator-() const
			{ return operate(std::negate<T>()); }
			mVector operator*(mVector const &o) const
			{ return operate(std::multiplies<T>(), o); }

			template <typename U>
			mVector operator/(U o) const
			{ return operate(std::bind2nd(std::divides<T>(), o)); }
			template <typename U>
			mVector operator*(U o) const
			{ return operate(std::bind2nd(std::multiplies<T>(), o)); }
			template <typename U>
			mVector operator%(U o) const
			{ return operate([o] (T a) { return modulus(a, o); }); }

			// maths
			mVector abs() const
			{
				mVector a; 
				std::transform(begin(), end(), a.begin(), ::fabs);
				return a;
			}

			T sqr() const
			{ 
				T v = 0; 
				for (auto x : *this)
					v += x*x;
				return v;
			}

			T norm() const
			{ 
				return sqrt(sqr());
			}

			T max() const
			{
				return *std::max_element(begin(), end());
			}

			T sum() const
			{
				return std::accumulate(begin(), end(), T(0));
			}

			T inf_norm() const
			{
				return abs().max();
			}

			T dot(mVector const &o) const
			{
				return std::inner_product(begin(), end(), o.begin(), T(0));
			}
	};

	template <typename T, unsigned R>
	std::ostream &operator<<(std::ostream &out, mVector<T, R> const &v)
	{
		std::copy(v.begin(), v.end(), std::ostream_iterator<T>(out, " "));
		return out;
	}

}

// vim:ts=4:sw=4:tw=80
