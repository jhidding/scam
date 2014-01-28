#pragma once
#include <array>
#include <algorithm>
#include <numeric>
#include <cmath>

#include "vector.hh"

namespace Scam
{
	template <typename T = double>
	class Quat_
	{
		T          m_scalar;
		Vector_<T> m_vector;

		public:
			// accessors
			T s() const { return m_scalar; }
			Vector_<T> const &v() const { return m_vector; }

			// constructors
			Quat_(T s_, Vector_<T> const &v_):
				m_scalar(s_),
				m_vector(v_)
			{}

			static Quat_ rotation(Vector_<T> const &u, T theta)
			{
				return Quat_(cos(theta/2), u.scale(sin(theta/2)));
			}

			static Quat_ scalar(T s_)
			{
				return Quat_(s_, Vector_<T>(0, 0, 0));
			}

			// product
			Quat_ operator*(Quat_ const &o) const
			{
				return Quat_(
					s()*o.s() - Vector_<T>::dot(v(),o.v()),
					o.v().scale(s()) +
					v().scale(o.s()) +
					Vector_<T>::cross(v(), o.v()));
			}

			// other methods
			Quat_ conj() const
			{
				return Quat_(s(), -v());
			}

			T sqr() const
			{
				return s()*s() + v().sqr();
			}

			Quat_ scale(T a) const
			{
				Quat_(s()*a, v().scale(a));
			}

			Quat_ inv() const
			{
				return conj().scale(1./sqr());
			}

			// vector conjugation
			Vector_<T> operator()(Vector_<T> const &a) const
			{
				return (*this * Quat_(0, a) * conj()).v();
			}
	};

	using Quat = Quat_<>;
}

