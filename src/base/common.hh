#pragma once
#include <functional>
#include <iostream>
#include <vector>
#include <memory>

namespace System
{
	typedef std::function<void (int, char **)> Command;

	template <typename R>
	auto head(R const &r) -> decltype(*r.begin())
	{
		return *r.begin();
	}

	template <typename T>
	class Tail
	{
		public:
			typedef typename T::value_type value_type;
			typedef typename T::const_iterator const_iterator;
			
			Tail(T const &a):
				b(a.begin()), e(a.end())
			{ 
				++b;
			}

			const_iterator begin() const { return b; }
			const_iterator end() const { return e; }
			const_iterator cbegin() const { return b; }
			const_iterator cend() const { return e; }

		private:
			const_iterator b, e;
	};

	template <typename T>
	Tail<T> tail(T const &a) { return Tail<T>(a); }

	template <typename R>
	std::string join(R const &r, std::string const &d)
	{
		std::string a = head(r);
		for (auto b : tail(r))
			a += d + b;

		return a;
	}
}

namespace Scam
{
	template <typename T>
	using ptr = std::shared_ptr<T>;

	template <typename T, typename ...Args>
	ptr<T> make_ptr(Args &&...args)
	{ return std::make_shared<T>(std::forward<Args>(args)...); }

	/*! Maybe monad
	 */
	template <typename T>
	class Maybe
	{
		ptr<T> X;

		public:
			Maybe() {}
			Maybe(T const &X_): X(new T(X_)) {}
			Maybe(ptr<T> X_): X(X_) {}

			operator bool() const { return static_cast<bool>(X); }
			T const &operator*() const { return *X; }
	};

	class MaybeNothing
	{
		public:
			template <typename T>
			operator Maybe<T>() const { return Maybe<T>(); }
	};
	
	extern MaybeNothing Nothing;

	template <typename T>
	Maybe<T> Just(T const &X) { return Maybe<T>(X); }

	template <typename T>
	Maybe<T> Just(ptr<T> X) { return Maybe<T>(X); }

	template <typename T>
	std::ostream &operator<<(std::ostream &out, Maybe<T> const &X)
	{
		if (X)
			return out << *X;
		else
			return out << "Nothing";
	}

	template <typename R>
	auto head(R const &r) -> decltype(*r.begin())
	{
		return *r.begin();
	}

	template <typename T>
	class Tail
	{
		public:
			typedef typename T::value_type value_type;
			typedef typename T::const_iterator const_iterator;
			
			Tail(T const &a):
				b(a.begin()), e(a.end())
			{ 
				++b;
			}

			const_iterator begin() const { return b; }
			const_iterator end() const { return e; }
			const_iterator cbegin() const { return b; }
			const_iterator cend() const { return e; }

		private:
			const_iterator b, e;
	};

	template <typename T>
	Tail<T> tail(T const &a) { return Tail<T>(a); }
}

/*!
 * calculates a mod b, by the mathematical definition:
 * the C operator % does not give the right answer if a < 0
 */
inline int modulus(int a, int b)
{
	if (a < 0) return b + (a %  b);
	else return (a % b);
}

/*!
 * calculates the a mod b, where a and b are double floats.
 */
inline double modulus(double a, double b)
{
	if (a < 0) return a - static_cast<int>(a / b - 1) * b;
	else return a - static_cast<int>(a / b) * b;
}

inline size_t ipow(size_t N, unsigned R)
{
	size_t S = 1;
	for (unsigned i = 0; i < R; ++i)
		S *= N;
	return S;
}

