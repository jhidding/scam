#pragma once
#include <functional>
#include <iostream>
#include <vector>
#include <memory>
#include <algorithm>

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

	template <typename T>
	class Seq
	{
		std::shared_ptr<T> _data;

		public:
			typedef typename T::value_type value_type;
			typedef typename T::iterator iterator;
			typedef typename T::const_iterator const_iterator;

			Seq() {}

			Seq(T *data_): 
				_data(data_) 
			{}

			T &operator*() { return *_data; }
			T *operator->() { return _data.get(); }
			T const *operator->() const { return _data.get(); }
			T *get() { return _data.get(); }
			T const *get() const { return _data.get(); }

			size_t size() const { return _data->size(); }
			bool empty() const { return _data->empty(); }
			value_type &operator[](size_t idx) { return (*_data)[idx]; }
			value_type const &operator[](size_t idx) const { return (*_data)[idx]; }
			void push_back(value_type const &t) { _data->push_back(t); }
			iterator begin() const { return _data->begin(); }
			iterator end() const { return _data->end(); }	
			const_iterator cbegin() const { return _data->cbegin(); }
			const_iterator cend() const { return _data->cend(); }	
	};

	template <typename T>
	class Array: public Seq<std::vector<T>>
	{
		public:
			typedef T value_type;

			using Seq<std::vector<T>>::get;

			Array(size_t n = 0):
				Seq<std::vector<T>>(new std::vector<T>(n))
			{}

			Array(size_t n, T value):
				Seq<std::vector<T>>(new std::vector<T>(n, value))
			{}

			template <typename Iter>
			Array(Iter a, Iter b):
				Seq<std::vector<T>>(new std::vector<T>(a, b))
			{}

			Array(std::initializer_list<T> l):
				Seq<std::vector<T>>(new std::vector<T>(l))
			{}
	};
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
			T const *operator->() const { return X.get(); }
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


	template <typename R>
	void sort(R r)
	{
		std::sort(r.begin(), r.end());
	}

	/*
	template <typename F, typename ...Args>
	auto apply_with(Args &&...args) -> std::function<decltype(F()(args...))(F)>
	{
		return [args...] (F f)
		{
			return f(std::forward<Args>(args)...);
		};
	}
	*/

	template <typename F, typename T>
	void for_each(F f, Array<T> a)
	{
		std::for_each(a.begin(), a.end(), f);
	}

	template <typename F, typename T>
	auto flatmap(F f, Array<T> a) -> decltype(f(*a.begin()))
	{
		using R = decltype(f(*a.begin()));

		R c;
		for (T const &i : a)
		{
			R b = f(i);
			std::copy(b.begin(), b.end(), std::back_inserter(c));
		}
		return c;
	}

	template <typename F, typename T>
	auto map(F f, T a) -> Array<decltype(f(*a.begin()))>
	{
		using R = decltype(f(*a.begin()));
		Array<R> c;
		std::transform(a.begin(), a.end(), std::back_inserter(c), f);
		return c;
	}

	template <typename T>
	typename T::value_type product(T const &A)
	{
		return std::accumulate(A.begin(), A.end(), (typename T::value_type)(1), 
				std::multiplies<typename T::value_type>());
	}	
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

