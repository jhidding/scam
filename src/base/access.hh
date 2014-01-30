#pragma once

#include <functional>
#include <iterator>

/*
 * class Access, takes a source range, and a function.
 * The source range must be an object that has begin()
 * and end() returning an iterator. The function should
 * work on the value_type and return a reference to some
 * value that is contained in value_type.
 * This allows to iterate over specific members in a list
 * of structs. Specifically, if value_type is a vector,
 * and the source range some space (filled with vectors),
 * we get an object that iterates over all x, y, or z values.
 *
 * Below we defined a factory function that should be able to
 * handle all the long typenames automatically.
 */

namespace Scam
{
	template <typename SrcList, typename Tgt>
	class Access
	{
		typedef std::function<Tgt &(typename SrcList::value_type &)> func_type;
		typedef typename SrcList::iterator src_iterator;
		typedef typename SrcList::iterator src_const_iterator;

		SrcList source;
		func_type f;

		public:
			typedef Tgt value_type;

			Access(SrcList source_, func_type const &f_):
				source(source_), f(f_) {}

			class iterator: 
				public src_iterator,
				public std::iterator<std::forward_iterator_tag, value_type>
			{
				Access const *obj;

				public:
					iterator(Access const *obj_, src_iterator i):
						src_iterator(i), 
						obj(obj_)
					{}

					value_type &operator*()
					{
						return (obj->f)(src_iterator::operator*());
					}
			};

			class const_iterator: 
				public src_const_iterator,
				public std::iterator<std::forward_iterator_tag, value_type>
			{
				Access const *obj;

				public:
					const_iterator(Access const *obj_, src_const_iterator i):
						src_const_iterator(i), 
						obj(obj_)
					{}

					value_type const &operator*()
					{
						return (obj->f)(src_const_iterator::operator*());
					}
			};

			value_type &operator[](size_t i)
			{ return f(source[i]); }

			value_type const &operator[](size_t i) const
			{ return f(source[i]); }

			iterator begin()
			{ return iterator(this, source.begin()); }

			iterator end()
			{ return iterator(this, source.end()); }

			const_iterator begin() const
			{ return const_iterator(this, source.begin()); }

			const_iterator end() const
			{ return const_iterator(this, source.end()); }
	};

	template <typename>
	struct Dereference;

	template <typename T>
	struct Dereference<T &>
	{
		typedef T type;
	};

	template <typename SrcList, typename Func>
	auto access(SrcList source, Func f) 
		-> Access<SrcList, typename Dereference<decltype(f(*source.begin()))>::type>
	{
		return Access<SrcList, typename Dereference<decltype(f(*source.begin()))>::type>(source, f);
	}
}

// vim:ts=4:sw=4:tw=80
