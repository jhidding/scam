/* map.h
 *
 * lazy mapping, very similar to Access class, but now we don't have
 * writing access, since the the function returns a computed value
 * in stead of a reference.
 */

#pragma once

#include <iterator>
#include <functional>

namespace Scam
{
	template <typename SrcList, typename Tgt>
	class Map
	{
		public:
			typedef typename SrcList::value_type arg_type;
			typedef Tgt value_type;

		private:
			typedef std::function<Tgt (arg_type const &)> func_type;
			typedef typename SrcList::const_iterator src_iterator;

			SrcList source;
			func_type f;

		public:
			Map(SrcList const &source_, func_type const &f_):
				source(source_), f(f_) {}

			class const_iterator: 
				public std::iterator<std::forward_iterator_tag, value_type>
			{
				src_iterator base;
				func_type f;

				public:
					const_iterator(Map const &obj_, src_iterator i):
						base(i), f(obj_.f)
					{}

					value_type operator*() const
					{
						return f(*base);
					}

					arg_type arg() const
					{
						return *base;
					}

					bool operator==(const_iterator const &o) const { return base == o.base; }
					bool operator!=(const_iterator const &o) const { return base != o.base; }
					const_iterator &operator++() { ++base; return *this; }
			};

			size_t size() const { return source.size(); }

			value_type operator[](size_t i) const
			{ return f(source[i]); }

			const_iterator begin() const
			{ return const_iterator(*this, source.begin()); }

			const_iterator end() const
			{ return const_iterator(*this, source.end()); }
	};

	template <typename SrcList, typename Func>
	auto lazy_map(SrcList const &src, Func f) -> 
		Map<SrcList, decltype(f(*src.begin()))>
	{
		return Map<SrcList, decltype(f(*src.begin()))>(src, f);
	}
}

// vim:ts=4:sw=4:tw=80
