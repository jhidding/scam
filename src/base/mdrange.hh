/* ndrange.h
 * NdRange does nice Nd iteration with custom step size
 */

#pragma once

#include "mvector.hh"
#include <vector>
#include <utility>
#include <iterator>
#include <iostream>

namespace Scam
{
	template <unsigned R>
	class MdRange
	{
		public:
			typedef mVector<int, R> value_type;
			typedef size_t arg_type;

			value_type			 	shape, step, leap;
			size_t					m_size;

			class const_iterator: 
				public std::iterator<std::forward_iterator_tag, value_type const>
			{
				value_type				shape, step, leap, value;
				size_t					idx;

				public:
					const_iterator(MdRange const &range_, size_t idx_):
						shape(range_.shape), step(range_.step),
						leap(range_.leap), value(0), idx(idx_)
					{}

					const_iterator &operator++()
					{
						idx += step[0];
						++(value[0]);

						for (size_t i = 0; i < R-1; ++i)
						{
							if (value[i] >= shape[i])
							{
								++value[i+1];
								value[i] = 0;

								idx += leap[i+1];
							}
						}

						return *this;
					}

					value_type const &operator*() const
					{ return value; }

					value_type const *operator->() const
					{ return &value; }

					size_t arg() const
					{ return idx; }

					bool operator==(const_iterator const &other) const
					{ return idx == other.idx; }

					bool operator!=(const_iterator const &other) const
					{ return idx != other.idx; }
			};

			typedef const_iterator iterator;

			MdRange(value_type const &shape_):
				shape(shape_),
				m_size(product(shape))
			{
				step[0] = 1;
				for (unsigned i = 1; i < R; ++i)
				{
					step[i] = step[i-1] * shape[i-1];
					leap[i] = 0;
				}
			}

			MdRange(value_type const &shape_, value_type const &step_):
				shape(shape_), step(step_),
				m_size(product(shape))
			{
				m_size = step_.back() * shape_.back();
				for (unsigned i = 1; i < R; ++i)
					leap[i] = step[i] - shape[i-1]*step[i-1];
			}

			MdRange(std::initializer_list<size_t> lst):
				MdRange(value_type(lst.begin(), lst.end()))
			{}

			size_t size() const { return m_size; }

			size_t index(value_type const &A) const
			{
				return step.dot(A);
			}

			value_type operator[](size_t idx) const
			{
				value_type A;
				for (unsigned i = 0; i < R; ++i)
				{
					A[i] = idx % shape[i];
					idx = (idx - A[i]) / shape[i];
				}
				return A;
			}

			const_iterator at(size_t i) const
			{ return const_iterator(*this, i); }

			const_iterator begin() const
			{ return const_iterator(*this, 0); }

			const_iterator end() const
			{ return const_iterator(*this, m_size); }
	};
}

// vim:ts=4:sw=4:tw=80
