#pragma once
#include <functional>
#include <iostream>

#include "point.hh"
#include "vector.hh"

namespace Scam
{
	class Vertex
	{
		friend std::hash<Vertex>;

		static size_t count;

		size_t id;
		Point  p;

		public:
			Vertex() {}

			Vertex(Point const &p_):
				id(count++), p(p_) {}

			bool operator==(Vertex const &o) const
			{
				return id == o.id;
			}

			bool operator<(Vertex const &o) const
			{
				return id < o.id;
			}

			// in effect a Vertex can be treated as a Point
			operator Point() const
			{ 
				return p; 
			}
	};

	inline std::ostream &operator<<(std::ostream &out, Vertex const &v)
	{
		return out << static_cast<Point>(v);
	}
}

namespace std
{
	template <>
	struct hash<Scam::Vertex>
	{
		using result_type = size_t;
		using argument_type = Scam::Vertex;

		size_t operator()(Scam::Vertex const &v) const
		{
			return v.id;
		}
	};
}

