#pragma once
#include <functional>
#include <iostream>
#include <sstream>
#include <map>

#include "../base/common.hh"
#include "point.hh"
#include "vector.hh"

namespace Scam
{
	class Vertex: public Point
	{
		friend std::hash<Vertex>;

		static size_t count;
		size_t id;
		//Point  p;

		using Info_t = std::map<std::string,std::string>;
		ptr<Info_t> m_info;

		public:
			Vertex() {}

			Vertex(double x, double y, double z):
				Point(x, y, z), id(count++) {}

			Vertex(Point const &p_):
				Point(p_), id(count++) {}

			bool operator==(Vertex const &o) const
			{
				return id == o.id;
			}

			bool operator<(Vertex const &o) const
			{
				return id < o.id;
			}

			template <typename T>
			void set_info(std::string const &key, T const &value)
			{
				if (not m_info) m_info = make_ptr<Info_t>();
				std::ostringstream ss; ss << value;
				(*m_info)[key] = ss.str();
			}

			template <typename T>
			Maybe<T> get_info(std::string const &key) const
			{
				if (not m_info) return Nothing;
				auto i = m_info->find(key);
				if (i == m_info->end()) return Nothing;
				std::istringstream ss(i->second);
				T value; ss >> value;
				return value;
			}

			/* in effect a Vertex can be treated as a Point
			operator Point() const
			{ 
				return p; 
			}*/
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

