#pragma once
#include <map>
#include <vector>
#include <functional>
#include <string>
#include <sstream>

#include "../base/common.hh"
#include "../base/info.hh"
#include "point.hh"
#include "vector.hh"
#include "vertex.hh"
#include "surface.hh"

namespace Scam
{
	class Polygon
	{
		friend std::hash<Polygon>;

		Array<Vertex>			   m_vert;
		Plane				   m_plane;
		Info				   m_info;
		size_t				   m_hash;

		size_t compute_hash() const;

		public:
			using const_iterator = Array<Vertex>::const_iterator;

			Polygon() {}

			Polygon(Array<Vertex> V):
				m_vert(V)
			{
				m_hash = compute_hash();
				m_plane = Plane((*V)[1], Vector::cross(
					(*V)[2] - (*V)[1], 
					(*V)[0] - (*V)[1]).normalize());
			}

			template <typename Iter>
			Polygon(Iter a, Iter b):
				m_vert(Array<Vertex>(a, b))
			{
				m_hash = compute_hash();
				m_plane = Plane((*m_vert)[1], Vector::cross(
					(*m_vert)[2] - (*m_vert)[1], 
					(*m_vert)[0] - (*m_vert)[1]).normalize());
			}

			Polygon(Array<Vertex> V, Plane const &P):
				m_vert(V), m_plane(P)
			{
				m_hash = compute_hash();
			}

			Info info() const { return m_info; }

			template <typename T>
			void set_info(std::string const &key, T const &value)
			{
				m_info.set(key, value);
			}

			template <typename T>
			Maybe<T> get_info(std::string const &key) const
			{
				return m_info.get<T>(key);
			}

			Array<Vertex> vertices() const
			{
				return m_vert;
			}

			Plane const &plane() const
			{
				return m_plane;
			}

			size_t size() const 
			{ 
				return m_vert.size(); 
			}

			bool empty() const
			{
				return m_vert.empty();
			}

			const_iterator begin() const
			{
				return m_vert.begin();
			}

			const_iterator end() const
			{
				return m_vert.end();
			}
	};

	class Segment
	{
		friend std::hash<Segment>;

		Vertex 					m_a, m_b;
		Info				   m_info;

		public:
			Segment(Vertex const &a_, Vertex const &b_):
				m_a(std::min(a_, b_)),
				m_b(std::max(a_, b_))
			{}

			Info info() const { return m_info; }

			template <typename T>
			void set_info(std::string const &key, T const &value)
			{
				m_info.set(key, value);
			}

			template <typename T>
			Maybe<T> get_info(std::string const &key) const
			{
				return m_info.get<T>(key);
			}

			Vertex const &first() const
			{ 
				return m_a; 
			}

			Vertex const &second() const
			{
				return m_b;
			}

			bool operator==(Segment const &o) const
			{
				return m_a == o.m_a and m_b == o.m_b;
			}
	};
}

namespace std
{
	template <>
	struct hash<Scam::Polygon>
	{
		using result_type = size_t;
		using argument_type = Scam::Polygon;

		size_t operator()(Scam::Polygon const &v) const
		{
			return v.m_hash;
		}
	};

	template <>
	struct hash<Scam::Segment>
	{
		using result_type = size_t;
		using argument_type = Scam::Segment;
		
		size_t operator()(Scam::Segment const &v) const
		{
			return hash<Scam::Vertex>()(v.first()) * 1024 + 
			       hash<Scam::Vertex>()(v.second());
		}
	};
}

