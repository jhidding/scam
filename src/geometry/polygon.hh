#pragma once
#include <map>
#include <vector>
#include <functional>
#include <string>

#include "../base/common.hh"
#include "point.hh"
#include "vector.hh"
#include "vertex.hh"

namespace Scam
{
	class Polygon
	{
		friend std::hash<Polygon>;

		ptr<std::vector<Vertex>>	   m_vert;
		std::map<std::string, std::string> m_info;
		size_t				   m_hash;

		size_t compute_hash() const;

		public:
			using const_iterator = std::vector<Vertex>::const_iterator;

			Polygon(ptr<std::vector<Vertex>> V):
				m_vert(V)
			{
				m_hash = compute_hash();
			}

			template <typename Iter>
			Polygon(Iter a, Iter b):
				m_vert(new std::vector<Vertex>(a, b))
			{
				m_hash = compute_hash();
			}


			ptr<std::vector<Vertex>> const &vertices() const
			{
				return m_vert;
			}

			size_t size() const 
			{ 
				return m_vert->size(); 
			}

			bool empty() const
			{
				return m_vert->empty();
			}

			const_iterator begin() const
			{
				return m_vert->begin();
			}

			const_iterator end() const
			{
				return m_vert->end();
			}
	};

	class Segment
	{
		friend std::hash<Segment>;

		Vertex 					m_a, m_b;
		std::map<std::string, std::string> 	m_info;

		public:
			Segment(Vertex const &a_, Vertex const &b_):
				m_a(std::min(a_, b_)),
				m_b(std::max(a_, b_))
			{}

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

