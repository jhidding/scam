#pragma once
#include <map>
#include <vector>
#include <functional>
#include <string>

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
			template <typename Range>
			Polygon(Range R):
				m_vert(new std::vector<Vertex>(R.cbegin(), R.cend()))
			{
				m_hash = compute_hash();
			}

			std::vector<Vertex> const &vertices() const
			{
				return m_vert;
			}

			size_t size() const 
			{ 
				return m_vert.size(); 
			}

			bool empty() const
			{
				return m_vert.empty();
			}
	};

	class Segment
	{
		friend std::hash<Segment>;

		Vertex 					m_a, m_b;
		std::map<std::string, std::string> 	m_info;

		public:
			Segment(Vertex const &a_, Vertex const &b_)
			{
				if (a_ < b_) 
				{
					m_a = a_;
					m_b = b_;
				}
				else
				{
					m_a = b_;
					m_b = a_;
				}

				compute_hash();
			}

			Vertex const &first() const
			{ 
				return m_a; 
			}

			Vertex const &second() const
			{
				return m_b;
			}

			bool operator==(Segment const &o)
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
			return hash<Point>()(v.first()) * 1024 + 
				hash<Point>()(v.second());
		}
	}
}

