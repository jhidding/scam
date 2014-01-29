#pragma once
#include <unordered_map>
#include <utility>
#include "../base/common.hh"
#include "point.hh"
#include "vector.hh"
#include "polygon.hh"

namespace Scam
{
	class Surface
	{
		public:
			virtual double distance(Point const &p) const = 0;
			virtual Maybe<Point> intersect(Point const &a, Point const &b) const = 0;

			bool is_below(Point const &a) const;

			std::pair<Maybe<Polygon>, Maybe<Polygon>> split_polygon(
				ptr<Polygon> p) const;
			std::pair<Maybe<Polygon>, Maybe<Polygon>> split_polygon(
				ptr<std::unordered_map<Segment,Vertex>> cache, ptr<Polygon> p) const;

		private:
	};

	class Plane: public Surface
	{
		Point 	m_origin;
		Vector	m_normal;

		public:
			Plane(Point origin_, Vector normal_):
				m_origin(origin_), m_normal(normal_)
			{}

			Point const &origin() const
			{
				return m_origin;
			}

			Vector const &normal() const
			{
				return m_normal;
			}

			double distance(Point const &p) const;
			Maybe<Point> intersect(Point const &a, Point const &b) const;
	};

	class Sphere: public Surface
	{
		Point 	m_origin;
		double	m_radius;

		public:
			Sphere(Point origin_, double radius_):
				m_origin(origin_), m_radius(radius_)
			{}

			double radius() const
			{
				return m_radius;
			}

			Point const &origin() const
			{
				return m_origin;
			}

			double distance(Point const &p) const;
			Maybe<Point> intersect(Point const &a, Point const &b) const;
	};
}

