#pragma once
#include <tuple>
#include "../geometry/geometry.hh"
#include "camera.hh"

namespace Scam
{
	using Projection = std::function<Point (Vector const &)>;

	inline void Aitoff_Hammer(double ra, double dec, double &x, double &y)
	{
		double denom = sqrt(1 + (cos(dec) * cos(ra/2)));
		           x = 2 * sqrt(2) * cos(dec) * sin(ra/2) / denom;
		           y = sqrt(2) * sin(dec) / denom;
	}

	inline void cartesian_to_spherical(
		double x, double y, double z,
		double &R, double &ra, double &dec)
	{
		  R = sqrt(x*x + y*y + z*z);
		 ra = atan2(x, z);
		dec = atan2(y, sqrt(x*x + z*z));
	}

	class Map_projection_splitter
	{
		Plane cut, shd;

		public:
			Map_projection_splitter(Point const &origin, Point const &target, Vector const &shub):
				cut(origin, Vector::cross(shub, target - origin)),
				shd(origin, origin - target) {}

			std::tuple<Maybe<Polygon>, Maybe<Polygon>, Maybe<Polygon>> 
			operator()(Polygon const &P) const
			{
				using return_type = 
					std::tuple<Maybe<Polygon>, Maybe<Polygon>, Maybe<Polygon>>;

				if (shd.is_below(P))
					return return_type(Nothing, Just(P), Nothing);

				auto split = cut.split_polygon(P);

				return return_type(split.first, Nothing, split.second);
			}
			
			std::tuple<Maybe<Segment>, Maybe<Segment>, Maybe<Segment>> 
			operator()(Segment const &P) const
			{
				using return_type = 
					std::tuple<Maybe<Segment>, Maybe<Segment>, Maybe<Segment>>;

				if (shd.is_below(P))
					return return_type(Nothing, Just(P), Nothing);

				auto split = cut.split_segment(P);

				return return_type(split.first, Nothing, split.second);
			}
	};

	class Map_projection
	{
		using function = std::function<void (double,double,double&,double&)>; 
		function m_projection;
		
		public:
			Map_projection(function const &f):
				m_projection(f) {}

			Point operator()(Vector const &v) const
			{
				return middle(v);
			}

			Point left(Vector const &q) const
			{
				double R, ra, dec, u, v;
				cartesian_to_spherical(-q.z(), q.y(), q.x(), R, ra, dec);
				m_projection(ra + M_PI/2, dec, u, v);
				return Point(u, v, R);
			}

			Point right(Vector const &q) const
			{
				double R, ra, dec, u, v;
				cartesian_to_spherical(q.z(), q.y(), -q.x(), R, ra, dec);
				m_projection(ra - M_PI/2, dec, u, v);
				return Point(u, v, R);
			}

			Point middle(Vector const &q) const
			{
				double R, ra, dec, u, v;
				cartesian_to_spherical(q.x(), q.y(), q.z(), R, ra, dec);
				m_projection(ra, dec, u, v);
				return Point(u, v, R);
			}
	};

	class Map_projection_camera: public Camera
	{
		Map_projection_splitter m_split;
		Map_projection m_project;

		public:
			Map_projection_camera(
				Point const 		&position, 
				Point const 		&target, 
				Vector const 		&shub, 
				Map_projection const 	&p_):

				Camera(position, target, shub, p_),
				m_split(position, target, shub),
				m_project(p_)
			{}

			Plane operator()(Plane const &p) const
			{
				return Plane(
					m_project.middle(rotate(translate(p.origin()))),
					at_point(p.origin(), p.normal()) );
			}

			Array<Path> operator()(Polygon const &P) const
			{
				Array<Path> A;
				auto S = m_split(P);

				if (std::get<0>(S))
				{
					Path p(true);
					for (Vertex const &v : *std::get<0>(S))
						p.push_back(m_project.right(rotate(translate(v))));
					A.push_back(p);
				}

				if (std::get<1>(S))
				{
					Path p(true);
					for (Vertex const &v : *std::get<1>(S))
						p.push_back(m_project.middle(rotate(translate(v))));
					A.push_back(p);
				}

				if (std::get<2>(S))
				{
					Path p(true);
					for (Vertex const &v : *std::get<2>(S))
						p.push_back(m_project.left(rotate(translate(v))));
					A.push_back(p);
				}

				return A;
			}

			Array<Path> operator()(Segment const &S) const
			{
				Array<Path> A;
				auto Q = m_split(S);

				if (std::get<0>(Q))
				{
					Path p(false);
					p.push_back(m_project.right(rotate(translate(std::get<0>(Q)->first()))));
					p.push_back(m_project.right(rotate(translate(std::get<0>(Q)->second()))));
					A.push_back(p);
				}

				if (std::get<1>(Q))
				{
					Path p(false);
					p.push_back(m_project.middle(rotate(translate(std::get<1>(Q)->first()))));
					p.push_back(m_project.middle(rotate(translate(std::get<1>(Q)->second()))));
					A.push_back(p);
				}

				if (std::get<2>(Q))
				{
					Path p(false);
					p.push_back(m_project.left(rotate(translate(std::get<2>(Q)->first()))));
					p.push_back(m_project.left(rotate(translate(std::get<2>(Q)->second()))));
					A.push_back(p);
				}

				return A;
			}
	};
}
