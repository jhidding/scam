#pragma once
#include <iostream>
#include "../geometry/geometry.hh"

namespace Scam
{
	class Path: public Array<Point>
	{
		bool m_closed;

		public:
			Path(bool closed_ = true): m_closed(closed_) {}

			bool closed() const { return m_closed; }
	};

	class Camera
	{
		public:
			using Translation = std::function<Vector (Point const &)>;
			using Rotation = std::function<Vector (Vector const &)>;
			using Projection = std::function<Point (Vector const &)>;

		private:
			Translation T;
			Rotation    R;
			Projection  P;

			Point  position, target;
			Vector shub;

		public:
			Camera(	Point const 		&position_, 
				Point const 		&target_, 
				Vector const 		&shub_, 
				Projection const 	&p_):
				P(p_), position(position_), target(target_), shub(shub_)
			{
				T = [position_] (Point const &p) { return p - position_; };

				Vector 	line_of_sight = T(target).normalize();
				Point	origin = Point(0, 0, 0);
				Vector  z_axis = Vector(0, 0, 1);

				/* The line of sight has to become the z-axis in the new
				 * coordinate system. So we have to rotate around the cross
				 * product of z-axis and l.o.s.
				 */
				Vector	adjust = Vector::cross(-z_axis, line_of_sight).normalize();
				Quat	pitch = Quat::rotation(adjust, acos(z_axis * line_of_sight));

				/* The shub should point up! (think "should be up"-hub),
				 * this should not be in the line of sight
				 */
				Vector  ps = pitch(shub);
				double	roll_angle = atan2(ps.x(), ps.y());
				Quat	roll = Quat::rotation(z_axis, roll_angle);

				R = roll * pitch;
			}

			Vector at_point(Point const &p, Vector const &v) const
			{
				Vector	line_of_sight = T(p).normalize();
				Point	origin = Point(0, 0, 0);
				Vector  z_axis = Vector(0, 0, 1);
				Vector	adjust = Vector::cross(-z_axis, line_of_sight).normalize();
				Quat	pitch = Quat::rotation(adjust, acos(z_axis * line_of_sight));
				Vector  ps = pitch(shub);
				double	roll_angle = atan2(ps.x(), ps.y());
				Quat	roll = Quat::rotation(z_axis, roll_angle);
				return (roll * pitch)(v);
			}

			Vector translate(Point const &p) const { return T(p); }
			Vector rotate(Vector const &v) const { return R(v); }
			Point  project(Vector const &v) const { return P(v); }

			virtual Point operator()(Point const &p) const { return P(R(T(p))); }
			virtual Vector operator()(Vector const &v) const { return R(v); }
			virtual Plane operator()(Plane const &p) const { return Plane(P(R(T(p.origin()))), R(p.normal())); }

			virtual Array<Path> operator()(Polygon const &Q) const
			{
				Path A(true);
				for (Point const &p : Q) 
					A.push_back(P(R(T(p))));

				Array<Path> B; B.push_back(A);
				return B;
			}

			virtual Array<Path> operator()(Segment const &Q) const
			{
				Path A(true);
				A.push_back(P(R(T(Q.first()))));
				A.push_back(P(R(T(Q.second()))));

				Array<Path> B; B.push_back(A);
				return B;
			}
	};

	inline Point parallel_projection(Vector const &v)
	{
		return Point(v.x(), v.y(), v.z());
	}
}

