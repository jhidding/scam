#pragma once
#include <iostream>
#include "../geometry/geometry.hh"

namespace Scam
{
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

		public:
			Camera(	Point const 		&position, 
				Point const 		&target, 
				Vector const 		&shub, 
				Projection const 	&p_):
				P(p_)
			{
				T = [position] (Point const &p) { return position - p; };

				Vector 	line_of_sight = T(target).normalize();
				Point	origin = Point(0, 0, 0);
				Vector  z_axis = Vector(0, 0, 1);

				/* The line of sight has to become the z-axis in the new
				 * coordinate system. So we have to rotate around the cross
				 * product of z-axis and l.o.s.
				 */
				Vector	adjust = Vector::cross(z_axis, line_of_sight);
				Quat	pitch = Quat::rotation(adjust, acos(z_axis * line_of_sight));

				/* The shub should point up! (think "should be up"-hub),
				 * this should not be in the line of sight
				 */
				Vector  ps = pitch(shub);
				double	roll_angle = atan2(ps.x(), ps.y());
				Quat	roll = Quat::rotation(z_axis, roll_angle);

				R = roll * pitch;
			}

			Vector rotate(Vector const &v) const { return R(v); }
			Point  project(Point const &p) const { return P(R(T(p))); }

			Point operator()(Point const &p) const { return project(p); }
			Vector operator()(Vector const &v) const { return rotate(v); }
			Plane operator()(Plane const &p) const { return Plane(project(p.origin()), rotate(p.normal())); }
	};

	Point parallel_projection(Vector const &v)
	{
		return Point(v.x(), v.y(), v.z());
	}
}

