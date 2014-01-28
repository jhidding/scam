#ifdef UNITTEST
#include <iostream>
#include "../base/unittest.hh"
#include "../geometry/surface.hh"

using namespace Scam;

Test::Unit _test_Surface(
	"G02", "Surfaces in this context are things that you can intersect "
	"line segments with. This notion is limited to finding a single "
	"intersection point if point a is below and point b above the surface, "
	"or vice versa.",
	[] ()
{
	Sphere S1(Point(0,0,0), 1.0);
	auto a = S1.intersect(Point(3./5, 0, 0), Point(3./5, 1.0, 0));
	auto b = S1.intersect(Point(6./5, 0, 0), Point(3./5, 1.0, 0));

	std::cout << "Intersection between unit sphere and (3/5,0,0)-(3/5,1,0): " << a << "\n";
	std::cout << "Intersection between unit sphere and (6/5,0,0)-(3/5,1,0): " << b << "\n";

	return a and not b;
});

#endif

