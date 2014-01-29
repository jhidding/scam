#ifdef UNITTEST
#include <iostream>
#include "../base/unittest.hh"
#include "../geometry/surface.hh"
#include "../geometry/polygon.hh"

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


std::ostream &operator<<(std::ostream &out, Polygon const &p)
{
	for (Vertex const &v : p)
		out << "(" << v << ") ";
	return out;
}

Test::Unit _test_Splitting(
	"G03", "Splitting of polygons.",
	[] ()
{
	double P[4][3] = {{0,0,0}, {1,0,0}, {1,1,0}, {0,1,0}};
	auto V = make_ptr<std::vector<Vertex>>();
	for (double *a : P)
	{
		V->push_back(Vertex(Point(a, a+3)));
	}
	auto  square = make_ptr<Polygon>(V);

	Sphere S1(Point(0.0, 0.2, 0.2), 1.0);
	Plane  P1(Point(0.75, 0.75, 0.5), Vector(1, 1, 0).normalize());

	auto a = P1.split_polygon(square);
	std::cout << "below: ";
	if (a.first)
		for (auto p : *a.first) std::cout <<"(" << p << ") ";
	else
		std::cout << "Nothing";
	std::cout << "\nabove: ";
	if (a.second)
		for (auto p : *a.second) std::cout <<"(" << p << ") ";
	else
		std::cout << "Nothing";
	std::cout << "\n";

	auto b = S1.split_polygon(square);
	std::cout << "below: ";
	if (b.first)
		for (auto p : *b.first) std::cout <<"(" << p << ") ";
	else
		std::cout << "Nothing";
	std::cout << "\nabove: ";
	if (b.second)
		for (auto p : *b.second) std::cout <<"(" << p << ") ";
	else
		std::cout << "Nothing";
	std::cout << "\n";

	return true;
});

#endif

