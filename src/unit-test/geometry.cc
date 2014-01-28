#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../geometry/quaternion.hh"

using namespace Scam;

inline bool eq(double a, double b)
{
	return fabs(b - a) < 1e-4;
}

Test::Unit _test_Geometry(
	"G00", "Basic geometric functionality, the working of Points, Vectors and Quaternions.",
	[] () 
{
	auto R = Quat::rotation(Vector(0,0,1), M_PI/2);
	Vector b(1,0,0);
	Vector a = R(b); // should return 0, 1, 0
	std::cout << b << " rotated around " << R.v() << " -> " << a << std::endl;
	return (eq(a.x(), 0) and eq(a.y(), 1) and eq(a.z(), 0));
});

#include <unordered_map>
#include "../geometry/vertex.hh"

Test::Unit _test_Geometry_vertex(
	"G01", "Vertices encapsulate a point with a tag that gives it a hash, making "
	"it uniquely identifiable.",
	[] ()
{
	std::unordered_map<Vertex, std::string> A;
	Vertex Q1(Point(0, 0, 0)), Q2(Point(1, 0, 0)), Q3(Point(0, 1, 1));
	A[Q1] = "Hello"; A[Q2] = ", "; A[Q3] = "World!";
	std::string B = A[Q1] + A[Q2] + A[Q3];

	std::cout << B << std::endl;

	return B == "Hello, World!";
});
#endif

