#ifdef UNITTEST
#include <iostream>
#include <cmath>

#include "../material/colour.hh"
#include "../base/unittest.hh"

using namespace Scam;

inline bool eq(double a, double b)
{
	return fabs(b - a) < 1e-4;
}

inline std::string is_ok(bool a)
{
	return (a ? "[\033[32mOK\033[m]" : "[\033[31mEek!\033[m]");
}

Test::Unit _test_Colour(
	"M01", "Conversion between HSV and RGB spaces.",
	[] ()
{
	Colour a = Colour::RGB(0.5, 0, 0), b = Colour::HSV(0.5, 1, 1), c = Colour::HSV(1./3, 0.5, 1),
		d = Colour::RGB(0.3, 0.3, 0.6);

	bool r1 = (eq(a.h(), 0) and eq(a.v(), 0.5) and eq(a.s(), 1.0)),
             r4 = (eq(d.h(), 2./3) and eq(d.v(), 0.6) and eq(d.s(), 0.5)),
	     r2 = (eq(b.r(), 0) and eq(b.g(), 1.0) and eq(b.b(), 1.0)),
	     r3 = (eq(c.r(), 0.5) and eq(c.g(), 1.0) and eq(c.b(), 0.5));

	std::cout << "A: (rgb) " << a.r() << " " << a.g() << " " << a.b() << ", "
		"(hsv) " << a.h() << " " << a.s() << " " << a.v() << " " << is_ok(r1) << std::endl;
	std::cout << "D: (rgb) " << d.r() << " " << d.g() << " " << d.b() << ", "
		"(hsv) " << d.h() << " " << d.s() << " " << d.v() << " " << is_ok(r4) << std::endl;
	std::cout << "B: (rgb) " << b.r() << " " << b.g() << " " << b.b() << ", "
		"(hsv) " << b.h() << " " << b.s() << " " << b.v() << " " << is_ok(r2) << std::endl;
	std::cout << "C: (rgb) " << c.r() << " " << c.g() << " " << c.b() << ", "
		"(hsv) " << c.h() << " " << c.s() << " " << c.v() << " " << is_ok(r3) << std::endl;
	return r1 and r2;
});
#endif

