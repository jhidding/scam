#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../base/common.hh"
#include <cmath>
#include <iostream>

using namespace Scam;

Maybe<double> inv(double a)
{
	if (fabs(a) < 1e-6) return Nothing;
	else return Just<double>(1./a);
}

Test::Unit _test_Maybe(
	"000", "Maybe monad, a way to return a value that may turn out to be nothing.",
	[] ()
{
	std::cout << "Trying to divide by 0 ... ";
	auto a = inv(0);
	if (a)
	{
		std::cout << " error, got: " << *a << "\n";
	}
	else
	{
		std::cout << " got Nothing.\n";
	}

	std::cout << "Trying to divide by 10 ... ";
	auto b = inv(10);
	if (b)
	{
		std::cout << " ok, got " << *b << "\n";
	}
	else
	{
		std::cout << " error, got Nothing.\n";
	}

	return b and not a;
});

#endif

