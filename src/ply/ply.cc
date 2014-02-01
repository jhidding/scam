#include "ply.hh"

#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../base/mvector.hh"
#include "../base/mdrange.hh"
#include "../base/map.hh"

using namespace Scam;

Test::Unit PLY_test(
	"1000:PLY",
	"Testing the writing of a correct .ply file. "
	"If this test succeeds, it should write a correct "
	"mesh to a test file called ply_test.ply. ",
	[] ()
{
	PLY::PLY ply;
	int N = 50;

	auto grid = lazy_map(MdRange<2>(mVector<int,2>(N)),
		[N] (mVector<int,2> const &X) { 
		return X.as<double>() * (2./N) - mVector<double,2>(1.0);
	});

	auto func = [] (mVector<double,2> const &X) {
		double v = X.norm();
		return (v == 0 ? 1.0 : sin(v * 4*M_PI) / (v * 4*M_PI));
	};

	ply.add_comment("file written as test to DMT3D .ply writer");
	ply.add_element("vertex", 
		PLY::scalar_type<float>("x"), 
		PLY::scalar_type<float>("y"), 
		PLY::scalar_type<float>("z"),
		PLY::scalar_type<uint8_t>("red"),
		PLY::scalar_type<uint8_t>("green"),
		PLY::scalar_type<uint8_t>("blue")
	);
	for (auto p : grid)
	{
		double v = p.norm();
		auto q = p/v;
		double phase = atan2(q[0], q[1]);
		if (phase < 0) phase += 2*M_PI;
		if (phase > 2*M_PI) phase -= 2*M_PI;

		ply.put_data(
			PLY::scalar<float>(p[0]), 
			PLY::scalar<float>(p[1]), 
			PLY::scalar<float>(func(p)),
			PLY::scalar<uint8_t>((phase > M_PI ? 
				0 : 1.0 - 2 * fabs(phase/M_PI - 0.5))*255),
			PLY::scalar<uint8_t>(((phase > (5./3 * M_PI)) or (phase < (2./3*M_PI)) ? 
				0 : 1.0 - 2 * fabs(phase/M_PI - 1.1667))*255),
			PLY::scalar<uint8_t>(((phase > (1./3 * M_PI)) and (phase < (4./3*M_PI)) ? 
				0 : 1.0 - 2 * fabs((phase < (1./3 * M_PI) ? phase + 2*M_PI : phase)/M_PI - 1.8333))*255)
		);
	}

	ply.add_element("face",
		PLY::list_type<unsigned>("vertex_indices"));
	mVector<int,2> dx({0, 1}), dy({1, 0});
	for (auto p : MdRange<2>(mVector<int,2>(N-1)))
	{
		std::vector<unsigned> v1, v2;
		v1.push_back(p[0] +     N * p[1]);
		v1.push_back(p[0] + 1 + N * p[1]);
		v1.push_back(p[0] + 1 + N * p[1] + N);
		ply.put_data(PLY::list<unsigned>(v1));
		v2.push_back(p[0] + 1 + N * p[1] + N);
		v2.push_back(p[0] +     N * p[1] + N);
		v2.push_back(p[0] +     N * p[1]);
		ply.put_data(PLY::list<unsigned>(v2));
	}

	ply.write("ply_test.ply", PLY::ASCII);

	return true;
});

#endif

void PLY::PLY::write(std::string const &filename, Format format) const
{
	m_header.set_format(format);
	m_data.set_format(format);
	std::ofstream out(filename);
	out << m_header;
	out << m_data;
	out.close();
}

void PLY::PLY::print_header(std::ostream &out, Format format) const
{
	m_header.set_format(format);
	out << m_header;
}

