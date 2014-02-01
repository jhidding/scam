#ifdef UNITTEST
#include "../ply/ply.hh"
#include "../ply/read.hh"

#include "../base/unittest.hh"
#include "../render/render.hh"
#include "../geometry/geometry.hh"

using namespace Scam;

Scam::Array<Scam::Vertex> read_vertices(std::istream &fi, Scam::ptr<PLY::PLY> ply, PLY::Format format)
{
	Scam::Array<Scam::Vertex> result;
	auto block = PLY::read_element(fi, (*ply)["vertex"], format);
	
	for (auto const &item : block)
	{
		double x = item.get<double>("x"),
		       y = item.get<double>("y"),
		       z = item.get<double>("z");

		result.push_back(Scam::Vertex(x, y, z));
	}

	return result;
}

Scam::Array<Scam::Polygon> read_polygons(std::istream &fi, Scam::ptr<PLY::PLY> ply, Scam::Array<Scam::Vertex> vertices, PLY::Format format)
{
	Scam::Array<Scam::Polygon> result;
	auto block = PLY::read_element(fi, (*ply)["face"], format);
	size_t i = 0;
	for (auto const &item : block)
	{
		auto indices = item.get_vector<unsigned>("vertex_indices");
		auto V = Scam::map([vertices] (int i) { return vertices[i]; }, indices);
		result.push_back(Polygon(V));
	}

	return result;
}

Test::Unit _test_PLY_read(
	"PLY01", "Reading a .ply file.",
	[] ()
{
	auto ply = make_ptr<PLY::PLY>();

	std::ifstream fi("test/stanford_bunny.ply");
	PLY::Format format = PLY::read_header(fi, ply);
	ply->print_header(std::cout, PLY::BINARY);
	auto v = read_vertices(fi, ply, format);
	std::cout << "read " << v.size() << " vertices.\n";
	auto polygons = read_polygons(fi, ply, v, format);
	std::cout << "read " << polygons.size() << " polygons.\n";

	double cx = 0, cy = 0, cz = 0;
	for (Point p : v)
	{
		cx += p.x(); cy += p.y(); cz += p.z();
	}
	Point centre(cx/v.size(), cy/v.size(), cz/v.size());
	std::cout << "centre of object lies at " << centre << "\n";
	double mar=0;
	for (Point p : v)
		mar = std::max((p - centre).sqr(), mar);
	mar = sqrt(mar);

	Array<RenderObject> scene;
	scene.push_back(RenderObject(polygons, [] (Plane const &P, Context cx)
	{
		double s = P.normal() * Vector(0, 0, 1);
		cx->set_source_rgb(1,fabs(s),fabs(s));
		cx->fill_preserve();
		cx->set_source_rgb(0,0,0);
		cx->set_line_width(0.0001);
		cx->stroke();
	}));
	
	Camera C(
		Point(-1.0, 0.5, 1.0), centre, Vector(0, -1, 0),
			parallel_projection);
	

	auto R = Renderer::SVG(300, 300, "bunny.svg");
	R->apply([mar] (Context cx)
	{
		double L = 2;
		double N = 300;
		cx->scale(N/L,N/L);
		cx->translate(L/2, L/2);
		cx->set_source_rgb(0.5,0.5,0.5);
		cx->arc(0,0,1.0,0,6.283184);
		cx->set_line_width(0.01);
		cx->stroke();
		cx->scale(1./mar, 1./mar);
		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	});

	R->render(scene, C);
	R->finish();
	std::cout << std::endl;

	return true;
});

#endif
