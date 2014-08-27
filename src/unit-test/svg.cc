#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../base/common.hh"
#include "../geometry/geometry.hh"
#include "../render/render.hh"

using namespace Scam;

Test::Unit _test_SVG(
	"R01", "Render a simple scene to SVG.",
	[] ()
{
	Array<Vertex> vertices = {
		Point(0, 0, 0), Point(0, 0, 1), Point(0, 1, 0), Point(0, 1, 1),
		Point(1, 0, 0), Point(1, 0, 1), Point(1, 1, 0), Point(1, 1, 1) };

	Array<Array<int>> facets = {
		{ 0, 2, 3, 1 }, { 4, 5, 7, 6 },
		{ 0, 1, 5, 4 }, { 3, 2, 6, 7 },
		{ 5, 1, 3, 7 }, { 2, 0, 4, 6 } };

	Array<Polygon> polygons;
	for (Array<int> f : facets)
	{
		auto V = map([vertices] (int i) { return vertices[i]; }, f);
		polygons.push_back(Polygon(V));
	}

	Array<ptr<RenderObject>> scene;
	scene.push_back(ptr<RenderObject>(new PolygonObject(
		polygons, [] (Info I, Context cx)
	{
		auto s_ = I.get<double>("incidence");
		double s = 0.0; if (s_) s = *s_;
		cx->set_source_rgba(1,0,0,1.0-fabs(s));
		cx->fill_preserve();
		cx->set_source_rgb(0,0,0);
		cx->set_line_width(0.01);
		cx->stroke();
	})));

	auto C = make_ptr<Camera>(
		Point(3, 2, 1), Point(0.5,0.5,0.5), Vector(0, 0, -1),
		parallel_projection);
		
	auto R = Renderer::SVG(300, 300, "cube.svg");
	R->apply([] (Context cx)
	{
		cx->scale(200, 200);
		cx->translate(0.75,0.75);
		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	});

	R->render(scene, C);
	R->finish();
	std::cout << std::endl;

	return true;
});

#endif
