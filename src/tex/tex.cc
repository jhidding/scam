#include "tex.hh"
#include "../base/common.hh"

using namespace Scam;

#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../geometry/geometry.hh"
#include <cassert>

Test::Unit _test_TeX(
	"TEX00", "Testing capability of rendering LaTeX.",
	[] ()
{
	TeX A("\\renewcommand\\vec[1]{\\boldsymbol{#1}}");
	A << "\\[\\color{Maroon}\\frac{\\partial \\vec{u}}{\\partial t} + (\\vec{u} "
	     "\\cdot \\vec{\\nabla})\\vec{u} = \\nu\\nabla^2 \\vec{u}\\]\n";
	
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
		cx->set_source_rgba(0,0,1,1.0-fabs(s));
		cx->fill_preserve();
		cx->set_source_rgb(0,0,0);
		cx->set_line_width(0.01);
		cx->stroke();
	})));

	auto C = make_ptr<Camera>(
		Point(3, 2, 1), Point(0.5,0.5,0.5), Vector(0, 0, -1),
		parallel_projection);
		
	auto R = Renderer::SVG(300, 300, "tex-cube.svg");
	R->apply([] (Context cx)
	{
		cx->scale(200, 200);
		cx->translate(0.75,0.75);
		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	});

	R->render(scene, C);

	auto eqn = A.svg();
        assert(eqn->ok());
	R->apply([eqn] (Context cx)
	{
		double w = eqn->width(), h = eqn->height();
		cx->save();
		cx->translate(-0.6, -0.1);
		cx->scale(0.7/w, 0.7/w);
		cx->rectangle(-w/20, -w/20, 1.1*w, 0.1*w + h);
		cx->set_source_rgba(1,1,1,0.7);
		cx->fill_preserve();
		cx->set_line_width(w/100);
		cx->set_source_rgb(0,0,0);
		cx->stroke();
		eqn->render(cx);
		cx->restore();
	});
	R->finish();
	std::cout << std::endl;

	return true;
});

#endif

Scam::SvgFile::SvgFile(std::string const &fn):
	m_ok(false)
{
	GError *err = NULL;
	m_handle = rsvg_handle_new_from_file(fn.c_str(), &err);
	if (m_handle != NULL)
	{
		m_ok = true;
		rsvg_handle_get_dimensions(m_handle, &m_dimension);
	}
}

bool Scam::SvgFile::ok() const { return m_ok; }

Scam::SvgFile::~SvgFile()
{
	GError *err = NULL;
	rsvg_handle_close(m_handle, &err);
	g_object_unref(m_handle);
}

double Scam::SvgFile::width() const { return m_dimension.width; }
double Scam::SvgFile::height() const { return m_dimension.height; }

void Scam::SvgFile::render(Scam::Context cx) const
{
	rsvg_handle_render_cairo(m_handle, cx->cobj());
}

inline int msystem(std::string const &s)
{
	return system(s.c_str());
}

void Scam::TeX::make_svg() const
{
	std::ofstream fo(fn_prefix + ".tex");
	fo << "\\documentclass{article}\n"
	   << "\\usepackage{amsmath}\n"
	   << "\\usepackage{amssymb}\n"
	   << "\\usepackage[bitstream-charter]{mathdesign}\n"
	   << "\\usepackage[T1]{fontenc}\n"
	   << "\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}\n"
	   << "\\pagestyle{empty}\n";

	fo << preamble << "\n";

	fo << "\\begin{document}\n"
	   << str()
	   << "\\end{document}\n";

	fo.close();

	char *pwd = get_current_dir_name();
	int err;
	err = chdir("/tmp");
	msystem("pdflatex " + fn_prefix + " > /dev/null");
	msystem("pdfcrop " + fn_prefix + ".pdf " + fn_prefix + ".pdf > /dev/null");
	msystem("pdftocairo -svg " + fn_prefix + ".pdf ");
	err = chdir(pwd);
	free(pwd);
}

inline std::string hex_code(unsigned i)
{
	std::ostringstream ss;
	ss << std::setbase(16) << std::setfill('0') << std::setw(8) << i;
	return ss.str();
}

Scam::TeX::TeX(std::string const preamble_, unsigned code):
	preamble(preamble_),
	fn_prefix("/tmp/scam-" + hex_code(code))
{}

void Scam::TeX::reset() 
{ 
	str("");
	msystem("rm " + fn_prefix + ".*");
}

Scam::ptr<Scam::SvgFile> Scam::TeX::svg() const
{
	make_svg();
	return make_ptr<SvgFile>(fn_prefix + ".svg");
}

Scam::TeX::~TeX() 
{ 
	//reset(); 
}

