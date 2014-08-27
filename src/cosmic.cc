#include "base/common.hh"
#include "base/argv.hh"
#include "base/format.hh"
#include "ply/ply.hh"
#include "ply/read.hh"
#include "geometry/geometry.hh"
#include "render/render.hh"
#include "render/map_projection.hh"
#include "two_mass.hh"
#include "material/colour.hh"
#include "tex/tex.hh"

#include <ctime>
#include <sstream>
#include <iomanip>

using namespace System;
using namespace Scam;
using namespace TwoMass;

std::string date_string()
{
	time_t tsec = time(NULL);
	struct tm *T = gmtime(&tsec);
	std::ostringstream s;
	s << std::setfill('0') 
		<< std::setw(2) << T->tm_year - 100
		<< std::setw(2) << T->tm_mon + 1 
		<< std::setw(2) << T->tm_mday; 
	return s.str();
}

std::string seconds_since_epoch_string()
{
	std::ostringstream ss;
	ss << time(NULL);
	return ss.str();
}

std::string time_string(double t)
{
	std::ostringstream s;
	s << std::setfill('0') << std::setw(5) << static_cast<int>(round(t * 10000));
	return s.str();
}


std::string timed_filename(std::string const &id, std::string const &stage, float b, std::string const &ext)
{
	std::ostringstream s;

	if (b < 0.0)
	{
		s 	<< id << "." << stage << "." << "init" 
			<< "." << ext;
	}
	else
	{
		s 	<< id << "." << stage << "." << std::setfill('0') << std::setw(5)
			<< static_cast<int>(round(b * 10000)) 
			<< "." << ext;
	}
	return s.str();
}

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
		std::vector<unsigned> indices = item.get_vector<unsigned>("vertex_index");
		double density = item.get<double>("density");
		auto V = Scam::map([vertices] (int i) { return vertices[i]; }, indices);
		Polygon P(V);
		P.set_info("density", density);
		result.push_back(P);
	}

	return result;
}

Scam::Array<Scam::Segment> read_segments(std::istream &fi, Scam::ptr<PLY::PLY> ply, Scam::Array<Scam::Vertex> vertices, PLY::Format format)
{
	Scam::Array<Scam::Segment> result;
	auto block = PLY::read_element(fi, (*ply)["edge"], format);
	size_t i = 0;
	for (auto const &item : block)
	{
		unsigned v1 = item.get<unsigned>("vertex1");
		unsigned v2 = item.get<unsigned>("vertex2");
		double density = item.get<double>("density");
		Segment S(vertices[v1], vertices[v2]);
		S.set_info("density", density);
		result.push_back(S);
	}

	return result;
}

Array<Vertex> read_abell(std::string const &fn)
{
	std::cerr << "Reading Abell cluster catalog upto redshift 0.03 ... ";
	Array<Vertex> A;
	std::ifstream fi(fn);
	while (!fi.eof())
	{
		Abell::Cluster C; fi >> C;
		if (C.A_ID == "") continue;

		double sg_ra, sg_dec;
		ga_to_sg(radians(C.l), radians(C.b), sg_ra, sg_dec);
		Vertex v(Point(90,90,90) + spherical_to_cartesian(sg_ra, sg_dec, 1.0));
		v.set_info("r", C.z * 2997.92458);

		if (C.name != "")
			v.set_info("name", C.name);
		else
			v.set_info("name", "A"+C.A_ID);

		v.set_info("ox", C.ox); v.set_info("oy", C.oy);

		A.push_back(v);
	}

	std::cerr << "Ok\n";
	return A;
}

std::function<void (Context)> prepare_context(unsigned w, unsigned h, double r)
{
	return [w, h, r] (Context cx)
	{
		double L = 6;
		double margin = (h * (L/w) - L/2)/2;
		cx->scale(w/L,w/L);
		cx->translate(L/2, L/4 + margin);
		cx->set_source_rgb(0.5,0.5,0.5);
		cx->set_line_width(0.01);

		cx->save();
		cx->scale(2*sqrt(2), sqrt(2));
		cx->arc(0,0,1.0,0,6.283184);
		cx->stroke();
		cx->restore();

		cx->save();
		cx->translate(-L/2, L/4 - 2*margin);
		cx->scale(margin*3, margin*3);
		cx->rectangle(0,0, 2,1);
		cx->set_line_width(0.001);
		cx->set_source_rgb(1,0,1);
		cx->stroke();

		cx->move_to(0.9,0.24);
		cx->rel_curve_to(0.01, 0.03, 0.02, 0.04, 0.05, 0.05);
		cx->rel_line_to(1.0, 0.0);
		cx->rel_curve_to(0.03, -0.01, 0.04, -0.02, 0.05, -0.05);
		cx->rel_line_to(0.0, 0.12);
		cx->rel_curve_to(-0.01, -0.03, -0.02, -0.04, -0.05, -0.05);
		cx->rel_line_to(-1.0, 0.0);
		cx->rel_curve_to(-0.03, 0.01, -0.04, 0.02, -0.05, 0.05);
		cx->rel_line_to(0.0, -0.12);
		cx->close_path();
		cx->set_source_rgb(0,0,0);
		cx->fill();

		TeX text_ruler; 
		text_ruler << "$\\approx " << int(r) << "\\ {\\rm Mpc\\ h^{-1}}$\n";
		auto svg = text_ruler.svg();
		double u = svg->width(), v = svg->height(), H = 0.15;
		cx->save();
		cx->translate(0.9 + 0.55 - u/2*H/v, 0.24 + 0.03 - H);
		cx->scale(H/v, H/v);
		svg->render(cx);
		cx->restore();
		cx->restore();

		cx->save();
		cx->translate(L/2-margin*6, L/4 - 2*margin);
		cx->scale(margin*3, margin*3);
		cx->rectangle(0,0, 2,1);
		cx->set_line_width(0.001);
		cx->set_source_rgb(1,0,1);
		cx->stroke();
		cx->restore();

		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	};
}

std::function<void (Context)> prepare_context_rv(unsigned w, unsigned h, double r)
{
	return [w, h, r] (Context cx)
	{
		double L = 6;
		double margin = (h * (L/w) - L/2)/2;
		cx->scale(w/L,w/L);
		cx->translate(L/2, L/4 + margin);
		cx->set_source_rgb(0.5,0.5,0.5);
		cx->set_line_width(0.01);

		cx->save();
		cx->scale(2*sqrt(2), sqrt(2));
		cx->arc(0,0,1.0,0,6.283184);
		cx->restore();
		cx->stroke();

		cx->save();
		cx->translate(-L/2, L/4 - 2*margin);
		cx->scale(margin*3, margin*3);
		cx->rectangle(0,0, 2,1);
		cx->set_line_width(0.001);
		cx->set_source_rgb(1,0,1);
		cx->stroke();

		cx->move_to(0.9,0.24);
		cx->rel_curve_to(0.01, 0.03, 0.02, 0.04, 0.05, 0.05);
		cx->rel_line_to(1.0, 0.0);
		cx->rel_curve_to(0.03, -0.01, 0.04, -0.02, 0.05, -0.05);
		cx->rel_line_to(0.0, 0.12);
		cx->rel_curve_to(-0.01, -0.03, -0.02, -0.04, -0.05, -0.05);
		cx->rel_line_to(-1.0, 0.0);
		cx->rel_curve_to(-0.03, 0.01, -0.04, 0.02, -0.05, 0.05);
		cx->rel_line_to(0.0, -0.12);
		cx->close_path();
		cx->set_source_rgb(1,1,1);
		cx->fill();

		TeX text_ruler; 
		text_ruler << "$\\color{White}\\approx " << int(r) << "\\ {\\rm Mpc\\ h^{-1}}$\n";
		auto svg = text_ruler.svg();
		double u = svg->width(), v = svg->height(), H = 0.15;
		cx->save();
		cx->translate(0.9 + 0.55 - u/2*H/v, 0.24 + 0.03 - H);
		cx->scale(H/v, H/v);
		svg->render(cx);
		cx->restore();
		cx->restore();

		cx->save();
		cx->translate(L/2-margin*6, L/4 - 2*margin);
		cx->scale(margin*3, margin*3);
		cx->rectangle(0,0, 2,1);
		cx->set_line_width(0.001);
		cx->set_source_rgb(1,0,1);
		cx->stroke();
		cx->restore();

		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	};
}

Vertex make_label(double sg_ra, double sg_dec, std::string const &name, double ox, double oy)
{
	Vertex v(Point(90,90,90) + spherical_to_cartesian(sg_ra, sg_dec, 1.0));
	v.set_info("ox", ox);
	v.set_info("oy", oy);
	v.set_info("name", name);
	return v;
}

struct Cluster
{
	double ra, dec, v;
	std::string name;
	double ox, oy;

	bool ok(double v_min, double v_max) const
	{
		return (v + 5.0 > v_min) and (v_max > v - 5.0);
	}

	Vertex vertex() const
	{
		double l, b; eq_to_sg(radians(ra), radians(dec), l, b);
		return make_label(l, b, name, ox, oy);
	}
};

void command_cosmic(int argc_, char **argv_)
{
	Argv argv = read_arguments(argc_, argv_,
		Option({0, "h", "help", "false", 
			"print this help."}),
		Option({0, "2m", "2mass", "false",
			"include 2Mass into rendering."}),
		Option({0, "ac", "abell", "false",
			"include Abell cluster catalog."}),
		Option({0, "r", "reverse", "false",
			"reverse colours."}),
		Option({Option::VALUED | Option::CHECK, "i", "id", date_string(),
			"identifier for filenames."}),
		Option({Option::VALUED | Option::CHECK, "L", "size", "100",
			"size of the box."}),
		Option({Option::VALUED | Option::CHECK, "t", "time", "1.0",
			"growing mode parameter."}),
		Option({Option::VALUED | Option::CHECK, "rs", "rstep", "10",
			"selection radius."}),
		Option({Option::VALUED | Option::CHECK, "dr", "dr", "10",
			"selection radius."}),
		Option({Option::VALUED | Option::CHECK, "f", "fila-lim", "200.0",
			"lower limit of filament density to show."}),
		Option({Option::VALUED | Option::CHECK, "w", "wall-lim", "20.0",
			"lower limit of wall density to show."}) );

	if (argv.get<bool>("help"))
	{
		std::cerr << "Scam -- 3D vector graphics for science.\n"
			"Copyright Johan Hidding, June 2014 - licence: GPL3.\n\n";
		argv.print(std::cerr);
		exit(0);
	}

	/*
	Array<Cluster> cluster_catalog = {
		{ 186.75,  12.72,  10.00, "Virgo cluster",    0.0,  0.3 },

		{  49.50,  41.50,  53.66, "Perseus cluster", -0.2, -0.2 },
		{  28.20,  36.15,  45.30, "A262",             0.1,  0.1 },
		{  36.45,  41.87,  51.60, "A347",             0.2,  -0.03 },

		{ 243.89, -60.91,  47.07, "Norma cluster",    0.2, -0.2 },

		{ 194.95,  27.98,  69.25, "Coma cluster",     0.0,  0.3 }, 
		{ 176.12,  19.84,  65.95, "Leo cluster",      0.0, -0.3 } };
	*/

	double t = argv.get<double>("time");
	double L = argv.get<double>("size");
	double wall_lim = argv.get<double>("wall-lim");
	double fila_lim = argv.get<double>("fila-lim");

	std::string fn_i_wall = Misc::format(argv["id"], ".", time_string(t), ".walls.ply");
	std::string fn_i_fila = Misc::format(argv["id"], ".", time_string(t), ".filam.ply");

	std::cerr << "Reading " << fn_i_wall << " ..." << std::endl;
	auto ply = make_ptr<PLY::PLY>();

	std::ifstream fi(fn_i_wall);
	PLY::Format format = PLY::read_header(fi, ply);
	ply->print_header(std::cout, PLY::BINARY);
	auto v = read_vertices(fi, ply, format);
	std::cout << "read " << v.size() << " vertices.\n";
	auto polygons = read_polygons(fi, ply, v, format);
	std::cout << "read " << polygons.size() << " polygons.\n";

	std::cerr << "Reading " << fn_i_fila << " ..." << std::endl;
	v->clear(); ply = make_ptr<PLY::PLY>(); fi.close();
	
	std::ifstream fi2(fn_i_fila);
	format = PLY::read_header(fi2, ply);
	ply->print_header(std::cout, PLY::BINARY);
	v = read_vertices(fi2, ply, format);
	std::cout << "read " << v.size() << " vertices.\n";
	auto segments = read_segments(fi2, ply, v, format);
	std::cout << "read " << segments.size() << " segments.\n";
	v->clear(); ply.reset(); fi2.close();

	Maybe<Array<Vertex>> galaxies;
	if (argv.get<bool>("2mass"))
		galaxies = Just(read_2mass("2mrs_1175_done.dat"));
	else
		galaxies = Nothing;

	Maybe<Array<Vertex>> clusters;
	if (argv.get<bool>("abell"))
		clusters = Just(read_abell("abell_catalog.tsv"));
	else
		clusters = Nothing;

	double step = argv.get<double>("rstep");
	double dr = argv.get<double>("dr");

	auto cluster_label = (argv.get<bool>("reverse") ?
		[] (Info info, Context cx)
		{
			auto ox_ = info.get<double>("ox"), oy_ = info.get<double>("oy");
			auto name_ = info.get_str("name");
			double ox = 0.15; if (ox_) ox = *ox_;
			double oy = 0.15; if (oy_) oy = *oy_;
			std::string name = "empty-string"; if (name_) name = *name_;

			TeX label_height; label_height << "lg";
			auto svg_height = label_height.svg();

			std::cout << "label: " << name; //<< std::endl;
			TeX label; label << "\\color{White}" << name;
			auto svg = label.svg();
			double s = 0.05 / svg_height->height();
			double w = svg->width() * s;
			double x, y;

			cx->rel_line_to(ox, oy);
			cx->get_current_point(x, y);
			cx->set_line_width(0.007);
			cx->set_source_rgb(0,0,0);
			cx->stroke_preserve();
			cx->set_line_width(0.003);
			cx->set_source_rgb(0,0.5,1);
			cx->stroke();

			std::cout << " " << x << ", " << y << std::endl;
			cx->move_to(x, y);
			cx->rel_move_to(-w/2 - 0.05/4, (oy < 0 ? -0.05 - 0.05/4 : 0.05/4));
			cx->rel_curve_to(0.01/4, -0.03/4, 0.02/4, -0.04/4, 0.05/4, -0.05/4);
			cx->rel_line_to(w, 0);
			cx->rel_curve_to(0.03/4, 0.01/4, 0.04/4, 0.02/4, 0.05/4, 0.05/4);
			cx->rel_line_to(0, 0.05);
			cx->rel_curve_to(-0.01/4, 0.03/4, -0.02/4, 0.04/4, -0.05/4, 0.05/4);
			cx->rel_line_to(-w, 0);
			cx->rel_curve_to(-0.03/4, -0.01/4, -0.04/4, -0.02/4, -0.05/4, -0.05/4);
			cx->rel_line_to(0, -0.05);
			cx->close_path();
			cx->set_source_rgba(0,0.5,1,0.5);
			cx->fill_preserve();
			cx->set_source_rgb(0,0.5,1);
			cx->stroke();

			cx->save();
			cx->translate(x - w/2, (oy < 0 ? y - 0.05 - 0.05/4 : y + 0.05/4));
			cx->scale(s, s);
			svg->render(cx);
			cx->restore();
		} :
		[] (Info info, Context cx)
		{
			auto ox_ = info.get<double>("ox"), oy_ = info.get<double>("oy");
			auto name_ = info.get_str("name");
			double ox = 0.15; if (ox_) ox = *ox_;
			double oy = 0.15; if (oy_) oy = *oy_;
			std::string name = "empty-string"; if (name_) name = *name_;

			TeX label_height; label_height << "lg";
			auto svg_height = label_height.svg();

			std::cout << "label: " << name; //<< std::endl;
			TeX label; label << name;
			auto svg = label.svg();
			double s = 0.05 / svg_height->height();
			double w = svg->width() * s;
			double x, y;

			cx->rel_line_to(ox, oy);
			cx->get_current_point(x, y);
			cx->set_line_width(0.005);
			cx->set_source_rgb(0,0,0);
			cx->stroke_preserve();
			cx->set_line_width(0.003);
			cx->set_source_rgb(0,0.5,1);
			cx->stroke();

			std::cout << " " << x << ", " << y << std::endl;
			cx->move_to(x, y);
			cx->rel_move_to(-w/2 - 0.05/4, (oy < 0 ? -0.05 - 0.05/4 : 0.05/4));
			cx->rel_curve_to(0.01/4, -0.03/4, 0.02/4, -0.04/4, 0.05/4, -0.05/4);
			cx->rel_line_to(w, 0);
			cx->rel_curve_to(0.03/4, 0.01/4, 0.04/4, 0.02/4, 0.05/4, 0.05/4);
			cx->rel_line_to(0, 0.05);
			cx->rel_curve_to(-0.01/4, 0.03/4, -0.02/4, 0.04/4, -0.05/4, 0.05/4);
			cx->rel_line_to(-w, 0);
			cx->rel_curve_to(-0.03/4, -0.01/4, -0.04/4, -0.02/4, -0.05/4, -0.05/4);
			cx->rel_line_to(0, -0.05);
			cx->close_path();
			cx->set_source_rgba(0.5,0.9,1,0.5);
			cx->fill_preserve();
			cx->set_source_rgb(0,0.5,1);
			cx->stroke();

			cx->save();
			cx->translate(x - w/2, (oy < 0 ? y - 0.05 - 0.05/4 : y + 0.05/4));
			cx->scale(s, s);
			svg->render(cx);
			cx->restore();
		});

	/* How to draw a wall */
	auto wall_material = (argv.get<bool>("reverse") ?
		[] (Info info, Context cx)
		{	
			auto s_ = info.get<double>("incidence");
			auto d_ = info.get<double>("density");
			double s, d;
			if (s_) s = fabs(*s_);
			if (d_) d = sqrt(*d_);

			d = std::max(5., d); d = std::min(10., d);
			d = (d - 5.) / 5.;
			auto Z = Colour::HSVA(d*0.1667, 0.9 - d*0.35, 1.0-s/3, 0.5 + d/2 - s/2);
			cx->set_source_rgba(Z.r(), Z.g(), Z.b(), Z.a());
			cx->fill_preserve();
			cx->set_source_rgba(1,1,1,0.18);
			cx->set_line_width(0.001);
			cx->stroke();
		} :
		[] (Info info, Context cx)
		{
			auto s_ = info.get<double>("incidence");
			auto d_ = info.get<double>("density");
			double s, d;
			if (s_) s = fabs(*s_);
			if (d_) d = sqrt(*d_);

			d = std::max(5., d); d = std::min(10., d);
			d = (d - 5.) / 5.;
			auto Z = Colour::HSVA(0.1667 + d/3, 0.5 + s/5 + d*0.3, 1.0 - d*0.5, 0.5 + d/2 - s/2);
			cx->set_source_rgba(Z.r(), Z.g(), Z.b(), Z.a());
			cx->fill_preserve();
			cx->set_source_rgba(0,0,0,0.2);
			cx->set_line_width(0.001);
			cx->stroke();
		});

	/* How to draw a filament */
	auto filament_material = (argv.get<bool>("reverse") ?
		[] (Info info, Context cx)
		{
			auto d_ = info.get<double>("density");
			double d;
			if (d_) d = sqrt(*d_);
			d = std::max(10., d); d = std::min(30., d);
			d = (d - 10.) / 20.;
			cx->set_source_rgb(1,1,1);
			cx->set_line_width(0.02 * d);
			cx->set_line_cap(Cairo::LINE_CAP_ROUND);
			cx->stroke();
		} :
		[] (Info info, Context cx)
		{
			auto d_ = info.get<double>("density");
			double d;
			if (d_) d = sqrt(*d_);
			d = std::max(10., d); d = std::min(30., d);
			d = (d - 10.) / 20.;
			cx->set_source_rgb(0,0,0);
			cx->set_line_width(0.02 * d);
			cx->set_line_cap(Cairo::LINE_CAP_ROUND);
			cx->stroke();
		});

	/* How to draw a galaxy */
	auto galaxy_material = (argv.get<bool>("reverse") ?
		[] (Info info, Context cx)
		{
			auto m_ = info.get<double>("magnitude");
			double m = 1.0; if (m_) m = *m_;
			double s = (12.5 - m)/2.5;
			cx->set_source_rgba(0.0,0.4,0.8,0.5);
			cx->rel_move_to(-0.01*s,0);
			cx->rel_line_to(0.01*s, -0.01*s);
			cx->rel_line_to(0.01*s, 0.01*s);
			cx->rel_line_to(-0.01*s, 0.01*s);
			cx->rel_line_to(-0.01*s, -0.01*s);
			//cx->set_line_width(0.01);
			//cx->rel_line_to(0,0);
			//cx->stroke();
			cx->close_path();
			cx->fill_preserve();
			cx->set_line_width(0.001);
			cx->set_source_rgba(1,1,1,0.8);
			cx->stroke();
		} :
		[] (Info info, Context cx)
		{
			cx->set_source_rgba(1,0,0,0.5);
			cx->rel_move_to(-0.01,0);
			cx->rel_line_to(0.01, -0.01);
			cx->rel_line_to(0.01, 0.01);
			cx->rel_line_to(-0.01, 0.01);
			cx->rel_line_to(-0.01, -0.01);
			//cx->set_line_width(0.01);
			//cx->rel_line_to(0,0);
			//cx->stroke();
			cx->close_path();
			cx->fill();
		} );

	for (double r = 0.0; r + dr < L/2; r += step)
	{
		//std::string fn_output = timed_filename(argv["id"], Misc::format("slice-", r, "-", r+dr), t, "pdf");
		std::string fn_output_png = timed_filename(argv["id"], Misc::format("slice-", r, "-", r+dr), t, "png");
		Sphere S1(Point(L/2,L/2,L/2), r), S2(Point(L/2,L/2,L/2), r + dr);
		std::cout << "filtering polygons ... radius " << r << "\n";
		Array<Polygon> filtered_polygons;
		Array<Vertex>  filtered_galaxies;
		Array<Segment> filtered_segments;
		Array<Vertex>  filtered_clusters;

		if (clusters)
		for (Vertex const &c : *clusters)
		{
			auto r_ = c.get_info<double>("r");
			double cr = 0.0; if (r_) cr = *r_;

			if (cr < r-3) continue;
			if (cr > r+dr+3) continue;

			filtered_clusters.push_back(c);
		}

		for (Polygon const &p : polygons)
		{
			auto d = p.get_info<double>("density");
			if ((not d) or (*d < wall_lim)) continue;
			auto A = S1.split_polygon(p);
			if (not A.second) continue;
			auto B = S2.split_polygon(*A.second);
			if (not B.first) continue;

			filtered_polygons.push_back(*B.first);
		}

		if (galaxies)
		for (Vertex const &v : *galaxies)
		{
			if ((not S1.is_below(v)) and S2.is_below(v))
				filtered_galaxies.push_back(v);
		}

		for (Segment const &s : segments)
		{
			auto d = s.get_info<double>("density");
			if ((not d) or (*d < fila_lim)) continue;
			auto A = S1.split_segment(s);
			if (not A.second) continue;
			auto B = S2.split_segment(*A.second);
			if (not B.first) continue;

			filtered_segments.push_back(*B.first);
		}
		std::cerr << "#segments: " << filtered_segments.size() << std::endl;

		Array<ptr<RenderObject>> scene;
		scene.push_back(ptr<RenderObject>(new PolygonObject(
			filtered_polygons, wall_material)));
		
		scene.push_back(ptr<RenderObject>(new SegmentObject(
			filtered_segments, filament_material)));

		if (galaxies)
		scene.push_back(ptr<RenderObject>(new VertexObject(
			filtered_galaxies, galaxy_material)));

		scene.push_back(ptr<RenderObject>(new VertexObject(
			filtered_clusters, cluster_label)));
		
		auto C = make_ptr<Map_projection_camera>(
			Point(L/2,L/2,L/2), Point(L, L/2, L/2), Vector(0, 0, 1),
			//Point(-1.0, 0.5, 1.0), centre, Vector(0, -1, 0),
				Map_projection(Aitoff_Hammer));

		auto R = Renderer::Image(1920, 1080);
		if (argv.get<bool>("reverse"))
			R->apply(prepare_context_rv(1920, 1080, r+dr/2));
		else
			R->apply(prepare_context(1920, 1080, r+dr/2));

		R->render(scene, C);
		R->write_to_png(fn_output_png);
		R->finish();
	}
}

#include "base/global.hh"
System::Global<Command> _COMMAND_COSMIC("cosmic", command_cosmic);

