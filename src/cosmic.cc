#include "base/common.hh"
#include "base/argv.hh"
#include "base/format.hh"
#include "ply/ply.hh"
#include "ply/read.hh"
#include "geometry/geometry.hh"
#include "render/render.hh"
#include "render/map_projection.hh"

#include <ctime>
#include <sstream>
#include <iomanip>

using namespace System;
using namespace Scam;

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

void command_cosmic(int argc_, char **argv_)
{
	Argv argv = read_arguments(argc_, argv_,
		Option({0, "h", "help", "false", 
			"print this help."}),
		Option({Option::VALUED | Option::CHECK, "i", "id", date_string(),
			"identifier for filenames."}),
		Option({Option::VALUED | Option::CHECK, "t", "time", "1.0",
			"growing mode parameter."}),
		Option({Option::VALUED | Option::CHECK, "r1", "r1", "30",
			"selection radius."}),
		Option({Option::VALUED | Option::CHECK, "r2", "r2", "0",
			"upper bound of selection, by default this is r1 + 10."} ));

	if (argv.get<bool>("help"))
	{
		std::cerr << "Scam -- 3D vector graphics for science.\n"
			"Copyright Johan Hidding, June 2014 - licence: GPL3.\n\n";
		argv.print(std::cerr);
		exit(0);
	}

	double r1 = argv.get<double>("r1");
	double r2 = (argv["r2"] == "0" ? r1 + 10 : argv.get<double>("r2"));	
	double t = argv.get<double>("time");

	std::string fn_output = timed_filename(argv["id"], Misc::format("slice-", r1, "-", r2), t, "png");
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

	Sphere S1(Point(50,50,50), r1), S2(Point(50,50,50), r2);
	std::cout << "filtering polygons ... \n";
	Array<Polygon> filtered_polygons;
	for (Polygon const &p : polygons)
	{
		auto d = p.get_info<double>("density");
		if ((not d) or (*d < 50)) continue;
		auto A = S1.split_polygon(p);
		if (not A.second) continue;
		auto B = S2.split_polygon(*A.second);
		if (not B.first) continue;

		filtered_polygons.push_back(*B.first);
	}

	Array<RenderObject> scene;
	scene.push_back(RenderObject(filtered_polygons, [] (Plane const &P, Context cx)
	{
		double s = P.normal() * Vector(0, 0, 1);
		cx->set_source_rgba(1,s*s,s*s,0.6);
		cx->fill_preserve();
		cx->set_source_rgb(0,0,0);
		cx->set_line_width(0.001);
		cx->stroke();
	}));
	
	auto C = make_ptr<Map_projection_camera>(
		Point(50,50,50), Point(100, 50, 50), Vector(0, 0, 1),
		//Point(-1.0, 0.5, 1.0), centre, Vector(0, -1, 0),
			Map_projection(Aitoff_Hammer));

	std::cout << "rendering ... \n";
	double N = 600;	
	auto R = Renderer::Image(2*N, N);
	R->apply([N] (Context cx)
	{
		double L = 3;
		cx->scale(N/L,N/L);
		cx->translate(L, L/2);
		cx->set_source_rgb(0.5,0.5,0.5);

		cx->save();
		cx->scale(2*sqrt(2), sqrt(2));
		cx->arc(0,0,1.0,0,6.283184);
		cx->set_line_width(0.01);
		cx->stroke();
		cx->restore();

		cx->set_line_join(Cairo::LINE_JOIN_ROUND);
	});

	R->render(scene, C);
	R->write_to_png(fn_output);
	R->finish();
}

#include "base/global.hh"
System::Global<Command> _COMMAND_COSMIC("cosmic", command_cosmic);

