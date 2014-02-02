#include "base/common.hh"
#include "base/argv.hh"

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
	s << std::setfill('0') << setw(5) << static_cast<int>(round(t * 10000));
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
		s 	<< id << "." << stage << "." << std::setfill('0') << setw(5)
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
		auto indices = item.get_vector<unsigned>("vertex_indices");
		auto density = item.get<double>("density");
		auto V = Scam::map([vertices] (int i) { return vertices[i]; }, indices);
		result.push_back(Polygon(V));
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
		Option({Option::VALUED | Option::CHECK, "r1", "r1", "50",
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

	std::string fn_output = timed_filename(H["id"], Misc::Format("slice-", r1, "-", r2), t, "png");
	std::string fn_i_wall = Misc::Format(H["id"], ".", time_string(t), ".walls.ply");
	std::string fn_i_fila = Misc::Format(H["id"], ".", time_string(t), ".filam.ply");
}

