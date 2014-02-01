#include "ply.hh"
#include <fstream>
#include <stdexcept>
#include <sstream>
#include "../base/common.hh"

using namespace PLY;
using namespace Scam;

Maybe<std::pair<std::string, std::string>> split(std::string const &line, char c)
{
	using pair = std::pair<std::string, std::string>;

	size_t pos = line.find(c);

	if (pos == std::string::npos)
		return Nothing;
	else
		return Just(pair(
			line.substr(0, pos), 
			line.substr(pos+1, line.length() - pos - 1)));
}

Maybe<std::pair<std::string, std::string>> split_right(std::string const &line, char c)
{
	using pair = std::pair<std::string, std::string>;

	size_t pos = line.find_last_of(c);

	if (pos == std::string::npos)
		return Nothing;
	else
		return Just(pair(
			line.substr(0, pos), 
			line.substr(pos+1, line.length() - pos - 1)));
}

template <typename T>
T from_string(std::string const &s)
{
	std::istringstream ss(s);
	T value;
	ss >> value;
	return value;
}

Format read_header(std::istream &fi, std::shared_ptr<PLY::PLY> ply) throw (Exception)
{
	std::string line;
	Format format;

	std::getline(fi, line);
	if (line != "ply")
		throw Exception("this is not a .ply file.");

	while (true)
	{
		std::getline(fi, line);
		auto S = split(line, ' ');
		if (not S)
		{
			if (line == "end_header")
				break;
			else
				throw Exception("error in header of .ply file.");
		}
	
		if (S->first == "comment")
		{
			ply->add_comment(S->second);
			continue;
		}

		if (S->first == "format")
		{
			auto S2 = split(S->second, ' ');
			if (not S2)
				throw Exception("error in 'format' clause in header of .ply file: " + line);

			if (S2->first == "ascii")
			{
				format = PLY::ASCII;
				continue;
			}

			if (S2->first == "binary_little_endian")
			{
				format = PLY::BINARY;
				continue;
			}

			if (S2->first == "binary_big_endian")
			{
				throw Exception("sorry big-endian files are not yet supported");
			}

			throw Exception("error, could not parse format-string: " + line);
		}

		if (S->first == "element")
		{
			auto S2 = split(S->second, ' ');
			if (not S2)
				throw Exception("error in 'element' clause in header of .ply file: " + line);

			std::string name = S2->first;
			size_t N = from_string<size_t>(S2->second);
			ply->add_element_n(name, N);

			continue;
		}

		if (S->first == "property")
		{
			auto S2 = split_right(S->second, ' ');
			if (not S2)
				throw Exception("error in 'property' clause in header of .ply file: " + line);

			std::string name = S2->second;
			auto S3 = split(S2->first, ' ');
			if (not S3)
			{
				if (make_scalar_type.count(S2->first) == 0)
					throw Exception("unrecognized type in 'property' clause in header of .ply file: " + line);

				ply->add_property(*make_scalar_type[S2->first](name));
			}
			else
			{
				auto S4 = split(S3->second, ' ');

				if ((not S4) or (S3->first != "list"))
					throw Exception("unrecognized type in 'property' clause in header of .ply file: " + line);

				if (S4->first != "uchar")
					std::cerr << "Warning: list type in .ply with index type other than 'uchar': " << line << std::endl;
				
				if (make_list_type.count(S4->second) == 0)
					throw Exception("unrecognized type in 'property' clause in header of .ply file: " + line);
					
				ply->add_property(*make_list_type[S4->second](name));
			}

			continue;
		}

		throw Exception("error in header of .ply file: " + line);
	} 

	return format;
}

#include "../geometry/geometry.hh"

template <typename T, typename U>
T cast(char const *d)
{
	return static_cast<T>(*reinterpret_cast<U const *>(d));
}

template <typename T>
T caster(std::string const &type, char const *d)
{
	if (type == "char")	return cast<T, int8_t>(d);
	if (type == "uchar") 	return cast<T, uint8_t>(d);
	if (type == "short") 	return cast<T, int16_t>(d);
	if (type == "ushort") 	return cast<T, uint16_t>(d);
	if (type == "int") 	return cast<T, int32_t>(d);
	if (type == "uint") 	return cast<T, uint32_t>(d);
	if (type == "float") 	return cast<T, float>(d);
	if (type == "double") 	return cast<T, double>(d);
};

struct Atom
{
	std::vector<char> u;
	std::string type;
	unsigned type_size;

	Atom() {}
	Atom(std::vector<char> u_, std::string const &type_, unsigned type_size_):
		u(u_), type(type_), type_size(type_size_) 
	{}
			
	template <typename T>
	T as() const { return caster<T>(type,u.data()); }

	size_t size() const { return u.size()  / type_size; }

	template <typename T>
	T idx(unsigned i) const { return caster<T>(type,u.data() + i*type_size); }

	template <typename T>
	std::vector<T> as_vector() const {
		std::vector<T> result;
		for (unsigned i = 0; i < size(); ++i)
			result.push_back(idx<T>(i));
		return result;
	}
};

class Item: public std::map<std::string, Atom>
{
	public:
		template <typename T> T get(std::string const &q) const
		{ return find(q)->second.as<T>(); }
		template <typename T> std::vector<T> get_vector(std::string const &q) const
		{ return find(q)->second.as_vector<T>(); }
};

using Block = Array<Item>;

Block read_element(std::istream &fi, PLY::Header::Element const &e, Format format)
{
	Block A;
	std::vector<PLY::ptr<PLY::Property>> const &P = e.properties();
	for (size_t i = 0; i < e.count(); ++i)
	{
		Item I;
		for (auto p = P.begin(); p != P.end(); ++p)
		{
			auto d = (*p)->datum();
			if (format == PLY::ASCII)
				d->read_ascii(fi);
			else
				d->read_binary(fi);

			I[(*p)->name()] = Atom(d->raw(), d->type_name(), d->type_size());
		}
		A.push_back(I);
	}
	return A;
}

Scam::Array<Scam::Vertex> read_vertices(std::istream &fi, Scam::ptr<PLY::PLY> ply, Format format)
{
	Scam::Array<Scam::Vertex> result;
	auto block = read_element(fi, (*ply)["vertex"], format);
	
	for (auto const &item : block)
	{
		double x = item.get<double>("x"),
		       y = item.get<double>("y"),
		       z = item.get<double>("z");

		result.push_back(Scam::Vertex(x, y, z));
	}

	return result;
}

Scam::Array<Scam::Polygon> read_polygons(std::istream &fi, Scam::ptr<PLY::PLY> ply, Scam::Array<Scam::Vertex> vertices, Format format)
{
	Scam::Array<Scam::Polygon> result;
	auto block = read_element(fi, (*ply)["face"], format);
	size_t i = 0;
	for (auto const &item : block)
	{
		auto indices = item.get_vector<unsigned>("vertex_indices");
		auto V = Scam::map([vertices] (int i) { return vertices[i]; }, indices);
		result.push_back(Polygon(V));
	}

	return result;
}

std::shared_ptr<PLY::PLY> PLY::read(std::string const &filename) throw (Exception)
{
	auto ply = std::make_shared<PLY>();

	std::ifstream fi(filename);
	Format format = read_header(fi, ply);

	return ply;
}

#ifdef UNITTEST
#include "../base/unittest.hh"
#include "../render/render.hh"

Test::Unit _test_PLY_read(
	"PLY01", "Reading a .ply file.",
	[] ()
{
	auto ply = make_ptr<PLY::PLY>();

	std::ifstream fi("test/stanford_bunny.ply");
	Format format = read_header(fi, ply);
	ply->print_header(std::cout, BINARY);
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

