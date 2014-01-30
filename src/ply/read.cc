#include "ply.hh"
#include <fstream>
#include <stdexcept>
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

std::shared_ptr<PLY::PLY> PLY::read(std::string const &filename) throw (std::exception)
{
	auto ply = std::make_shared<PLY>();
	Format format;

	std::ifstream fi(filename);
	std::string line;

	std::getline(fi, line);
	if (line != "ply")
		throw std::exception("this is not a .ply file.");

	while (true)
	{
		std::getline(fi, line);
		auto S = split(line, ' ');
		if (not S)
		{
			if (line == "end_header")
				break;
			else
				throw std::exception("error in header of .ply file.");
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
				throw std::exception("error in 'format' clause in header of .ply file: " + line);

			if (S2->first == "ascii")
			{
				format = ASCII;
				continue;
			}

			if (S2->first == "binary_little_endian")
			{
				format = BINARY;
				continue;
			}

			if (S2->first = "binary_big_endian")
			{
				throw std::exception("sorry big-endian files are not yet supported");
			}

			throw std::exception("error, could not parse format-string: " + line);
		}

		if (S->first == "element")
		{
			auto S2 = split(S->second, ' ');
			if (not S2)
				throw std::exception("error in 'element' clause in header of .ply file: " + line);

			std::string name = S2->first;
			size_t N = from_string<size_t>(S2->second);
			ply->add_element_n(name, N);

			continue;
		}

		if (S->first == "property")
		{
			auto S2 = split_right(S->second, ' ');
			if (not S2)
				throw std::exception("error in 'property' clause in header of .ply file: " + line);

			std::string name = S2->second;
			auto S3 = split(S2->first, ' ');
			if (not S3)
			{
				if (make_scalar_type.count(S2->first) == 0)
					throw std::exception("unrecognized type in 'property' clause in header of .ply file: " + line);

				ply->add_properties(make_scalar_type[S2->first](name));
			}
			else
			{
				auto S4 = split(S3->second, ' ');

				if ((not S4) or (S3->first != "list"))
					throw std::exception("unrecognized type in 'property' clause in header of .ply file: " + line);

				if (S4->first != "uchar")
					std::cerr << "Warning: list type in .ply with index type other than 'uchar': " << line;
				
				if (make_list_type.count(S4->second) == 0)
					throw std::exception("unrecognized type in 'property' clause in header of .ply file: " + line);
					
				ply->add_properties(make_list_type[S4->second](name));
			}
		}
	} 

	return ply;
}

