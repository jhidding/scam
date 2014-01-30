#include "header.hh"

std::ostream &PLY::operator<<(std::ostream &out, PLY::Property const &property)
{
	return out << "property " << property.type_expression() << " " << property.name() << std::endl;
}

std::ostream &PLY::operator<<(std::ostream &out, PLY::Header::Element const &element)
{
	out << "element " << element.name() << " " << element.count() << std::endl;

	for (auto &property : element.properties())
		out << *property;

	return out;
}

std::ostream &PLY::operator<<(std::ostream &out, PLY::Header const &header)
{
	out << "ply" << std::endl
		<< "format " << (header.format() == PLY::ASCII ? "ascii" : "binary_little_endian") 
		<< " 1.0" << std::endl;

	for (auto &comment : header.comments())
		out << "comment " << comment << std::endl;

	for (auto &element : header.elements())
		out << *element;

	out << "end_header" << std::endl;

	return out;
}

