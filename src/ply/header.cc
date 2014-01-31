#include "header.hh"
#include <memory>

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

	for (auto &element_name : header.names())
		out << header[element_name];

	out << "end_header" << std::endl;

	return out;
}

std::map<std::string, std::function<PLY::ptr<PLY::Property> (std::string const &)>> PLY::make_scalar_type = {
	{ "char", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<int8_t>(name)); } },
	{ "uchar",	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<uint8_t>(name)); } },
	{ "short",	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<int16_t>(name)); } },
	{ "ushort", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<uint16_t>(name)); } },
	{ "int", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<int32_t>(name)); } },
	{ "uint", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<uint32_t>(name)); } },
	{ "float", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<float>(name)); } },
	{ "double", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::Scalar<double>(name)); } } };

std::map<std::string, std::function<PLY::ptr<PLY::Property> (std::string const &)>> PLY::make_list_type = {
	{ "char", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<int8_t>(name)); } },
	{ "uchar",	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<uint8_t>(name)); } },
	{ "short",	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<int16_t>(name)); } },
	{ "ushort", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<uint16_t>(name)); } },
	{ "int", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<int32_t>(name)); } },
	{ "uint", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<uint32_t>(name)); } },
	{ "float", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<float>(name)); } },
	{ "double", 	[] (std::string const &name) { return PLY::ptr<PLY::Property>(new PLY::Header::List<double>(name)); } } };

