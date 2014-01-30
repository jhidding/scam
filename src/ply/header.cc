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

std::map<std::string, std::function<ptr<Property> (std::string const &)>> make_scalar_type = {
	{ "char", 	[] (std::string const &name) { return make_ptr<scalar_type<int8_t>>(name); } },
	{ "uchar",	[] (std::string const &name) { return make_ptr<scalar_type<uint8_t>>(name); } },
	{ "short",	[] (std::string const &name) { return make_ptr<scalar_type<int16_t>>(name); } },
	{ "ushort", 	[] (std::string const &name) { return make_ptr<scalar_type<uint16_t>>(name); } },
	{ "int", 	[] (std::string const &name) { return make_ptr<scalar_type<int32_t>>(name); } },
	{ "uint", 	[] (std::string const &name) { return make_ptr<scalar_type<uint32_t>>(name); } },
	{ "float", 	[] (std::string const &name) { return make_ptr<scalar_type<float>>(name); } },
	{ "double", 	[] (std::string const &name) { return make_ptr<scalar_type<double>>(name); } } };

std::map<std::string, std::function<ptr<Property> (std::string const &)>> make_list_type = {
	{ "char", 	[] (std::string const &name) { return make_ptr<list_type<int8_t>>(name); } },
	{ "uchar",	[] (std::string const &name) { return make_ptr<list_type<uint8_t>>(name); } },
	{ "short",	[] (std::string const &name) { return make_ptr<list_type<int16_t>>(name); } },
	{ "ushort", 	[] (std::string const &name) { return make_ptr<list_type<uint16_t>>(name); } },
	{ "int", 	[] (std::string const &name) { return make_ptr<list_type<int32_t>>(name); } },
	{ "uint", 	[] (std::string const &name) { return make_ptr<list_type<uint32_t>>(name); } },
	{ "float", 	[] (std::string const &name) { return make_ptr<list_type<float>>(name); } },
	{ "double", 	[] (std::string const &name) { return make_ptr<list_type<double>>(name); } } };

